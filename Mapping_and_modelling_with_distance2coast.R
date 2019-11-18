###########################
#     Screening the       #
# National Survey dataset #
#    10-October-2018      #
###########################

library(car)
library(visreg)
library(lme4)
library(tidyverse)
library(tidylog)
library(gridExtra)


# # # # # # # 
# Data import --------------------------------------------------------------------------------------
# # # # # # # 

source("Importing&TidyingNatSurvey.R")
source("mcheck_function.R")


####################################
## Mapping respondents with ggmap ##
####################################

# From https://lucidmanager.org/geocoding-with-ggmap/
library(ggmap)
# Now, to use Google API you have to be registered (include a credit card) and get an API key. See below.
api <- readLines("Figs&Tables/google.api") # Text file with the API key
register_google(key = api)
ggmap_credentials()
# To check we don't exceed the day_limit that Google imposes (2500)
geocodeQueryCheck()

#### Successful trial with https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
# We import a data set with the centre of each postcode in lat lon (data set from https://www.freemaptools.com/download-uk-postcode-lat-lng.htm)
# Also useful https://stackoverflow.com/questions/29036809/plotting-uk-postcodes-on-a-map-in-r
Df_UK <- read.csv(file = "Data/postcode-outcodes.csv")
Df_UK <- Df_UK[,-1]
# We calculate the frequency of respondents from each postcode
freq <- marshq2 %>% 
  filter(!is.na(rural_urban2)) %>% 
  group_by(Postcode, rural_urban2) %>% 
  summarise(n = n())

  
# Checking what will not be joined. People that entered an incorrect postcode.
anti_join(freq, Df_UK, by = c("Postcode" = "postcode"))

# Join
freq2 <- freq %>% 
  left_join(Df_UK, by = c("Postcode" = "postcode")) %>% 
  filter(latitude < 54 & longitude < -2)  # We get rid of those postcodes that are not from Wales

# We build a map of Wales
Map <- ggmap(get_googlemap(center = c(longitude = -3.8, latitude = 52.42),
                           zoom = 8,
                           maptype = "roadmap",
                           color = "bw"),
             extent = "normal")

# We plot the frequency of respondents as 'bubbles'
MapBubbles <- Map +
  geom_point(data = freq2, mapping = aes(x=longitude, y=latitude, size = n, col=freq2$rural_urban2), alpha=0.3) +
  # scale_size(range = range((freq2$n))/3) +
  scale_size(range = c(1.5, 11)) +
  scale_color_manual(values = c("#019e23", "#db4c4c")) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(size ="Respondents\nby postcode", col = "Postcode type") +
  theme_bw()
# ggsave("Figs&Tables/Map_points_nationalSurveyNew4.pdf")


# Plotting saltmarsh areas on the same map ------------------------------------------------------------------------------------------------------
# https://www.r-bloggers.com/shapefile-polygons-plotted-on-google-maps-using-ggmap-in-r-throw-some-throw-some-stats-on-that-mappart-2/
library(rgdal)
smextents <- readOGR(dsn="WelshMarshes", layer="WelshMarshesOK")
smextents <- spTransform(smextents, CRS("+proj=longlat +datum=WGS84"))
smextents <- fortify(smextents)
smmap <- MapBubbles + geom_polygon(aes(x=long, y=lat, group=group), fill='black', size=.2,color='black', data=smextents, alpha=0.3) + theme_bw()
smmap
ggsave("Figs&Tables/Map_Bubles+saltmarshes1.jpeg")

# Joining panels
t2 <- grid.arrange(MapBubbles, smmap, widths = c(1,1), nrow = 1)
ggsave("Figs&Tables/Fig.BubbleMap+Saltmarshes.pdf", plot = t2)

# We plot the kernel density estimation of responses
# pdf(file = "Documents/FEINA/POSTDOCTORAT/RESILCOAST/ARTICLES/Emma_NationalSurvey/RFiles/Figures/Map_kernel_nationalSurvey2.pdf")
# Map + 
#   geom_density2d(data = freq, aes(x = Lon, y = Lat), size = 0.3, col = "darkgrey") +
#   stat_density2d(data = freq, 
#                  aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, 
#                  bins = 16, geom = "polygon") + scale_fill_gradient(low = "grey", high = "black") + 
#   scale_alpha(range = c(0, 0.6), guide = FALSE)
# dev.off()


#####
### Less successful old mapping trials 

# from https://www.sharpsightlabs.com/blog/basic-maps-ggmap/
# map.uk <- get_map(location = c(longitude = -3.8, latitude = 52.42), zoom = 8, maptype = 'terrain')
# ggmap(map.uk)

####### From Adel Heenan's book
# library(RgoogleMaps)
# LivMap <- GetMap(center = c(lon = -3.8, lat = 52.42), zoom = 8, API_console_key = "AIzaSyAw_8cxu49YGFGWuLlzexw0H2ezrFndDk8")
# 
# backdrop <- function(gmt){
#   # Set x and y plot limits
#   limx <- c(-320, 320)
#   limy <- c(-320, 320)
#   # Make the map fill the entire window
#   par(mar = c(0,0,0,0))
#   # Create the empty plot
#   plot(limx, limy, type = "n", asp = 1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
#   # Draw a box around it
#   box()
#   # Fill it with the raster map
#   rasterImage(gmt$myTile, -320, -320, 320, 320)
# }
# 
# backdrop(LivMap)
# points(x = c(310,0))

# Another trial with ggmap from https://chrisbeeley.net/?p=239
#####


############################################################################################################
############################################################################################################

####################################################
## Getting the minimum distance from the postcode ## 
##        of each respondent to the coast         ##
####################################################

# Info. obtained from https://stackoverflow.com/questions/21295302/calculating-minimum-distance-between-a-point-and-the-coast
library(rgeos)
library(maptools)
library(rgdal)

# Data set with the postcode we need. But we have to get rid of NA's (thus complete.cases)
freq2 <- freq[complete.cases(freq),]

# We import a shapefile of the UK coast
coast <- readOGR(dsn="GB_polygon_outline", layer="greatbritain")
class(coast)
# coast is a polygon, and since all points are contained in the polygon, when we calculate gDistance
# it gives us a result of 0 m (because the point is in contact with the polygon)
# Thus, we transform the polygon into a lines object.
coastLines <- as(coast, 'SpatialLines') 

# Projection data taken from http://spatialreference.org/ref/epsg/27700/ and then clicking proj4
# epsg.27700 <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"

# Currently, my points are projected in WGS84, since they are in degrees
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mypoints <- SpatialPoints(coords = as.data.frame(cbind(freq2$Lon, freq2$Lat)), proj4string = CRS(wgs.84))

# In order to compare mypoints to coastLine and get a distance in meters I must re-project mypoints into
# a planar projection, and it has to be the same as coastLines (thus proj4string(coastLines))
mypoints.proj <- spTransform(mypoints, CRS(proj4string(coastLines)))

freq2$gDistances <- sapply(1:length(freq2$MiniPostcode), function(i){gDistance(mypoints.proj[i], coastLines)})
freq2$gDistancesKM <- freq2$gDistances/1000

# plot(coast, col = "grey")
# plot(mypoints.proj[which((freq2$gDistancesKM)<10)], add = T, pch = 16, cex = 0.7)




####################
# Modelling --------------------------------------------------------------------------------------------
###################

# Adding the distance to coast variable to the main data set.
marshq$Distance2coast <- freq2$gDistancesKM[match(marshq$MiniPostcode, freq2$MiniPostcode)]

##### RESPONSE VARIABLE: VISITED A MARSH BEFORE? ######
####################
# Data exploration #
####################

# We create a new data frame with just the variables we want to model
marsh_df <- marshq[, -c(2, 3, 4, 33, 34, 35, 36, 37, 39, 40, 42)]

# We get rid of NA's
# marsh_df <- marsh_df[complete.cases(marsh_df),]

# Outliers in Y? In this case, more than outliers, it's a matter of checking there aren't errors in the dataset. 
boxplot(marsh_df$Visited_marshOK, ylab = "Visited marsh?")
dotchart(marsh_df$Visited_marshOK, ylab = "Order of the data", xlab = "Visited marsh?")
# No errors

# Outliers in X?
boxplot(as.integer(marsh_df$fGender), ylab = "fGender")
dotchart(as.integer(marsh_df$fGender), ylab = "Order of the data", xlab = "fGender")
boxplot(as.integer(marsh_df$fAge), ylab = "fAge")
dotchart(as.integer(marsh_df$fAge), ylab = "Order of the data", xlab = "fAge")
boxplot(as.integer(marsh_df$fEmployment), ylab = "fEmployment")
dotchart(as.integer(marsh_df$fEmployment), ylab = "Order of the data", xlab = "fEmployment")
boxplot(as.integer(marsh_df$fHighest_Edu), ylab = "fHighest_Edu")
dotchart(as.integer(marsh_df$fHighest_Edu), ylab = "Order of the data", xlab = "fHighest_Edu")
boxplot(as.integer(marsh_df$fMember_environ_orgBINOMIAL), ylab = "fMember_environ_orgBINOMIAL")
dotchart(as.integer(marsh_df$fMember_environ_orgBINOMIAL), ylab = "Order of the data", xlab = "fMember_environ_orgBINOMIAL")
boxplot(marsh_df$cIncome, ylab = "cIncome")
dotchart(marsh_df$cIncome, ylab = "Order of the data", xlab = "cIncome")
boxplot(marsh_df$Distance2coast, ylab = "Distance2coast")
dotchart(marsh_df$Distance2coast, ylab = "Order of the data", xlab = "Distance2coast")

#############
# Modelling #
#############

# Binomial full initial model
glm1 <- glm(Visited_marshOK ~ fGender + fAge + fEmployment + fHighest_Edu + 
            fMember_environ_orgBINOMIAL + cIncome + Distance2coast,
            family = "binomial", data = na.omit(marsh_df))

nobs(glm1) # N = 877

# Model selection
step(glm1)

glm2 <- glm(Visited_marshOK ~ fGender + fEmployment + fHighest_Edu + fMember_environ_orgBINOMIAL + cIncome
            + Distance2coast, family = "binomial", data = na.omit(marsh_df))
Anova(glm2)
# Analysis of Deviance Table (Type II tests)
#                               LR Chisq Df Pr(>Chisq)    
# fGender                      14.9839  1  0.0001084 ***
# fEmployment                  20.0854  5  0.0012044 ** 
# fHighest_Edu                 13.9500  4  0.0074564 ** 
# fMember_environ_orgBINOMIAL  15.2534  1  9.401e-05 ***
# cIncome                      17.9837  1  2.228e-05 ***
# Distance2coast                3.5294  1  0.0602897 . 

summary(glm2)
coef(glm2)
# (Intercept)                     fGender2                 fEmployment2                 fEmployment3 
# -1.36300923                  -0.58427671                   0.95490743                   0.63947387 
# fEmployment4                 fEmployment5                 fEmployment6                fHighest_Edu2 
# 0.23508055                  -0.04813885                   0.24812206                   0.49072848 
# fHighest_Edu3                fHighest_Edu4                fHighest_Edu5 fMember_environ_orgBINOMIAL1 
# 0.80115837                   0.64499986                   0.71640327                   1.30408566 
# cIncome               Distance2coast 
# 0.01743081                  -0.01290662 

# Model validation
plot(glm2) # Very ugly, but it's very usual to find such kind of strange validation plots for binomial GLMs (Zuur et al 2009)
mcheck(glm2)

####
# Should we include random effects?
####

glm1.random <- glmer(Visited_marshOK ~ fGender + fAge + fEmployment + fHighest_Edu + 
              fMember_environ_orgBINOMIAL + cIncome + Distance2coast + (1|Abbr_MiniPostcode), 
              family = "binomial", data = na.omit(marsh_df))

anova(glm1.random, glm1)
# Abbr_MiniPostcode is an important grouping (random) variable

summary(glm1.random)

# Model selection
drop1(glm1.random, test = "Chisq") # fAge can be dropped
glm2.random <- update(glm1.random, .~. -fAge)
drop1(glm2.random, test = "Chisq") # Distance2coast can be dropped
glm3.random <- update(glm2.random, .~. -Distance2coast)
drop1(glm3.random, test = "Chisq") # Nothing else should be dropped

# Final model
glm.random.final <- glmer(Visited_marshOK ~ fGender + fEmployment + fHighest_Edu + fMember_environ_orgBINOMIAL +
                            cIncome + (1|Abbr_MiniPostcode), family = "binomial", data = na.omit(marsh_df))
Anova(glm.random.final)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# Response: Visited_marshOK
#                             Chisq   Df Pr(>Chisq)    
# fGender                     15.662  1  7.574e-05 ***
# fEmployment                 19.542  5  0.0015227 ** 
# fHighest_Edu                15.037  4  0.0046243 ** 
# fMember_environ_orgBINOMIAL 13.654  1  0.0002198 ***
# cIncome                     17.628  1  2.686e-05 ***

summary(glm.random.final)
coef(glm.random.final) # note how the random effect only affects the intercepts

# Model validation
plot(glm.random.final) # Very ugly, but it's very usual to find such kind of strange validation plots for binomial GLMs (Zuur et al 2009)
mcheck(glm.random.final)

# Plotting the significant effects
visreg(glm.random.final, scale = "linear") # To assess goodness of fit
visreg(glm.random.final, scale = "response") # Plots for easy interpretation

# Given that in visreg you can't see the confidence intervals with mixed models, 
# I plot the glm2 (which is extremely similar in terms of coeff).
visreg(glm2, scale = "linear") # To assess goodness of fit

# pdf(file = "~/Desktop/NationalSurvey1.pdf")
op <- par(mfrow = c(2,3), oma = c(5,4,2,0) + 0.1, mar = c(4,4,4,1) + 0.1, pch = 20)
  visreg(glm2, xvar = "fGender", scale = "response", ylim = c(0,1), ylab = "Probability of visiting a marsh", xlab = "Gender", rug = F)
  visreg(glm2, xvar = "fEmployment", scale = "response", ylim = c(0,1), ylab = "Probability of visiting a marsh", xlab = "Employment", rug = F)
  visreg(glm2, xvar = "fHighest_Edu", scale = "response", ylim = c(0,1), ylab = "Probability of visiting a marsh", xlab = "Education", rug = F)
  visreg(glm2, xvar = "fMember_environ_orgBINOMIAL", scale = "response", ylim = c(0,1), ylab = "Probability of visiting a marsh", rug = F, xlab = "Member environ. NGO")
  visreg(glm2, xvar = "cIncome", scale = "response", ylim = c(0,1), ylab = "Probability of visiting a marsh", xlab = "Income", rug = F)
  visreg(glm2, xvar = "Distance2coast", scale = "response", ylim = c(0,1), ylab = "Probability of visiting a marsh", xlab = "Distance to the coast (km)", rug = F)
par(op)
# dev.off()


# Let's check colliniearity between predictor variables
source("Documents/FEINA/POSTDOCTORAT/RESILCOAST/ARTICLES/Task 2.2. Livestock and erosion/R Files/HighstatLib.r")
# "HighstatLib.R" is a script for the corvif function (Zuur et al. 2009). It is available online at http://www.highstat.com/book2.htm
# NOT WORKING NOW, because of non-integer columns in marsh_df... just delete those that are factors.
marsh_df_collinearity <- marsh_df[,-c(1,2,9)]  
marsh_df_collinearity$fGender <- as.numeric(marsh_df_collinearity$fGender)
marsh_df_collinearity$fAge <- as.numeric(marsh_df_collinearity$fAge)
marsh_df_collinearity$fEmployment <- as.numeric(marsh_df_collinearity$fEmployment)
marsh_df_collinearity$fHighest_Edu <- as.numeric(marsh_df_collinearity$fHighest_Edu)
marsh_df_collinearity$fMember_environ_orgBINOMIAL <- as.numeric(marsh_df_collinearity$fMember_environ_orgBINOMIAL)

corvif(marsh_df_collinearity)
# all inflation factors are very low ~1.1-1.2: thus, there isn't indication of collinearity among variables used in the models (Zuur et al. 2007, 2009).
# And that's further explicited in the correlation matrix of the variables. The highest correlation is -0.35 (between Employment and income) 




##### RESPONSE VARIABLE: ORDINAL!!!!!!
# RATE OF KNOWLEDGE RE. MARSHES ######
#############
# Modelling #
#############

# This response variable has a peculiarity!
# It's ORDINAL! We can't just assume it's a continuous variable, because it's bounded between 1-4, and there's an order there.
# It must be analysed using ORDINAL LOGISTICAL REGRESSION
# Information to do this, taken from:
#         https://en.wikipedia.org/wiki/Ordered_logit
#         https://stats.stackexchange.com/questions/93454/how-do-i-run-ordinal-logistic-regression-analysis-in-r-with-both-numerical-cat
#         https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#         https://rstudio-pubs-static.s3.amazonaws.com/220675_90da5cd7a01c4a57b9f22ff2b89bc915.html
#         https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/
#         http://rcompanion.org/handbook/G_11.html


library(ordinal)
library(RVAideMemoire)
# Full model
om1 <- clm(fRate_knowledge ~ fVisited_marshOK + fGender + fAge + fEmployment + fHighest_Edu + 
             fMember_environ_orgBINOMIAL + Income + Distance2coast, data = na.omit(marsh_df))
nobs(om1)
summary(om1)

# Model selection
formula_om <- step(om1)$formula
om.final <- clm(formula_om, data = marsh_df)
summary <- summary(om.final)
print(summary)
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# fVisited_marshOK1              2.5757     0.1579   16.31  < 2e-16 ***
# fMember_environ_orgBINOMIAL1   0.9045     0.2966    3.05  0.00229 ** 
print(summary$cond.H) # 35.54
print(summary$cond.H>10000) # FALSE, which is great. Means the model is ok.
# The condition number of the Hessian is a measure of how identifiable the model is; 
# large values, say larger than 1e4 indicate that the model may be ill defined (manual of the R package ordinal)
# Hessian = 7.2, so ok
Anova_Test <- Anova(om.final)
print(Anova_Test)
# Analysis of Deviance Table (Type II tests)
# Response: fRate_knowledge
#                             LR Chisq Df Pr(>Chisq)    
# fVisited_marshOK              330.15  1  < 2.2e-16 ***
# fMember_environ_orgBINOMIAL     9.04  1   0.002642 ** 
confint(om.final, type = "Wald")
barplot(table(marsh_df$fRate_knowledge))

# test partial proportional odds assumption for temp and contact:
nominal_test(om.final) # fVisited_marshOK appears to violate this test ->  evidence of non-proportional odds.
# test if there are signs of scale effects 
scale_test(om.final) # No scale eeffects.

# This means that most people have no knowledge, or a basic one. 
# Confusion matrix
# library(caret)
# confusionMatrix(reference = marsh_df$fRate_knowledge, unlist(predict(om.final, newdata = marsh_df, type = "class")))
# According to https://rpubs.com/malshe/224660 accuracy in the confusion matrix is the model accuracy
# For our model accuracy is 73%, this is pretty good.


# Same with library(MASS) see below
######
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)

polr1 <- polr(fRate_knowledge ~ fVisited_marshOK + fHighest_Edu + fMember_environ_orgBINOMIAL, data = marsh_df, Hess=TRUE)
summary(polr1)
ctable <- coef(summary(polr1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(polr1)
exp(coef(polr1))
exp(cbind(OR = coef(polr1), ci))
pr <- profile(polr1)
plot(pr)
pairs(pr)

# Very similar results to library(ordinal)

# End modelling










######################################################################################
# RATING BIT OF THE QUESTIONNAIRE.                                                   #
# IT will be TACKLED WITH:                                                           #
#                     1- ONE-SAMPLE T-TESTS, TO TEST WHETHER RESPONSES               #
#                        DIFFER FROM THE MID-POINT (NEITHER AGREE NOR DISAGREE)      #
#                               AND                                                  #
#                     2- ORDINAL REGRESSION, TO CHECK IF ANY OF THE OTHER            #
#                         VARIABLES HAD A MODERATING EFFECT ON THE QUESTION          #
######################################################################################

####################################################################
# 1. USING T-TESTS TO TEST DEPARTURE FROM THE (NEUTRAL) MID-POINT #
####################################################################
# Sources of info: 
# http://www.talkstats.com/threads/are-scores-significantly-different-to-a-neutral-response-please-help.5772/
# http://www.sthda.com/english/wiki/one-sample-t-test-in-r

# marsh_df_auto <- na.omit(marsh_df[, -c(30, 35, 38)]) # we are deleting Income, cIncome and Abbr_MiniPostcode due to the presence of NAs (to increase sample size)
marsh_df_auto <- na.omit(marsh_df)
names(marsh_df_auto)[2:29]

# FOR AGREE-DISAGREE QUESTIONS

# First, we must delete "UNSURE" data from all columns (code 6)
# pdf(file = "~/Documents/FEINA/POSTDOCTORAT/RESILCOAST/ARTICLES/Emma_NationalSurvey/RFiles/Figures/t.studentsAgree.pdf")
  plot(2:15, xaxt = "n", xlab='', pch = "", yaxt = "n", ylab = '', ylim = c(1,5), xlim = c(1,15))
  axis(1, at=2:15, labels=names(marsh_df_auto[2:15]), las = 2)
  axis(2, at = 1:5, labels = c("Strongly disagree", "Disagree", "Neither agree or disagree", "Agree", "Strongly agree"), las = 2)
  for(i in 2:15) {
    data.t.test <- as.numeric(marsh_df_auto[-(which(marsh_df_auto[,i] == 6)), i])
    print(names(marsh_df_auto)[i])
    # boxplot(data.t.test)
    nobs <- length(data.t.test)
    print(nobs)
    tstudent <- t.test(data.t.test, mu = 3)
    tstudent
    points(x = i, tstudent$estimate, pch = 19, ylim = c(1, 5))
    arrows(x0 = i, y0 = tstudent$conf.int[1], x1 = i, y1 = tstudent$conf.int[2], length = 0.05, angle = 90, code = 3)
  }
    abline(h = 3, lty = 2)
# dev.off()

# pdf(file = "~/Documents/FEINA/POSTDOCTORAT/RESILCOAST/ARTICLES/Emma_NationalSurvey/RFiles/Figures/unsureAgree.pdf")
  unsure <- sapply(X = 2:15, function(i){
    data.t.test <- as.numeric(marsh_df_auto[(which(marsh_df_auto[,i] == 6)), i])
    nobs <- length(data.t.test)})
  
  plot(100*(unsure/length(marsh_df_auto$ID)), type = "l", xaxt = "n", xlab='', ylab = '% unsure', xlim = c(1,15), ylim = c(0,50))
  axis(1, at=2:15, labels=names(marsh_df_auto[2:15]), las = 2)
# dev.off()

# FOR BENEFIT-NO BENEFIT QUESTIONS
# First, we must delete "UNSURE" data from all columns (code 6)
# pdf(file = "~/Documents/FEINA/POSTDOCTORAT/RESILCOAST/ARTICLES/Emma_NationalSurvey/RFiles/Figures/t.studentsBENEFIT.pdf")
plot(16:29, xaxt = "n", xlab='', pch = "", yaxt = "n", ylab = '', ylim = c(1,5), xlim = c(1,15))
axis(1, at=1:14, labels=names(marsh_df_auto[16:29]), las = 2)
axis(2, at = 1:5, labels = c("No benefit", "Slightly beneficial", "Somewhat beneficial", "Moderately beneficial", 
                             "Very beneficial"), las = 2)
for(i in 16:29) {
  data.t.test <- as.numeric(marsh_df_auto[-(which(marsh_df_auto[,i] == 6)), i])
  print(names(marsh_df_auto)[i])
  # boxplot(data.t.test)
  nobs <- length(data.t.test)
  print(nobs)
  tstudent <- t.test(data.t.test, mu = 3)
  tstudent
  points(x = i-15, tstudent$estimate, pch = 19, ylim = c(1, 5))
  arrows(x0 = i-15, y0 = tstudent$conf.int[1], x1 = i-15, y1 = tstudent$conf.int[2], length = 0.05, angle = 90, code = 3)
}
abline(h = 3, lty = 2)
# dev.off()

# pdf(file = "~/Documents/FEINA/POSTDOCTORAT/RESILCOAST/ARTICLES/Emma_NationalSurvey/RFiles/Figures/unsureBENEFITS.pdf")
unsure <- sapply(X = 16:29, function(i){
  data.t.test <- as.numeric(marsh_df_auto[(which(marsh_df_auto[,i] == 6)), i])
  nobs <- length(data.t.test)})

plot(100*(unsure/length(marsh_df_auto$ID)), type = "l", xaxt = "n", xlab='', ylab = '% unsure', xlim = c(1,15), ylim = c(0,50))
axis(1, at=2:15, labels=names(marsh_df_auto[16:29]), las = 2)
# dev.off()

  
##### RESPONSE VARIABLE: Salt marshes are protected by existing legislation AND ASSOCIATED QUESTIONS ######
#############
# Modelling #
#############

# This variable is ORDINAL! ORDINAL LOGISTICAL REGRESSION
library(ordinal)

marsh_df_auto <- na.omit(marsh_df)
names(marsh_df_auto)[2:29]

# We delete all rows that contain unsure (6) codes. Because it breaks the correct order for the ordinal analysis.
data.new <- marsh_df_auto
for(i in 2:29) {
  data.new[which(data.new[,i] == 6), i] <- NA
}
data.new.clean <- na.omit(data.new)
marsh_df_auto <- data.new.clean
  
# Initialising the output lists
final_formulas <- NULL
# final_summaries <- NULL
final_hessians <- NULL
# final_Anovas <- NULL
# final_confints <- NULL

# We won't include fRateknowledge here, because it's collinear with fVisited_marshOK
# We won't include Income as it has too many levels and leads to unidentifiable models.
# We won't include fVisited_marshOK because it makes all the Nominal tests to be significant, due to how biased this variable is.

out <- capture.output(for(i in 2:29){
  # We build the formula for the full model
  formula_auto <-
    paste(
      names(marsh_df_auto)[i],
      " ~ ",
      "fGender + fAge + fEmployment + fHighest_Edu + fMember_environ_orgBINOMIAL + Distance2coast",
      sep = ""
    )
  # Full model
  om1 <- clm(formula_auto, data = marsh_df_auto)
  # Automatic backwards and forwards selection
  formula_om <- step(om1, trace = 0)$formula
  print(paste(cat("\n\n\n\n\n\nFormula of the ordinal regression: ", "\n", as.character(formula_om)[2], " ~ ", as.character(formula_om)[3], sep = "")))
  # Final best-selected model
  om.final <- clm(formula_om, data = marsh_df_auto)
  # Summary and coefficients
  summary_om <- summary(om.final)
  print(summary_om)
  # Hessian number
  hessian_om <- summary_om$cond.H 
  print(paste(cat("\nHessian = ", hessian_om, "\n", sep = "")))
  # Significance of the main effects
  if(length(names(om.final$model))>1){Anova_om <- Anova(om.final)}
  print(Anova_om)
  # Condifence intervals
  confint_om <- confint(om.final, type = "Wald")
  print(confint_om)
  # Testing asssumptions:
  # Tests partial proportional odds assumption:
  print(nominal_test(om.final) )
  # Tests if there are signs of scale effects: 
  print(scale_test(om.final))
  # Outputs
  final_formulas <- c(final_formulas, formula_om)
  # final_summaries <- c(final_summaries, summary_om)
  final_hessians <- c(final_hessians, hessian_om)
  # final_Anovas <- c(final_Anovas, Anova_om)
  # final_confints <- c(final_confints, confint_om)
})

write(out, file="~/Desktop/prova.txt", sep="")


final_df <- data.frame(matrix(unlist(final_formulas)))
names(final_df) <- "formula"
final_df$Hessians <- final_hessians

final_df
############################################################################################################
############################################################################################################

library(tidyverse)
library(tidylog)

marsh_df %>%
  as.tibble() %>% 
  select(c(starts_with("A_"), starts_with("I_"))) %>% 
  filter_all(all_vars(. != 6)) # es filtra sempre el que es true. Aqui, demanem rows que compleixin que en totes les variables no hi ha cap 6

