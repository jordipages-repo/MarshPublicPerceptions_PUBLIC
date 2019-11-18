###########################
#     Modelling the       #
# National Survey dataset #
# without distance2coast  #
# after Rhoda and Nicky's #
#  comments on the draft  #
#      15-April-2019      #
###########################

library(car)
library(visreg)
library(lme4)
library(tidyverse)
library(tidylog)


# # # # # # # 
# Data import --------------------------------------------------------------------------------------
# # # # # # # 

source("Importing&TidyingNatSurvey.R")
source("mcheck_function.R")


####################
# Modelling --------------------------------------------------------------------------------------------
###################

##### RESPONSE VARIABLE: VISITED A MARSH BEFORE? ######
####################
# Data exploration #
####################

# We create a new data frame with just the variables we want to model
marsh_df <- marshq2 %>% 
  select(-c("Visited_marsh", 
            "Where", 
            "Rate_knowledge", 
            "Gender",
            "Age",
            "Employment",
            "Highest_Edu",
            "Member_environ_org",
            "Postcode",
            "MiniPostcode",
            "Member_environ_orgBINOMIAL"))

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

#############
# Modelling #
#############

# Binomial full initial model
glm1 <- glm(Visited_marshOK ~ fGender + fAge + fEmployment + fHighest_Edu + 
            fMember_environ_orgBINOMIAL + rural_urban2,
            family = "binomial", data = na.omit(marsh_df))

nobs(glm1) # N = 882

# Model selection
step(glm1)

glm2 <- glm(Visited_marshOK ~ fGender + fEmployment + fHighest_Edu + 
              fMember_environ_orgBINOMIAL, family = "binomial", data = na.omit(marsh_df))
Anova(glm2)
# Analysis of Deviance Table (Type II tests)
#                               LR Chisq Df Pr(>Chisq)    
# fGender                       18.796  1  1.454e-05 ***
# fEmployment                   18.149  5  0.0027647 ** 
# fHighest_Edu                  20.573  4  0.0003848 ***
# fMember_environ_orgBINOMIAL   17.291  1  3.207e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(glm2)
coef(glm2)
# (Intercept)                     fGender2                 fEmployment2                 fEmployment3 
# -0.91658819                  -0.64299795                   0.75740821                   0.52190415 
# fEmployment4                 fEmployment5                 fEmployment6                fHighest_Edu2 
# 0.03841886                  -0.38925411                   0.01684932                   0.60344067 
# fHighest_Edu3                fHighest_Edu4                fHighest_Edu5 fMember_environ_orgBINOMIAL1 
# 0.88169045                   0.80781208                   0.90141693                   1.39512884 

# Model validation
plot(glm2) # Very ugly, but it's very usual to find such kind of strange validation plots for binomial GLMs (Zuur et al 2009)
mcheck(glm2)

####
# Should we include random effects?
####

glm1.random <- glmer(Visited_marshOK ~ fGender + fAge + fEmployment + fHighest_Edu + 
              fMember_environ_orgBINOMIAL  + rural_urban2 + (1|Abbr_MiniPostcode), 
              family = "binomial", data = na.omit(marsh_df))

anova(glm1.random, glm1)
# Abbr_MiniPostcode is an important grouping (random) variable

summary(glm1.random)

# Model selection
drop1(glm1.random, test = "Chisq") # fAge can be dropped
glm2.random <- update(glm1.random, .~. -fAge)
drop1(glm2.random, test = "Chisq") # rural_urban2 can be dropped
glm3.random <- update(glm2.random, .~. -rural_urban2)
drop1(glm3.random, test = "Chisq") # Nothing else should be dropped

# Final model
glm.random.final <- glmer(Visited_marshOK ~ fGender + fEmployment + fHighest_Edu + fMember_environ_orgBINOMIAL + 
                            + (1|Abbr_MiniPostcode), family = "binomial", data = na.omit(marsh_df))
Anova(glm.random.final)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# Response: Visited_marshOK
#                             Chisq   Df Pr(>Chisq)    
# fGender                     19.319  1  1.106e-05 ***
# fEmployment                 17.431  5  0.0037504 ** 
# fHighest_Edu                20.603  4  0.0003795 ***
# fMember_environ_orgBINOMIAL 15.319  1  9.080e-05 ***

summary(glm.random.final)
coef(glm.random.final) # note how the random effect only affects the intercepts

# Model validation
plot(glm.random.final) # Very ugly, but it's very usual to find such kind of strange validation plots for binomial GLMs (Zuur et al 2009)
mcheck(glm.random.final)

# Plotting the significant effects
visreg(glm.random.final, scale = "linear") # To assess goodness of fit
visreg(glm.random.final, scale = "response") # Plots for easy interpretation

# Given that in visreg you can't see the confidence intervals with mixed models, and that neither the 
# sifnificance of the effects, nor the coefficients change much adding the random factor, we
# plot the glm2 (which does not have the random effects).
visreg(glm2, scale = "linear") # To assess goodness of fit

# pdf(file = "Figs&Tables/P(visitinMarsh)OK.pdf")
op <- par(mfrow = c(2,2), oma = c(5,4,2,0) + 0.1, mar = c(4,4,4,1) + 0.1, pch = 20)
  visreg(glm2, xvar = "fGender", scale = "response", ylim = c(0,1), ylab = "Probability of visiting a marsh", xlab = "Gender", rug = F)
  visreg(glm2, xvar = "fEmployment", scale = "response", ylim = c(0,1), ylab = "Probability of visiting a marsh", xlab = "Employment", rug = F)
  visreg(glm2, xvar = "fHighest_Edu", scale = "response", ylim = c(0,1), ylab = "Probability of visiting a marsh", xlab = "Education", rug = F)
  visreg(glm2, xvar = "fMember_environ_orgBINOMIAL", scale = "response", ylim = c(0,1), ylab = "Probability of visiting a marsh", rug = F, xlab = "Member environ. NGO")
par(op)
# dev.off()




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
             fMember_environ_orgBINOMIAL + rural_urban2, data = na.omit(marsh_df))
nobs(om1)
summary(om1)

# Model selection
formula_om <- step(om1)$formula
om.final <- clm(formula_om, data = marsh_df)
summary <- summary(om.final)
print(summary)
# Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
# fVisited_marshOK1              2.5669     0.1595  16.093  < 2e-16 ***
# fMember_environ_orgBINOMIAL1   0.8577     0.3012   2.847  0.00441 ** 
# rural_urban2Urban             -0.2458     0.1736  -1.416  0.15691   
print(summary$cond.H) # 71.77
print(summary$cond.H>10000) # FALSE, which is great. Means the model is ok.
# The condition number of the Hessian is a measure of how identifiable the model is; 
# large values, say larger than 1e4 indicate that the model may be ill defined (manual of the R package ordinal)
Anova_Test <- Anova(om.final)
print(Anova_Test)
# Analysis of Deviance Table (Type II tests)
# Response: fRate_knowledge
#                             LR Chisq Df Pr(>Chisq)    
# fVisited_marshOK              320.43  1  < 2.2e-16 ***
# fMember_environ_orgBINOMIAL     7.88  1   0.004995 ** 
# rural_urban2                   29.50  1    5.6e-08 *** 
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
library(RVAideMemoire)

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
      "fGender + fAge + fEmployment + fHighest_Edu + fMember_environ_orgBINOMIAL + rural_urban2",
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

write(out, file="~/Desktop/prova3.txt", sep="")


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

