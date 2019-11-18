###########################
#     Screening the       #
# National Survey dataset #
#    10-October-2018      #
#  big update 15/04/2019  #
###########################


# # # # # # 
# Libraries ----------------------------------------------------------------------------------------------------
# # # # # #

library(tidyverse)
library(tidylog)


# # # # # # # # # 
# Importing data ----------------------------------------------------------------------------------------------------
# # # # # # # # # 

marshq <- read.csv(file = "Data/PPR Nationwide Data_Sept18_edited_JFP.csv")
str(marshq)


# # # # # # # # # # # # 
# Tidying the data set ----------------------------------------------------------------------------------------------------
# # # # # # # # # # # #

# Coding this variable to make it binomial.
marshq$Visited_marshOK <- NA
marshq$Visited_marshOK[which(marshq$Visited_marsh == 2)] <- 0
marshq$Visited_marshOK[which(marshq$Visited_marsh == 1)] <- 1

# Coding this predictor to make it a factor, and presence-absence-like.
marshq$Member_environ_orgBINOMIAL <- NA
marshq$Member_environ_orgBINOMIAL[which(is.na(marshq$Member_environ_org)==F)] <- 1
marshq$Member_environ_orgBINOMIAL[which(is.na(marshq$Member_environ_org)==T)] <- 0
marshq$fMember_environ_orgBINOMIAL <- as.factor(marshq$Member_environ_orgBINOMIAL)

# Transform Gender into a factor
marshq$fGender <- as.factor(marshq$Gender)

# Transform Employment into a factor
marshq$fEmployment <- as.factor(marshq$Employment)

# Label don't know/declined to answer into NA's for the income column
marshq$Income[which(marshq$Income == "Don’t know/declined to answer" | marshq$Income == "Ddim yn gwybod/ddim eisiau ateb" )] <- NA

# Transform income into a continuous variable
marshq$cIncome <- NA
marshq$cIncome[which(marshq$Income == "Less than £5,000" )] <- 2.500
marshq$cIncome[which(marshq$Income == "£5,001 i £15,000" | marshq$Income == "£5,001 to £15,000")] <- 10.000
marshq$cIncome[which(marshq$Income == "£15,001 i £25,000" | marshq$Income == "£15,001 to £25,000")] <- 20.000
marshq$cIncome[which(marshq$Income == "£25,001 i £35,000" | marshq$Income == "£25,001 to £35,000")] <- 30.000
marshq$cIncome[which(marshq$Income == "£35,001 i £45,000" | marshq$Income == "£35,001 to £45,000")] <- 40.000
marshq$cIncome[which(marshq$Income == "£45,001 i £55,000" | marshq$Income == "£45,001 to £55,000")] <- 50.000
marshq$cIncome[which(marshq$Income == "£55,001 to £65,000")] <- 60.000
marshq$cIncome[which(marshq$Income == "£65,001 to £75,000")] <- 70.000
marshq$cIncome[which(marshq$Income == "£75,001 to £85,000")] <- 80.000
marshq$cIncome[which(marshq$Income == "£85,001 to £100,000")] <- 90.000
marshq$cIncome[which(marshq$Income == "More than £100,000")] <- 10.0000
marshq$cIncome[which(marshq$Income == "Don’t know/declined to answer" | marshq$Income == "Ddim yn gwybod/ddim eisiau ateb")] <- NA

# Transform Education into a factor
marshq$fHighest_Edu <- as.factor(marshq$Highest_Edu)

# Transform Age into a factor
marshq$fAge <- as.factor(marshq$Age)

# We create a new variable called Abbr_MiniPostcode
marshq$Abbr_MiniPostcode <- abbreviate(marshq$MiniPostcode, minlength = 2, strict = T)

# We delete gender outliers that produce weird modelling outcomes
marshq <- marshq[-(which(marshq$fGender == 0 | marshq$fGender == 3)),]

# We make Rate_knowledge and Visit marsh variables, and the rest of ordinal variables as factors (ordinal ones, with ordered = T)
marshq$fVisited_marshOK <- as.factor(marshq$Visited_marshOK)
marshq$fRate_knowledge <- factor(marshq$Rate_knowledge, ordered = T)
marshq$A_SM_protected <- factor(marshq$A_SM_protected, ordered = T)
marshq$A_realignment_positive <- factor(marshq$A_realignment_positive, ordered = T)
marshq$A_SM_impacted_SLR <- factor(marshq$A_SM_impacted_SLR, ordered = T)
marshq$A_SM_undervalued <- factor(marshq$A_SM_undervalued, ordered = T)
marshq$A_clim_change_mgmnt <- factor(marshq$A_clim_change_mgmnt, ordered = T)
marshq$A_CC_positive <- factor(marshq$A_CC_positive, ordered = T)
marshq$A_SM_impacted_cities <- factor(marshq$A_SM_impacted_cities, ordered = T)
marshq$A_SM_important_agric <- factor(marshq$A_SM_important_agric, ordered = T)
marshq$A_SM_coastal_protec <- factor(marshq$A_SM_coastal_protec, ordered = T)
marshq$A_SM_valuable <- factor(marshq$A_SM_valuable, ordered = T)
marshq$A_SM_shelter_fish <- factor(marshq$A_SM_shelter_fish, ordered = T)
marshq$A_SM_prevent_erosion <- factor(marshq$A_SM_prevent_erosion, ordered = T)
marshq$A_SM_habitat_wildlife <- factor(marshq$A_SM_habitat_wildlife, ordered = T)
marshq$A_improve_water_qual <- factor(marshq$A_improve_water_qual, ordered = T)
marshq$I_recreation <- factor(marshq$I_recreation, ordered = T)
marshq$I_tourism <- factor(marshq$I_tourism, ordered = T)
marshq$I_coastal_protec <- factor(marshq$I_coastal_protec, ordered = T)
marshq$I_habitats <- factor(marshq$I_habitats, ordered = T)
marshq$I_water_purif <- factor(marshq$I_water_purif, ordered = T)
marshq$I_wellbeing <- factor(marshq$I_wellbeing, ordered = T)
marshq$I_agriculture <- factor(marshq$I_agriculture, ordered = T)
marshq$I_landscape <- factor(marshq$I_landscape, ordered = T)
marshq$I_nursery <- factor(marshq$I_nursery, ordered = T)
marshq$I_pollination <- factor(marshq$I_pollination, ordered = T)
marshq$I_Cstorage <- factor(marshq$I_Cstorage, ordered = T)
marshq$I_food <- factor(marshq$I_food, ordered = T)
marshq$I_prevention_erosion <- factor(marshq$I_prevention_erosion, ordered = T)
marshq$I_mitigating_CC <- factor(marshq$I_mitigating_CC, ordered = T)


# # # # # # # # # # # # # #
# Joining our data set to # 
# Rural vs Urban data set --------------------------------------------------------------
# (data from the National #
# Statistics Office)      #
# # # # # # # # # # # # # # 

load("Data/rural_urban_postcodes.RData")

# Checking potential join mismatches
# marshq %>% 
#   anti_join(RurUrbPostcode, by = c("Postcode" = "postcode")) 
# Correct. It only has problems with those entries whose postcode == NAs

# We make the join.
marshq2 <- marshq %>% 
  left_join(RurUrbPostcode, by = c("Postcode" = "postcode"))


# We categorise just between RURAL OR URBAN
marshq2 <- marshq2 %>% 
  mutate(rural_urban2 = ifelse(str_detect(rural_urban, "Rural"), "Rural", "Urban"))

marshq2$rural_urban2 <- factor(marshq2$rural_urban2)

# We clean the slate, to use source() in another document
rm(marshq, RurUrbPostcode)

# Checking the amount of people from urban and rural settings
# marshq2 %>% 
#   filter(!is.na(rural_urban)) %>% 
#   group_by(rural_urban) %>% 
#   summarise(n = n()) %>%
#   mutate(percent = n/sum(n)) %>% 
#   ggplot() +
#   geom_bar(aes(x = reorder(rural_urban, percent, mean), y = percent*100), stat = "identity") +
#   ylab("%") +
#   xlab("") +
#   coord_flip()
# ggsave("Figs&Tables/RuralUrbanBarplot.pdf")    

# marshq2 %>% 
#     filter(!is.na(rural_urban)) %>%
#     group_by(rural_urban2) %>%
#     summarise(n = n()) %>%
#     mutate(percent = n/sum(n)) %>%
#     ggplot() +
#     geom_bar(aes(x = reorder(rural_urban2, percent, mean), y = percent*100), stat = "identity") +
#     ylab("%") +
#     xlab("")
# ggsave("Figs&Tables/RuralUrbanSimpleBarplot.pdf")
    

    
    
    
    
  




