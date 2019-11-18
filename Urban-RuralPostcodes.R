# # # # # # # # # # # # # # # # # # # # # # # # 
# Getting a vector of Rural vs Urban          #
# postcodes for the national survey data set  #
# 15-04-2019                                  #
# # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # 
# Libraries ----------------------------------------------------------------------------------------------------
# # # # # #

library(tidyverse)
# library(tidylog)


# # # # # # # # # 
# Importing data ----------------------------------------------------------------------------------------------------
# # # # # # # # # 

# Data sets from the Office for National Statistics.
# This first data set contains information on whether a specific censal unit is rural or urban
# However, it does not contain information about the postdcode. 
# Data set rural-urban obtained from here: http://geoportal.statistics.gov.uk/items/rural-urban-classification-2011-of-output-areas-in-england-and-wales
rural_urban <- read_csv("Data/RUC11_OA11_EW.csv")

# This data set is quite heavy (ca. 327MB)
# To be able to match each postcode with a censal unit, we need this data set
# Data set postcodes obtained from here: https://geoportal.statistics.gov.uk/datasets/823ece4a492a40298fb74ba4b5a14c46_0
postcodes <- read_csv("Data/Postcode_to_Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_May_2018_Lookup_in_the_UK.csv")


# # # # # # # # # # # # 
# Tidying the data sets ----------------------------------------------------------------------------------------------------
# # # # # # # # # # # #
unique(postcodes$oa11cd)
unique(rural_urban$OA11CD)

# Checking potential join mismatches
notJoined <- rural_urban %>% 
  anti_join(postcodes, by = c('OA11CD' = 'oa11cd')) 
# These 39 oa11cd observations don't have a postcode equivalent... 

# Join both data sets by oa11cd column
rural_urban_postcodes <- rural_urban %>% 
  left_join(postcodes, by = c("OA11CD" = "oa11cd"))

# We eliminate the potential join mismatches by deleting those OA11CD that do not have a postcode
rural_urban_postcodes <- rural_urban_postcodes %>% 
  filter(!is.na(pcd7))


# Summarising the rural-urban component for the 'mini'postcodes we have from the National Survey:
# We will take the most frequent qualification for the all the postcodes under the 'mini' postcode.
# This FOR LOOP IS TIME CONSUMING!
postcode_vector <- unique(marshq$Postcode)
postcode_vector <- as.character(postcode_vector[which(!is.na(postcode_vector))])
RurUrbPostcode <- data.frame(postcode = postcode_vector, rural_urban = NA)
for(i in 1:length(postcode_vector)){
  a <- rural_urban_postcodes %>%
    filter(str_detect(pcd7, postcode_vector[i])) %>% 
    select(pcd7, RUC11) %>% 
    group_by(RUC11) %>% 
    summarise(n = n())
  if(length(a$n != 0)){
    RurUrbPostcode$rural_urban[i] <- a$RUC11[which(a$n == max(a$n))]
  }
}
  

# We save the results from this for loop into this file
save(RurUrbPostcode, file = "Data/rural_urban_postcodes.RData")




