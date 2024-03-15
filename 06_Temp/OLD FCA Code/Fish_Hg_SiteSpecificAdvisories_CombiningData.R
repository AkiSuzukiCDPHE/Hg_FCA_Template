#SITE SPECIFIC ADVISORIES FOR MERCURY FOR GP
library(dplyr)


# clean data to filter out samples > 5 years old before starting decision tree analysis
# following step filtering out species that do not have >= 11 samples, check data <10 years old to see if waterbodies
# that do not have sufficient sample size would have sufficient sample sizes with combined data. Then check if
# meal freq recs from the combined data are the same as those for uncombined data that doesn't meet sample size requirements

#Merge two data frames together to compare them

merged_innerj <- Hg_SS_SS2 %>% 
  inner_join(Hg_SS_IS2, by = c("Waterbody", "Species_Codes", "FishType"))
