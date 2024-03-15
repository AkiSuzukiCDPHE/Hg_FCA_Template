# R SCRIPT FOR MERCURY 2024 UPDATE TO COMBINE ALL POPULATIONS AND EXPORT ADVISORIES

# This R Script will combine the GP, WCBA, and Child site-specific data into one dataframe
# and rename, reorder, and delete extraneous variables.

library(dplyr)


# Merge data from all three population's dataframe's
Hg_merged_all_1 <- merge(merge(Hg_GP_SS_Merged2, Hg_WCBA_SS_Merged2, by = c("Waterbody", "Species_Codes", 
  "Old_Species_Codes", "Species", "Average_Result", "Units", "Num_Obs", "FishType", "Analyte1", 
  "Commonly_Consumed", "Length_Inches", "Pred_Length"), all = TRUE), Hg_Child_SS_Merged2, by = c("Waterbody", 
  "Species_Codes", "Old_Species_Codes", "Species", "Average_Result", "Units", "Num_Obs", "FishType",
  "Analyte1", "Commonly_Consumed", "Length_Inches", "Pred_Length"), all = TRUE)


# Rename variables to be easily understood in output
Hg_merged_all_2 <- Hg_merged_all_1 %>% 
  rename(
    GP_Existing_SW = In_Existing_Advisory.x,
    WCBA_Existing_SW = In_Existing_Advisory.y,
    Child_Existing_SW = In_Existing_Advisory,
    
    GP_Existing_SS = MealsMonth_ExistingSS.x, 
    WCBA_Existing_SS = MealsMonth_ExistingSS.y,
    Child_Existing_SS= MealsMonth_ExistingSS,  
    
    GP_Status_Existing_SS = Existing_SS_Advisory.x, 
    WCBA_Status_Existing_SS = Existing_SS_Advisory.y,
    Child_Status_Existing_SS= Existing_SS_Advisory,  
    
    GP_SS_Status = New_SS.x, 
    WCBA_SS_Status = New_SS.y,
    Child_SS_Status = New_SS,
    
    GP_Meals_SS = GP_MealsPerMonthSS, 
    WCBA_Meals_SS = WCBA_MealsPerMonthSS, 
    Child_Meals_SS = Children_MealsPerMonthSS,
    
    Population_GP = Population.x, 
    Population_WCBA = Population.y, 
    Population_Child = Population, 
    
    GP_Meals_SW = GP_MealsPerMonthSW, 
    WCBA_Meals_SW = WCBA_MealsPerMonthSW, 
    Child_Meals_SW = Children_MealsPerMonthSW,
    
  )



# Reorder variables
Hg_merged_all_3 <- Hg_merged_all_2 %>%
  select(
    Waterbody, Species_Codes, Old_Species_Codes, Species, Average_Result, Units, Num_Obs, FishType, Analyte1, Commonly_Consumed, Length_Inches, Pred_Length, 
    
    GP_Meals_SS, GP_SS_Status, GP_Existing_SW, GP_Meals_SW,  Population_GP, GP_Status_Existing_SS, GP_Existing_SS,
    
    WCBA_Meals_SS, WCBA_SS_Status, WCBA_Existing_SW, WCBA_Meals_SW, Population_WCBA, WCBA_Status_Existing_SS, WCBA_Existing_SS,
    
    Child_Meals_SS, Child_SS_Status,  Child_Existing_SW, Child_Meals_SW, Population_Child, Child_Status_Existing_SS, Child_Existing_SS,
  )


# Delete variables. 
Hg_merged_all_4 <- subset(Hg_merged_all_3, select = -c(GP_Existing_SW, WCBA_Existing_SW, Child_Existing_SW) )


# filter for waterbodies with new data
# Hg_merged_all_4 <- Hg_merged_all_4 %>% filter(Waterbody %in% c("name 1", "name 2"))


# This is the final data frame that has all new, lifted, and removed advisories.
# This data also includes species with no advisory.
# For 2024 update this entire data set was exported and examined to determine which advisories
# were lifted, new, and updated. 
HgSS_Final_AllPops<- Hg_merged_all_4


# Exporting the final dataframe
# install.packages("writexl")
# library("writexl")
# write_xlsx(HgSS_Final_AllPops,"C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA/04_Output/Hg_2024_Update/Site-specific_Hg_Advisory/SS_TimeNOTCensored_USETHIS/Semi_Final_Dataset//HgSS_Final_AllPops.xlsx")













