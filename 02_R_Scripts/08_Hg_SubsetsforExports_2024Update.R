
# OPTIONAL R SCRIPT TO SUBSET MERCURY SITE-SPECIFIC ADVISORIES.

# Subset to a data frame where GP or WCBA or Children have site-specific advisories.
# Export this.
# Fish TAC should review species that have small sample sizes.
Hg_Allpops_SSUpdates <- Hg_merged_all_4[Hg_merged_all_4$GP_SS_Status == "Yes" 
                                        | Hg_merged_all_4$WCBA_SS_Status == "Yes" 
                                        | Hg_merged_all_4$Child_SS_Status == "Yes", ]



# Subset to data frame with existing site-specific advisories that are more stringent than
# new site-specific advisories (less stringent)
# OR where the new calculations no longer require a site-specific advisory (lifted).
# for first update we will accept all changes bc previous advisories were based on errored data

# dataframe for all pops lifted or less stringent
# Export this.
# Fish TAC should review species that have small sample sizes.

Hg_Allpops_Lifted_LessStringent_SS <- Hg_merged_all_4[
  Hg_merged_all_4$GP_Meals_SS > Hg_merged_all_4$GP_Existing_SS |
    Hg_merged_all_4$WCBA_Meals_SS > Hg_merged_all_4$WCBA_Existing_SS |
    Hg_merged_all_4$Child_Meals_SS > Hg_merged_all_4$Child_Existing_SS,]

Hg_Allpops_Lifted_LessStringent_SS  <- filter(Hg_Allpops_Lifted_LessStringent_SS , rowSums(is.na(Hg_Allpops_Lifted_LessStringent_SS )) != ncol(Hg_Allpops_Lifted_LessStringent_SS ))

Hg_Allpops_Lifted_SS <- Hg_Allpops_Lifted_LessStringent_SS  %>% filter(GP_Meals_SS >= GP_Meals_SW | WCBA_Meals_SS >= WCBA_Meals_SW | Child_Meals_SS >= Child_Meals_SW) #Lifted only


# EXPORTS OF SUBSET DATAFRAMES
library("writexl")

# Dataframe with all updated or new site-specific advisories.
write_xlsx(Hg_Allpops_New_SS,"C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA/04_Output/Hg_FCA_2023_Update/Site-specific_Hg_Advisory//Hg_Updated_SiteSpecific_AllPops_2023.xlsx")

# Dataframes with lifted and less stringent advisories for all populations

write_xlsx(Hg_Allpops_Lifted_LessStringent_SS,"C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA/04_Output/Hg_FCA_2023_Update/Site-specific_Hg_Advisory//Hg_Updated_SS_LiftedLessStringent_Allpops_2023.xlsx")

# Dataframes with lifted advisories for all populations

write_xlsx(Hg_Allpops_Lifted_SS,"C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA/04_Output/Hg_FCA_2023_Update/Site-specific_Hg_Advisory//Hg_Updated_SS_Lifted_Allpops_2023.xlsx")

