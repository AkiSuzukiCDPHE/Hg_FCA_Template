# R SCRIPT FOR MERCURY STATEWIDE ADVISORY - not censored by time or sample size

library(ggplot2)
library(dplyr)
library(broom)
# library(ggpubr)
library(readr)
library("tidyverse")

# Use the "HgData_Clean" data frame from the cleaning R script.

# Turn off scientific notation
options(scipen = 999)


# CENSOR by time in the future? For example for data < 10 yeard old:
# HgData_Cleana <- HgData_Clean %>% filter (Sample_Year > "2012")


# Calculate the average concentration (result) for each species
# across all water bodies for the statewide advisory
HgData_Statewide = HgData_Clean %>%
  group_by(Species_Codes) %>%
  mutate(Average_Result_SW = mean(Result, na.rm = TRUE),
         Num_Obs = sum(!is.na(Average_Result_SW)))


# sub-set the data frame to only include one row per average result for a species
# Also choosing which variables to keep.
HgData_Statewide = distinct(HgData_Statewide, Average_Result_SW, .keep_all = TRUE) %>%
  select(Species, Old_Species_Codes, Species_Codes, Analyte1, Average_Result_SW, Num_Obs, In_Existing_Advisory, Commonly_Consumed)


# Add the column Unit
HgData_Statewide <- HgData_Statewide %>%
  mutate(Unit = "Mg/Kg")

# Reorder variables
HgData_Statewide= HgData_Statewide %>% select(Species, Old_Species_Codes,Species_Codes, Analyte1, Num_Obs, Average_Result_SW, Unit,
                                   In_Existing_Advisory, Commonly_Consumed)


# GENERAL POPULATION Statewide Advisory Calculations using updated BW and portion size
HgData_Statewide1=HgData_Statewide %>%
  mutate(GP_MealsPerMonth=case_when(Average_Result_SW>0 & Average_Result_SW <=.04 ~24,
                                    Average_Result_SW>0.04 & Average_Result_SW <=0.05 ~20,
                                    Average_Result_SW>0.05 & Average_Result_SW <=0.07 ~16,
                                    Average_Result_SW>0.07 & Average_Result_SW <=0.09 ~12,
                                    Average_Result_SW>0.09 & Average_Result_SW <=0.13 ~8,
                                    Average_Result_SW>0.13 & Average_Result_SW <=0.27 ~4,
                                    Average_Result_SW>0.27 & Average_Result_SW <=0.36 ~3,
                                    Average_Result_SW>0.36 & Average_Result_SW <=0.53 ~2,
                                    Average_Result_SW>0.53 & Average_Result_SW <=1.07 ~1,
                                    Average_Result_SW>1.07 & Average_Result_SW <=2.14 ~.5,
                                    Average_Result_SW>2.14 & Average_Result_SW <=4.28 ~.25,
                                    TRUE~0))
                                  

# Create a variable that determines whether advisory has changed from existing advisory;
HgData_Statewide1=HgData_Statewide1 %>%
  mutate(GP_SW_Change=case_when(Species_Codes %in% c("BRK","KOK","DRM") & (GP_MealsPerMonth=="8")~"No",
                                   Species_Codes %in% c("BBH", "BCR", "BGL", "LOC", "CCF", "CPP", "SNF", "MAC","LMB","NPK","RBT","SAG","SPL"
                                                        ,"SBS","Wal","WAL","WBA", "WHS", "SXW","YPE","WCR") & (GP_MealsPerMonth=="4")~"No",
                                   Species_Codes %in% c("NAT", "NPK", "SMB", "WAL") & (GP_MealsPerMonth=="2")~"No",
                                   Species_Codes %in% c("LMB","SMB","TGM") & (GP_MealsPerMonth=="1")~"No",
                                   TRUE~"Yes"))



# Create a variable for existing statewide advisory recommendations
HgData_Statewide1=HgData_Statewide1 %>%
  mutate(Existing_GP_MealsPerMonth=case_when(Species_Codes %in% c("BRK","KOK","DRM")~"8",
                                       Species_Codes %in% c("BBH", "BCR", "BGL", "LOC", "CCF", "CPP", "SNF", "MAC","LMB","NPK","RBT","SAG","SPL"
                                                            ,"SBS","Wal","WAL","WBA", "WHS", "SXW","YPE","WCR") ~"4",
                                       Species_Codes %in% c("NAT", "NPK", "SMB", "WAL")~"2",
                                       Species_Codes %in% c("LMB","SMB","TGM")~"1",
                                       TRUE ~ "No existing advisory"))


# WOMEN OF CHILDBEARING AGE Statewide Advisory Calculations using updated BW and portion size
HgData_Statewide1=HgData_Statewide1 %>%
    mutate(WCBA_MealsPerMonth=case_when(Average_Result_SW>0 & Average_Result_SW <=.03 ~24,
                                    Average_Result_SW>0.03 & Average_Result_SW <=0.04 ~20,
                                    Average_Result_SW>0.04 & Average_Result_SW <=0.05 ~16,
                                    Average_Result_SW>0.05 & Average_Result_SW <=0.07 ~12,
                                    Average_Result_SW>0.07 & Average_Result_SW <=0.1 ~8,
                                    Average_Result_SW>0.1 & Average_Result_SW <=0.19 ~4,
                                    Average_Result_SW>0.19 & Average_Result_SW <=0.27 ~3,
                                    Average_Result_SW>0.27 & Average_Result_SW <=0.4 ~2,
                                    Average_Result_SW>0.4 & Average_Result_SW <= 0.8 ~1,
                                    Average_Result_SW>0.8 & Average_Result_SW <=1.6 ~.5,
                                    Average_Result_SW>1.6 & Average_Result_SW <=3.2 ~.25,
                                    TRUE~0))



# Create a variable that determines whether advisory has changed from existing advisory;
HgData_Statewide1=HgData_Statewide1 %>%
  mutate(WCBA_SW_Change=case_when(Species_Codes %in% c("BBH","BGL","BRK","LOC","DRM","SNF","KOK","RBT","SPL","WHS") & (WCBA_MealsPerMonth =="4")~"No",
                                   Species_Codes %in% c("BCR","CCF","CPP","NAT","MAC","LMB","NPK","SAG","SMB","SBS","Wal","WAL","WBA",
                                                        "WCR","SXW","YPE") & (WCBA_MealsPerMonth =="2")~"No",
                                   Species_Codes %in% c("TGM") & (WCBA_MealsPerMonth =="1")~"No",
                                   TRUE~"Yes"))


# Create a variable for existing statewide advisory recommendations
HgData_Statewide1=HgData_Statewide1 %>%
  mutate(Existing_WCBA_MealsPerMonth=case_when(Species_Codes %in% c("BBH","BGL","BRK","LOC","DRM","SNF","KOK","RBT","SPL","WHS")~"4",
                                         Species_Codes %in% c("BCR","CCF","CPP","NAT","MAC","LMB","NPK","SAG","SMB","SBS","Wal","WAL","WBA","WCR","SXW","YPE") ~"2",
                                         Species_Codes %in% c("TGM")~"1",
                                             TRUE ~ "No existing advisory"))






# CHILDREN Statewide Advisory Calculations using updated BW and portion size
HgData_Statewide1=HgData_Statewide1 %>%
  mutate(Children_MealsPerMonth=case_when(Average_Result_SW>0 & Average_Result_SW <=0.011 ~24,
                                          Average_Result_SW>0.011 & Average_Result_SW <=0.013 ~20,
                                          Average_Result_SW>0.013 & Average_Result_SW <=0.016 ~16,
                                          Average_Result_SW>0.016 & Average_Result_SW <=0.022 ~12,
                                          Average_Result_SW>0.022 & Average_Result_SW <=0.033 ~8,
                                          Average_Result_SW>0.033 & Average_Result_SW <=0.065 ~4,
                                          Average_Result_SW>0.065 & Average_Result_SW <=0.087 ~3,
                                          Average_Result_SW>0.087 & Average_Result_SW <=0.13 ~2,
                                          Average_Result_SW>0.13 & Average_Result_SW <=0.26 ~1,
                                          Average_Result_SW>0.26 & Average_Result_SW <=0.52 ~.5,
                                          Average_Result_SW>0.52 & Average_Result_SW <=1.04 ~.25,
                                          TRUE~0))



# Create a variable that determines whether advisory has changed from existing advisory;
HgData_Statewide1 = HgData_Statewide1 %>%
  mutate(Children_SW_Change=case_when(Species_Codes %in% c("BBH","BGL","BRK","LOC","CCF", "NAT", "SAG", "WBA", "WCR","DRM","SNF","KOK","RBT","SPL","WHS") & (Children_MealsPerMonth =="2")~"No",
                                   Species_Codes %in% c("BCR","CPP","SNF", "MAC","LMB","NPK","SMB","SBS","TGM","Wal","WAL","SXW","YPE") & (Children_MealsPerMonth =="1")~"No",
                                   TRUE~"Yes"))


# Create a variable for existing statewide advisory recommendations
HgData_Statewide1 = HgData_Statewide1 %>%
  mutate(Existing_Children_MealsPerMonth=case_when(Species_Codes %in% c("BBH","BGL","BRK","LOC","CCF", "NAT", "SAG", "WBA", "WCR","DRM","SNF","KOK","RBT","SPL","WHS")~"2",
                                       Species_Codes %in% c("BCR","CPP","SNF", "MAC","LMB","NPK","SMB","SBS","TGM","Wal","WAL","SXW","YPE") ~"1",
                                       TRUE ~ "No existing advisory"))


# Renaming the dataframe
HgData_Statewide_Final <- HgData_Statewide1 

# This is your final statewide advisory final dataset
# HgData_Statewide_Final

# Statewide Hg Advisory export code

#library("writexl")
#write_xlsx(HgData_Statewide_Final, "C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA/04_Output/Hg_2024_Update/Statewide_Hg_Advisory/Hg_Statewide_Final_2024.xlsx")



