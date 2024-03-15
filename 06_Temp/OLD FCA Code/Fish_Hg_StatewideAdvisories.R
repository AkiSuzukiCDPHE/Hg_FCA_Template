#MERCURY STATEWIDE ADVISORY

library(ggplot2)
library(dplyr)
library(broom)
# library(ggpubr)
library(readr)
library("tidyverse")


#Upload the dataset as a csv
fishclean <- read_csv("C:/Users/oasuzuki/Documents/R/fishclean.csv")

#Turn off scientific notation
options(scipen = 999)

#Create a new data frame from the uploaded dataset

HgData_5 <- fishclean


# Calculating the average concentration (result) for each species
# across all water bodies for the statewide advisory

HgData_Statewide = HgData_5 %>%
  group_by(Species_Codes) %>%
  mutate(Average_Result = mean(Result, na.rm = TRUE),
         Num_Obs = sum(!is.na(Average_Result)))

# sub setting the data frame to only include one row per average result for a species
# Also choosing which variables to keep.

HgData_Statewide = distinct(HgData_Statewide, Average_Result, .keep_all = TRUE) %>%
  select(Species, Old_Species_Codes, Species_Codes, Analyte1, Average_Result, Num_Obs, In_Existing_Advisory, Commonly_Consumed)


#General Population Statewide Advisory Calculations using updated BW and portion size

HgData_Statewide1=HgData_Statewide %>%
  mutate(GP_MealsPerMonth=case_when(Average_Result>0 & Average_Result <=.04 ~24,
                                    Average_Result>0.04 & Average_Result <=0.05 ~20,
                                    Average_Result>0.05 & Average_Result <=0.07 ~16,
                                    Average_Result>0.07 & Average_Result <=0.09 ~12,
                                    Average_Result>0.09 & Average_Result <=0.13 ~8,
                                    Average_Result>0.13 & Average_Result <=0.27 ~4,
                                    Average_Result>0.27 & Average_Result <=0.36 ~3,
                                    Average_Result>0.36 & Average_Result <=0.53 ~2,
                                    Average_Result>0.53 & Average_Result <=1.07 ~1,
                                    Average_Result>1.07 & Average_Result <=2.14 ~.5,
                                    Average_Result>2.14 & Average_Result <=4.28 ~.25,
                                    TRUE~0))
                                  

#creating variable that determines whether advisory has changed from existing advisory;
HgData_Statewide1=HgData_Statewide1 %>%
  mutate(Advisory_Change=case_when(Species_Codes %in% c("BRK","KOK","DRM") & (GP_MealsPerMonth=="8")~"No",
                                   Species_Codes %in% c("BBH", "BCR", "BGL", "LOC", "CCF", "CPP", "SNF", "MAC","LMB","NPK","RBT","SAG","SPL"
                                                        ,"SBS","Wal","WAL","WBA", "WHS", "SXW","YPE","WCR") & (GP_MealsPerMonth=="4")~"No",
                                   Species_Codes %in% c("NAT", "NPK", "SMB", "WAL") & (GP_MealsPerMonth=="2")~"No",
                                   Species_Codes %in% c("LMB","SMB","TGM") & (GP_MealsPerMonth=="1")~"No",
                                   TRUE~"Yes"))


#creating variable for the old meals per month from the existing advisory
HgData_Statewide1=HgData_Statewide1 %>%
  mutate(OldGP_MealsPerMonth=case_when(Species_Codes %in% c("BRK","KOK","DRM")~"8",
                                   Species_Codes %in% c("BBH", "BCR", "BGL", "LOC", "CCF", "CPP", "SNF", "MAC","LMB","NPK","RBT","SAG","SPL"
                                                        ,"SBS","Wal","WAL","WBA", "WHS", "SXW","YPE","WCR") ~"4",
                                   Species_Codes %in% c("NAT", "NPK", "SMB", "WAL")~"2",
                                   Species_Codes %in% c("LMB","SMB","TGM")~"1",
                                   TRUE ~ "No existing advisory"))




