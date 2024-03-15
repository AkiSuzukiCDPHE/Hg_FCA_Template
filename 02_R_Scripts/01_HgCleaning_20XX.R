# R SCRIPT FOR CLEANING THE MERCURY DATA set

# setting a working directory
setwd("C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA")
getwd


# importing xlsx files of raw data. 
install.packages("readxl")
library(readxl)
HgData <- read_excel("C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA/01_Raw_Data/HgData_Clean.xlsx")

# Step 1: Rename variables with long names. The variable number position may change with
# different datasets. Double check before running.
names(HgData)[23]='Tissue_Type'
names(HgData)[12]='Sample_Year'
names(HgData)[1] = 'Data_Cleaning_Notes'
names(HgData)[6]='Qualifier'
names(HgData)[10]='MDL_PQL'

# Confirm variable names changed.
colnames(HgData)

# Step 2: Remove samples where tissue type = "O" or "E" (keep M & plug only)
HgData_1 <- HgData[HgData$Tissue_Type %in% c('M','Plug'),]

# Run a frequency table to make sure filtering for M and Plug worked
table(HgData_1$Tissue_Type)

# Step 3: Delete samples collected in 2004 to remove error where Hg result is listed as
# < MDL but value is above the listed MDL. 
HgData_1 <- HgData[HgData$Sample_Year!= 2004,]

# Confirm you removed the 2004 samples by running a frequency table
table(HgData_1$Sample_Year)

# Step 4: Create a new column titled "species" and populate with species names, including lumping species as specified
#4a. NPK & NPKB = Northern pike"
#4b. WALB & WAL = "Walleye"
#4c. SMBB & SMB = "Smallmouth bass"
#4d. LMBB & LMB = "Largemouth bass"
#4e. SAGB & SAG = Saugeye
#4f. NAT = "Cutthroat"
#4g. SRN = "Cutthroat"
#4h. RGN = "Cutthroat"
#4i. CRN = "Cutthroat"
#4j. RXN = "Rainbow trout"


library(dplyr)
HgData_2 <- HgData_1 %>%
  mutate(Species = case_when(Species1 == "SMB" ~ "Smallmouth Bass",
                             Species1 == "SMBB" ~ 'Smallmouth Bass',
                             Species1 == "LMB" ~ 'Largemouth Bass',
                             Species1 == "LMBB" ~ 'Largemouth Bass',
                             Species1 == "TGM" ~ 'Tiger Muskie',
                             Species1 == "NPK" ~ 'Northern Pike',
                             Species1 == "NPKB" ~ 'Northern Pike',
                             Species1 == "WAL" ~ 'Walleye',
                             Species1 == "WALB" ~ 'Walleye',
                             Species1 == "BCR" ~ "Black Crappie",
                             Species1 == "BGL" ~ "Bluegill",
                             Species1 == "BBH" ~ "Black Bullhead",
                             Species1 == "CPP" ~ 'Common Carp',
                             Species1 == "CCF" ~ 'Channel Catfish',
                             Species1 == "MAC" ~ "Lake Trout(Mackinaw)",
                             Species1 == "SAG" ~ 'Saugeye',
                             Species1 == "SAGB" ~ "Saugeye",
                             Species1 == "SPL" ~ "Splake",
                             Species1 == "SBS" ~ "Striped Bass",
                             Species1 == "SNF" ~ "Green Sunfish",
                             Species1 == "WBA" ~ "White Bass",
                             Species1 == "SXW" ~ "Wipe",
                             Species1 == "WHS" ~ "White Sucker",
                             Species1 == "YPE" ~ "Yellow Perch",
                             Species1 == "RXC" ~ "Rainbow Trout x Cutthroat",
                             Species1 == "RXN" ~ "Rainbow Trout",
                             Species1 == "SRN" ~ "Cutthroat",
                             Species1 == "RGN" ~ "Cuttrhoat",
                             Species1 == "BRK" ~ "Brook Trout",
                             Species1 == "RBT" ~ "Rainbow Trout",
                             Species1 == "KOK" ~ "Kokanee",
                             Species1 == "LOC" ~ "Brown Trout",
                             Species1 == "DRM" ~ "Freshwater Drum",
                             Species1 == "PKS" ~ "Pumpkinseed",
                             Species1 == "SGR" ~ "Sauger",
                             Species1 == "WCR" ~ "White Crappie",
                             Species1 == "NAT" ~ "Cutthroat",
                             Species1 == "CRN" ~ "Cutthroat",
                             Species1 == "LGS" ~ "Longnose Sucker",
                             Species1 == "GRA" ~ "Arctic Grayling",
                             Species1 == "SQF" ~ "Colorado Pikeminnow",
                             Species1 == "CFI" ~ "Crayfish",
                             Species1 == "FMS" ~ "Flannelmouth Sucker",
                             Species1 == "GSD" ~ "Gizzard Shed",
                             Species1 == "GSF" ~ "Golden Shiner maybe (GSH)",
                             Species1 == "HBG" ~ "Hybrid Bluegill",
                             Species1 == "SPB" ~ "Spotted Bass",
                             Species1 == "TRT" ~ "Trout - unspecified",
                             Species1 == "SXX" ~ "White bass x wiper backcross"))
                          

# Confirm that new variable with full specie's names was correctly added to the dataframe
table(HgData_2$Species1)

# Step 5:Lump waterbodies together if they have multiple names for the same waterbody
  #5a. "Aglient Pond A", "Agilent Pond B", & "Agilent Ponds" = "Agilent Ponds"
  #5b. "Carter Lake" & "Carter Reservoir" = "Carter Lake"
  #5c. "Yampa River", "Yampa River 2", & "Yampa River 3" = "Yampa River"
  #5d. "North Delaney Reservoir" & "North Delaney Reservoir Reservoir" = "North Delaney Reservoir"
  #5e. "North Sterling Reservoir" & " North Sterling Lake" = "North Sterling Reservoir"
  #5f. "Shadow Mountain Lake" & "Shadow Mountain Reservoir" = "Shadow Mountain Lake"
HgData_3 <- HgData_2 %>%
  mutate(Waterbody = case_when(Waterbody1 %in% c("Agilent Pond A", "Agilent Pond B", "Agilent Ponds") ~ "Agilent Ponds",
                               Waterbody1 %in% c("Carter Lake","Carter Reservoir") ~ "Carter Lake",
                               Waterbody1 %in% c("Yampa River","Yampa River 2","Yampa River 3") ~ "Yampa River",
                               Waterbody1 %in% c("North Delaney Reservoir","North Delaney Reservoir Reservoir") ~ "North Delaney Reservoir",
                               Waterbody1 %in% c("North Sterling Reservoir","North Sterling Lake") ~ "North Sterling Reservoir",
                               Waterbody1 %in% c("Shadow Mountain Lake","Shadow Mountain Reservoir") ~ "Shadow Mountain Lake",
                               TRUE ~ Waterbody1))


# Step 6:Create a new variable, Species_Codes, with fish subspecies lumped together.
HgData_3=HgData_3 %>%
  mutate(Species_Codes = case_when(Species1 == "SMBB" ~ "SMB",
                                   Species1 == "LMBB" ~ 'LMB',
                                   Species1 == "NPKB" ~ 'NPK',
                                   Species1 == "WALB" ~ 'WAL',
                                   Species1 == "SAGB" ~ "SAG",
                                   Species1 == "RXN" ~ "RBT",
                                   Species1 == "SRN" ~ "NAT",
                                   Species1 == "RGN" ~ "NAT",
                                   Species1 == "CRN" ~ "NAT",
                                   TRUE~Species1))

# Step 7:Create new column denoting whether a species is in the existing statewide advisory. 
  HgData_4 <- HgData_3 %>%
  mutate(In_Existing_Advisory = case_when(Species1 %in% c("GRA", "SQF", "CFI", "FMS","GSD","GSF","HBG", 
                                                    "LGS","PKS","SGR", "SPB","TRT","SXX") ~ "No",
                                   TRUE ~ "Yes"))


# Step 8: Create a new variable for whether a fish species is commonly consumed
  HgData_4 <- HgData_4 %>%
  mutate(Commonly_Consumed = case_when(Species1 %in% c("SQF", "CFI","FMS","GSD","GSF") ~ "No",
                                      TRUE ~ "Yes"))


# Step 9: Replace non-detect values with the MDL/PQL. This step uses the case_when function to
# replace values of one existing variable based on values of another existing variable.
  HgData_5 <- HgData_4 %>%
  mutate(Result=case_when(Qualifier == "<"~ MDL_PQL,
                                    TRUE~ Result))
  
# Step 10: Create a new variable for fish length in inches based on the variable for length in mm
  HgData_5$Length_Inches <- HgData_5$`Length (mm)`/25.4
  
# Step 11: Delete the column Data_Cleaning_Notes 
  HgData_5 <- select( HgData_5,- Data_Cleaning_Notes)
  
# Step 12: Rename Species1 (the original species code variable)
  names(HgData_5)[18]="Old_Species_Codes"
  
# Confirm rename worked
  colnames(HgData_5)
  
  
# Step 13: insert code to merge old cleaned dataframe with new cleaned dataframe
# For example: Hg_CleanedMaster_2024
  
# Step 14: filter out crayfish and trout unspecified because these species should not be included in the cleaned data
# Trout-unspecified is not a species and Crayfish is not a fish.
  HgData_Clean <- subset(HgData_5, !Species %in% c("Crayfish", "Trout - unspecified"))
  
  # This is your cleaned data frame. 
  # HgData_Clean

# Export the data frame as a cleaned master dataset for next year's analysis.
  
#library("writexl")
#Write_xlsx(HgData_Clean,"C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA/03_Clean_Data//Hg_CleanedMaster_2024.xlsx")
  
  

  

  
  