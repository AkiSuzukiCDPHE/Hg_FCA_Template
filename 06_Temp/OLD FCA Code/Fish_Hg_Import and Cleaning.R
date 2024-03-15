# R SCRIPT FOR CLEANING MERCURY DATA

#setting a working directory

setwd("C:/Users/oasuzuki/Documents/R")
getwd

#importing xlsx files
install.packages("readxl")
library(readxl)
HgData=read_excel("C:/Users/oasuzuki/Documents/R//HGData_Clean.xlsx")

# Step 1: Rename column 23 to 'Tissue Type'
names(HgData)[23]='Tissue_Type'
names(HgData)[12]='Sample_Year'
names(HgData)[1] = 'Data_Cleaning_Notes'
colnames(HgData)

# Step 2: Remove samples where tissue type = "O" or "E" (M & plug only)
HgData_1=HgData[HgData$Tissue_Type %in% c('M','Plug'),]
table(HgData_1$Tissue_Type)

# Step 3: Delete samples collected in 2004 to remove error where Hg result is listed as
# < MDL but value is above the listed MDL
HgData_1=HgData[HgData$Sample_Year!= 2004,]
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

HgData_2=HgData_1 %>%
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
                          

table(HgData_2$Species1)

# Step 5:Lump waterbodies together
  #5a. "Aglient Pond A", "Agilent Pond B", & "Agilent Ponds" = "Agilent Ponds"
  #5b. "Carter Lake" & "Carter Reservoir" = "Carter Lake"
  #5c. "Yampa River", "Yampa River 2", & "Yampa River 3" = "Yampa River"
  #5d. "North Delaney Reservoir" & "North Delaney Reservoir Reservoir" = "North Delaney Reservoir"
  #5e. "North Sterling Reservoir" & " North Sterling Lake" = "North Sterling Reservoir"
  #5f. "Shadow Mountain Lake" & "Shadow Mountain Reservoir" = "Shadow Mountain Lake"

library(dplyr)
HgData_3=HgData_2 %>%
  mutate(Waterbody = case_when(Waterbody1 %in% c("Agilent Pond A", "Agilent Pond B", "Agilent Ponds") ~ "Agilent Ponds",
                               Waterbody1 %in% c("Carter Lake","Carter Reservoir") ~ "Carter Lake",
                               Waterbody1 %in% c("Yampa River","Yampa River 2","Yampa River 3") ~ "Yampa River",
                               Waterbody1 %in% c("North Delaney Reservoir","North Delaney Reservoir Reservoir") ~ "North Delaney Reservoir",
                               Waterbody1 %in% c("North Sterling Reservoir","North Sterling Lake") ~ "North Sterling Reservoir",
                               Waterbody1 %in% c("Shadow Mountain Lake","Shadow Mountain Reservoir") ~ "Shadow Mountain Lake",
                               TRUE ~ Waterbody1))


# Step 6:Creating a new variable, Species_Codes, with fish subspecies lumped together

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

# Step 7:Create new column denoting whether a species is in the outdated current advisory
  HgData_4=HgData_3 %>%
  mutate(In_Existing_Advisory = case_when(Species1 %in% c("GRA", "SQF", "CFI", "FMS","GSD","GSF", "HBG", 
                                                    "LGS","PKS","SGR", "SPB","TRT","SXX") ~ "No",
                                   TRUE ~ "Yes"))


# Step 8: Creating a new variable for whether a fish species is commonly consumed
  HgData_4=HgData_4 %>%
  mutate(Commonly_Consumed = case_when(Species1 %in% c("SQF", "CFI","FMS","GSD","GSF") ~ "No",
                                      TRUE ~ "Yes"))

# Step 9: Renaming variables
  names(HgData_4)[6]='Qualifier'
  names(HgData_4)[10]='MDL_PQL'
  
# Step 10: Replacing non-detect values with the MDL/PQL. This datastep uses the case_when function to
  #replace values of an existing variable based on values of another existing variable
  
  HgData_5=HgData_4 %>%
  mutate(Result=case_when(Qualifier == "<"~ MDL_PQL,
                                    TRUE~ Result))
  
# Step 11: Creating a new variable for fish length in inches based on the variable for length in mm
  HgData_5$Length_Inches=HgData_5$`Length (mm)`/25.4
  
  #Checking frequency tables
  table(HgData_5$In_Existing_Advisory)
  table(HgData_5$Length_Inches)
  
#This is your cleaned data frame
  HgData_5

  
  HgData_5 <- select( HgData_5,- Data_Cleaning_Notes)
  names(HgData_5)[18]="Old_Species_Codes"
  
  colnames(HgData_5)
  
  WaterbodiesNames=HgData_5 %>% select(Waterbody) %>% unique()
  SpeciesNames=HgData_5 %>% select(c(Species_Codes, Species)) %>% unique()
  
  
  SpeciesNames= data.frame(table (HgData_5$Species_Codes, HgData_5$Species))
  