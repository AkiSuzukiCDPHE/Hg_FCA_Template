#SITE SPECIFIC ADVISORIES FOR MERCURY FOR WCBA
library(dplyr)


# Creating a new data frame from the data frame created in the length and concentration R file
# Renaming a variable and filtering the Analysis_Date for data > 2017

Hg_SS <- Samples_Final%>% rename("Analysis_Date" = "Analysis\r\r\nDate") %>%
  filter (Analysis_Date > as.Date("2013-01-01"))


# Calculate the average concentration (result) for each species by waterbody.
# The group_by code means that the subsequent calculations will be applied separately for each
# unique combination of Waterbody, Species_Codes, and FishType.
# The Num_obs=n()) line calculates the number of observations within each group of waterbody and species
# and creates a new variable called Num_Obs to store these counts i.e. it calculates the sample size for the ss
# advisory.

Hg_SS2 <- Hg_SS %>%
  group_by(Waterbody,Species_Codes,FishType) %>%
  mutate(Average_Result = mean(Result),
         Num_Obs = n())

# Obtain the unique rows based on the specified columns from the Hg_SS2 data frame.
# The distinct function keeps the distinct rows based on the Waterbody column while preserving all
# other columns specified in the .keep_all argument.
# This operation ensures that duplicate rows based on Waterbody are removed while preserving the values of other variables.

Hg_SS2= distinct(Hg_SS2, Waterbody, .keep_all = TRUE) %>%
  select(Waterbody, Species, Old_Species_Codes, Species_Codes, Analyte1, Average_Result, Num_Obs, In_Existing_Advisory,
         Commonly_Consumed,Length_Inches,Pred_Length, FishType)


# Filtering the data frame to include only observations that have a sufficient sample size

threshold <- 11  # Value from power analysis at 80% power, 40% detectable difference, and alpha.05

# Creating a variable for sufficient sample size using the ifelse statement

Hg_SS3 <- Hg_SS2 %>%
  mutate(Sufficient_Sample = ifelse(Num_Obs >= threshold, "Yes", "No"))

# Checking the frequency of observations that have a sufficient vs insufficient sample

table(Hg_SS3$Sufficient_Sample)

# Creating a new data frame filtered for species that have a sufficient sample size

Hg_SS4a <- Hg_SS3 %>% filter(Sufficient_Sample == "Yes")

# Create a new data frame filtered for only those species that DO NOT have a sufficient sample size.
# Hg_SS4b is your data frame that you will compare to the data frame with combined data from older years

Hg_SS4b <- Hg_SS3 %>% filter(Sufficient_Sample != "Yes")


# Assign meal frequency recommendations for site-specific advisories for data frame with insufficient sample sizes
# DOUBLE CHECK THESE ARE THE CORRECT FCLGS IN MG/KG

Hg_SS4b_manualreview=Hg_SS4b_manualreview %>%
  mutate(WCBA_MealsPerMonth=case_when(Average_Result>0 & Average_Result <=.03 ~24,
                                      Average_Result>0.03 & Average_Result <=0.04 ~20,
                                      Average_Result>0.04 & Average_Result <=0.05 ~16,
                                      Average_Result>0.05 & Average_Result <=0.07 ~12,
                                      Average_Result>0.07 & Average_Result <=0.1 ~8,
                                      Average_Result>0.1 & Average_Result <=0.19 ~4,
                                      Average_Result>0.19 & Average_Result <=0.27 ~3,
                                      Average_Result>0.27 & Average_Result <=0.4 ~2,
                                      Average_Result>0.4 & Average_Result <= 0.8 ~1,
                                      Average_Result>0.8 & Average_Result <=1.6 ~.5,
                                      Average_Result>1.6 & Average_Result <=3.2 ~.25,
                                      TRUE~0))




# Assign meal frequency recommendations for site-specific advisories using the
# data frame with sufficient sample sizes
# DOUBLE CHECK THESE ARE THE CORRECT FCLGS IN MG/KG

Hg_SS5=Hg_SS4a %>%
  mutate(WCBA_MealsPerMonth=case_when(Average_Result>0 & Average_Result <=.03 ~24,
                                    Average_Result>0.03 & Average_Result <=0.04 ~20,
                                    Average_Result>0.04 & Average_Result <=0.05 ~16,
                                    Average_Result>0.05 & Average_Result <=0.07 ~12,
                                    Average_Result>0.07 & Average_Result <=0.1 ~8,
                                    Average_Result>0.1 & Average_Result <=0.19 ~4,
                                    Average_Result>0.19 & Average_Result <=0.27 ~3,
                                    Average_Result>0.27 & Average_Result <=0.4 ~2,
                                    Average_Result>0.4 & Average_Result <= 0.8 ~1,
                                    Average_Result>0.8 & Average_Result <=1.6 ~.5,
                                    Average_Result>1.6 & Average_Result <=3.2 ~.25,
                                    TRUE~0))



0 - .03
.03 - .04
.04 - .05
.05 - .07
.07 - .1
.1 - .19
.19 - .27
.27 - .4
.4 - .8
.8 - 1.6
1.6 - 3.2
>3.2


# Is THERE AN EXISTING SS ADVISORY?

# Importing xlsx file for the data frame with existing ss advisories

# install.packages("readxl")
library(readxl)
Hg_Existing_SSAdvisories=read_excel("C:/Users/kberg/Downloads/Hg_Existing_SSAdvisories.xlsx")


# Filtering the data frame for only Women of Childbearing Age Population
Hg_Existing_SSAdvisories %>% select(ExistingSS_Size) %>% distinct()

Hg_ExistingSS <- Hg_Existing_SSAdvisories %>% filter(Population == "Pregnant women")

Hg_ExistingSS <- Hg_ExistingSS %>% 
  group_by(Waterbody, Species, Existing_SS_Advisory, Population) %>%
  summarize(MealsMonth_ExistingSS = min(MealsMonth_ExistingSS)) %>%
  arrange(Waterbody, Species)


# Merging the existing site-specific advisory data frame with with Hg_SiteSpecific5

Hg_SS6 <- left_join(Hg_SS5, Hg_ExistingSS %>% select( Waterbody, Species, Population, MealsMonth_ExistingSS, Existing_SS_Advisory),
                    by = c("Waterbody", "Species"))


#Filtering the data frame to subset species that have existing site-specific advisories and those that do not

Hg_SS6a <- Hg_SS6 %>% filter(Existing_SS_Advisory == "Yes")

Hg_SS6b <- subset(Hg_SS6, is.na(Existing_SS_Advisory))


# FOR SPECIES WITH A SITE-SPECIFIC ADVISORY

# Creating a new variable to check if the new site-specific advisories are more stringent than the existing
# site-specific advisories for species

Hg_SS6a_1 <- Hg_SS6a %>%           
  group_by(Species_Codes) %>%
  mutate(New_SS = case_when(
    WCBA_MealsPerMonth < MealsMonth_ExistingSS ~ "Yes",
    TRUE ~ "No"
  )) 

# Filtering the data frame for (1) species where new site-specific advisory is more stringent than the existing
# site-specific advisory and (2) where new site-specific advisory is less stringent or equal to the existing
# site-specific advisory

# Data frame with new site-specific advisories more stringent than the the existing one

Hg_SS6a_1a  <- Hg_SS6a_1  %>% filter(New_SS == "Yes")


# Data frame with species that should keep their old, more stringent site-specific advisories

Hg_SS6a_1b <- Hg_SS6a_1  %>% filter(New_SS == "No")



# FOR SPECIES WITHOUT A SITE-SPECIFIC ADVISORY


# Merge the data frame for species that do not have site-specific advisories with the
# statewide Hg advisory data frame

# The select function is used to choose only the columns Species_Codes and WCBA_MealsPerMonth
# from HgData_Statewide1.
# The left_join function is used to perform a left join between Hg_SS5 and the
# result of the select operation.
# The by = "Species_Codes" argument specifies the column used for the join, which is Species_Codes
# Rows from Hg_SS5 that have matching values in the Species_Codes column with
# HgData_Statewide1 will have corresponding values from HgData_Statewide1 included. Rows without a
# match will have NA values for the columns from HgData_Statewide1


Hg_SS6b_1 <- left_join(Hg_SS6b, HgData_Statewide1 %>% select(Species_Codes, WCBA_MealsPerMonth),
                       by = "Species_Codes")


#Renaming the variables for WCBA_MealsPerMonth

Hg_SS6b_1  <- Hg_SS6b_1  %>%
  rename(WCBA_MealsPerMonthSS = WCBA_MealsPerMonth.x, WCBA_MealsPerMonthSW = WCBA_MealsPerMonth.y)


#Filtering the data frame to subset species that are on the statewide advisory (<= 8 meals per month)
# and species that are not on the statewide advisory

Hg_SS6b_1a <- Hg_SS6b_1 %>% filter(WCBA_MealsPerMonthSW <=8) # Species that have a statewide advisory

Hg_SS6b_1b <- Hg_SS6b_1 %>% filter(WCBA_MealsPerMonthSW > 8) # Species that DO NOT have a statewide advisory


# FOR SPECIES WITH A STATEWIDE ADVISORY (<= 8 meals per month)

# Create a new variable to check if the site-specific advisory for a species is more stringent than the statewide advisory.

# The group_by function groups the data by the variable Species_Codes.The subsequent operations will
# be applied separately within each group
# The value of this variable is determined based on whether the corresponding value of WCBA_MealsPerMonthSS is
# less than the corresponding value of WCBA_MealsPerMonthSW within each group defined by Species_Codes. 
# If true, the value of the variable New_SS is "Yes"; otherwise, it's "No".

Hg_SS6b_1a <- Hg_SS6b_1a %>%           
  group_by(Species_Codes) %>%
  mutate(New_SS = case_when(
    WCBA_MealsPerMonthSS < WCBA_MealsPerMonthSW ~ "Yes",
    TRUE ~ "No"
  )) 


# FOR SPECIES WITHOUT A STATEWIDE ADVISORY

# Use a new variable called SS_Status to determine whether species will have a new site-specific advisory based
# whether species meal recs  are <= 8 meals/month

Hg_SS6b_1b <- Hg_SS6b_1b %>%
  mutate(New_SS = case_when(
    WCBA_MealsPerMonthSS <= 8 ~ "Yes",
    TRUE ~ "No"
  ))


# Merge all final data frames into one data frame that has a variable New_SS indicating species at waterbodies
# that require issuing a new site-specific advisory

# There should be three data frames merged:
# (1) Data frame for species with existing site-specific advisories
# Hg_SS6a_1

# (2) Data frame for species without existing site-specific advisories but with meal
# recs more stringent than an existing statewide advisory
# Hg_SS6b_1a

# (3) Data frame for species without existing site-specific advisories or statewide advisories but with meal
# recs <= 8 meals/month
# Hg_SS6b_1b

Hg_SS6a_1 <- Hg_SS6a_1 %>% rename(WCBA_MealsPerMonthSS = WCBA_MealsPerMonth )


merged_df <- bind_rows(Hg_SS6a_1, Hg_SS6b_1a, Hg_SS6b_1b)



# Removing duplicates
# There are 7 more observations than there should be due to size categories from the existing site specific 
# data frame 

unique_merged_df<- distinct(merged_df)

Hg_SS_Final <- unique_merged_df %>%
  mutate(WCBA_SS_MealPerMonthNew = coalesce(WCBA_MealsPerMonth, WCBA_MealsPerMonthSS))

