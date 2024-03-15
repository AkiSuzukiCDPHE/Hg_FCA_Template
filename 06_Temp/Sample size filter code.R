
# Filter the data frame for observations that have a sufficient sample size. 
threshold <- 0  # Value from power analysis at 80% power, 40% detectable difference, and alpha.05


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

Hg_GP_SS_InsufficientSample=Hg_SS4b %>%
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

