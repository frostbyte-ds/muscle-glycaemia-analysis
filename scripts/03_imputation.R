# -------------------------------- #
# Multiple imputation using Amelia
# -------------------------------- #

# Good amount of missingness for Amelia to handle
colSums(is.na(imputation_vars_1118))

# Setting realistic bounds for continuous variables for imputation
bounds_matrix_1118 <- matrix(
  c(
    which(names(imputation_vars_1118) == "FamIncPov_Ratio"),               0,     5,   # Family income to poverty ratio
    which(names(imputation_vars_1118) == "Bfp_perc"),  5,     80,     # Total Body Fat (%)
    which(names(imputation_vars_1118) == "WaistCircum_cm"), 40,    250,      # Waist Circumference (cm)
    which(names(imputation_vars_1118) == "Hba1c_perc"),    3.5,   20.0,   # HbA1c (%)
    which(names(imputation_vars_1118) == "Height_m"),    1.2,   2.2,    # Height (m)
    which(names(imputation_vars_1118) == "RightArmLean_g"),  500,   15000,  # R Arm Lean (g)
    which(names(imputation_vars_1118) == "LeftArmLean_g"),  500,   15000,  # L Arm Lean (g)
    which(names(imputation_vars_1118) == "RightLegLean_g"),  500,   15000,  # R Leg Lean (g)
    which(names(imputation_vars_1118) == "LeftLegLean_g"),  500,   15000,  # L Leg Lean (g)
    which(names(imputation_vars_1118) == "Phys"),   0,     960,   # MVPA in a typical day (mins)
    which(names(imputation_vars_1118) == "AvgDailyDrinks"), 0,    20,        # Average # of alcoholic drinks per day
    # for the last year
    which(names(imputation_vars_1118) == "HEI"), 0,    100,      # Healthy Eating Index (HEI)
    which(names(imputation_vars_1118) == "AvgNightlySleep"),     2,     15   # Average hours of sleep per night
  ),
  ncol = 3,
  byrow = TRUE
)

# Setting seed for reproducibility
set.seed(123)

# Run Amelia
amelia_out_1118 <- amelia(imputation_vars_1118, m = 30,
                          idvars = c("ID", "PSU", "Strata", "SurvWeight"),
                          noms = c("Gender", "Race", "LifetimeDrinkerFlag", "Smoking_Status"),
                          bounds = bounds_matrix_1118,
                          parallel = "multicore",
                          ncpus  = detectCores() - 1) # Utilises more CPU cores to speed up processing.   

# Feature engineering to form ALMI, alcohol status, and HbA1c outcome variables
# Also removing redundant columns 
imputed_1118 <- lapply(amelia_out_1118$imputations, function(df) {
  df %>%
    # ALMI
    mutate(
      Alm_kg = (RightArmLean_g + LeftArmLean_g + RightLegLean_g + LeftLegLean_g) / 1000,
      Almi = Alm_kg / (Height_m^2),
    ) %>%
    
    # Creating Alcohol_Status variable
    
    # Need to first ensure AvgDailyDrinks is 0 when LifetimeDrinkerFlag is "Never" again.
    # Imputation will have introduced instances where this does not occur due to 
    # missing data in LifetimeDrinkerFlag 
    
    mutate(AvgDailyDrinks = ifelse(LifetimeDrinkerFlag == "Never", 0, AvgDailyDrinks)) %>%
    
    mutate(
      Alcohol_Status = factor(case_when(
        
        # Condition 1: Never Drinkers
        LifetimeDrinkerFlag == "Never" ~ "Never Drinker",
        
        # Condition 2: Former Drinkers
        # They have drunk in their life ("Ever") but their current average is 0
        LifetimeDrinkerFlag == "Ever" & AvgDailyDrinks == 0 ~ "Former Drinker",
        
        # Condition 3: Heavy Drinkers
        Gender == "Female" & AvgDailyDrinks > 1 ~ "Heavy Drinker",
        Gender == "Male" & AvgDailyDrinks > 2 ~ "Heavy Drinker",
        
        # Condition 4: Moderate Drinkers
        # Any remaining person with AvgDailyDrinks > 0 is a moderate drinker
        AvgDailyDrinks > 0 ~ "Moderate Drinker"
        
      ),
      # Set the order of the factor levels for clear regression output,
      # with "Never Drinker" as the clean reference category.
      levels = c("Never Drinker", "Former Drinker", "Moderate Drinker", "Heavy Drinker"))
    ) %>%
    
    mutate(
      # Outcome for Onset models: At Risk or Worse
      # Ensure this is numeric 0/1
      at_risk_or_worse = ifelse(Hba1c_perc >= 5.7, 1, 0),
      
      # Outcome for Progression models: Has Diabetes
      is_diabetic = ifelse(Hba1c_perc >= 6.5, 1, 0)
    ) %>%
    
    select(-c(RightArmLean_g, LeftArmLean_g, RightLegLean_g, LeftLegLean_g, Alm_kg, AvgDailyDrinks,
              LifetimeDrinkerFlag))
})