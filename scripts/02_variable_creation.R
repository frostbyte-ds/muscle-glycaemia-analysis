# ----------------------------------------------------------------------------------------------- #
# Combining the various survey cycles and carrying feature engineering and variable harmonisation
# ----------------------------------------------------------------------------------------------- #

# Helper function for inner join of multiple datasets
multi_inner_join <- function(dfs) {
  reduce(dfs, ~inner_join(.x, .y, by = "SEQN"))
}

# Create lists of datasets for each time period
dfs1112 <- list(demo1112, body1112, HbA1c1112, dexa1112, phys1112, alcohol1112,
                smoking1112, hei1112, sleep1112)

dfs1314 <- list(demo1314, body1314, HbA1c1314, dexa1314, phys1314, alcohol1314,
                smoking1314, hei1314, sleep1314)

dfs1516 <- list(demo1516, body1516, HbA1c1516, dexa1516, phys1516, alcohol1516,
                smoking1516, hei1516, sleep1516)

dfs1718 <- list(demo1718, body1718, HbA1c1718, dexa1718, phys1718, alcohol1718,
                smoking1718, hei1718, sleep1718)

# Applying function and adding variable denoting time period to aid with variable harmonisation
xsec.data1112 <- multi_inner_join(dfs1112) %>%
  mutate(SurveyCycle = "2011-2012")
xsec.data1314 <- multi_inner_join(dfs1314) %>%
  mutate(SurveyCycle = "2013-2014")
xsec.data1516 <- multi_inner_join(dfs1516) %>%
  mutate(SurveyCycle = "2015-2016")
xsec.data1718 <- multi_inner_join(dfs1718) %>%
  mutate(SurveyCycle = "2017-2018")

# Array of variables needed from each time period
vars1112 <- c(
  "SEQN", "SDMVPSU", "SDMVSTRA", "WTMEC2YR", "RIAGENDR", "RIDAGEYR", "RIDRETH3", "INDFMPIR",  # Survey structure and
  # demographics
  "BMXHT", "BMXWAIST",  # Body measures
  "LBXGH",  # HbA1c
  "DXDRALE", "DXDLALE", "DXDRLLE", "DXDLLLE", "DXDTOPF",  # Lean mass in the limbs excl. bone mass
  "PAQ620", "PAD630", "PAQ665", "PAD675", "PAQ605", "PAD615", "PAQ650", "PAD660", "PAQ635", "PAD645",  # Physical activity - now including vig & mod work activity and travel activity
  "ALQ120Q", "ALQ130", "ALQ101", "ALQ110",  # Alcohol consumption
  "SMQ040", "SMQ020",  # Smoking habits
  "score",  # Healthy Eating Index score
  "SLD010H",  # Sleep 
  "SurveyCycle"  # Marker for survey years
)

vars1314 <- c(
  "SEQN", "SDMVPSU", "SDMVSTRA", "WTMEC2YR", "RIAGENDR", "RIDAGEYR", "RIDRETH3", "INDFMPIR",
  "BMXHT", "BMXWAIST",
  "LBXGH",
  "DXDRALE", "DXDLALE", "DXDRLLE", "DXDLLLE", "DXDTOPF",
  "PAQ620", "PAD630", "PAQ665", "PAD675", "PAQ605", "PAD615", "PAQ650", "PAD660", "PAQ635", "PAD645",
  "ALQ120Q", "ALQ130", "ALQ101", "ALQ110",
  "SMQ040", "SMQ020",
  "score",
  "SLD010H",
  "SurveyCycle"
)

vars1516 <- c(
  "SEQN", "SDMVPSU", "SDMVSTRA", "WTMEC2YR", "RIAGENDR", "RIDAGEYR", "RIDRETH3", "INDFMPIR",
  "BMXHT", "BMXWAIST",
  "LBXGH",
  "DXDRALE", "DXDLALE", "DXDRLLE", "DXDLLLE", "DXDTOPF",
  "PAQ620", "PAD630", "PAQ665", "PAD675", "PAQ605", "PAD615", "PAQ650", "PAD660", "PAQ635", "PAD645",
  "ALQ120Q", "ALQ130", "ALQ101", "ALQ110",
  "SMQ040", "SMQ020",
  "score",
  "SLD012",
  "SurveyCycle"
)

vars1718 <- c(
  "SEQN", "SDMVPSU", "SDMVSTRA", "WTMEC2YR", "RIAGENDR", "RIDAGEYR", "RIDRETH3", "INDFMPIR",
  "BMXHT", "BMXWAIST",
  "LBXGH",
  "DXDRALE", "DXDLALE", "DXDRLLE", "DXDLLLE", "DXDTOPF",
  "PAQ620", "PAD630", "PAQ665", "PAD675", "PAQ605", "PAD615", "PAQ650", "PAD660", "PAQ635", "PAD645",
  "ALQ121", "ALQ130", "ALQ111",
  "SMQ040", "SMQ020",
  "score",
  "SLD012", "SLD013",
  "SurveyCycle"
)

# Extracting variables for each time period
imputation_vars1112 <- xsec.data1112 %>% select(all_of(vars1112))
imputation_vars1314 <- xsec.data1314 %>% select(all_of(vars1314))
imputation_vars1516 <- xsec.data1516 %>% select(all_of(vars1516))
imputation_vars1718 <- xsec.data1718 %>% select(all_of(vars1718))

# Combining
imputation_vars_full <- bind_rows(
  imputation_vars1112,
  imputation_vars1314,
  imputation_vars1516,
  imputation_vars1718
)

# Need to set PAD675 to 0 where PAQ665 is "No" AND set PAD660 to 0 where PAQ650 is "No".
# Also need to set PAD675 and PAD660 to NA whenever they are 9999

# Now also doing so for variables relating to work-related physical activity & walking/cycling for travel
# Aim to create a total daily physical activity variable (work + recreational)

# Proportion missing of minutes of moderate and vigorous physical activity
sum(is.na(imputation_vars_full$PAD660))/nrow(imputation_vars_full) # 69% missing
sum(is.na(imputation_vars_full$PAD675))/nrow(imputation_vars_full) # 56% missing
sum(is.na(imputation_vars_full$PAD615))/nrow(imputation_vars_full) # 77% missing
sum(is.na(imputation_vars_full$PAD630))/nrow(imputation_vars_full) # 60% missing
sum(is.na(imputation_vars_full$PAD645))/nrow(imputation_vars_full) # 71% missing

imputation_vars_full <- imputation_vars_full %>%
  mutate(
    # For Vigorous Recreational Activity (PAD660)
    # If PAQ650 is 'No' AND PAD660 is currently NA, change it to 0.
    # Otherwise, leave it as it is.
    PAD660 = ifelse(PAQ650 == "No" & is.na(PAD660), 0, PAD660),
    
    # Assigning NA for "Don't know" code
    PAD660 = ifelse(PAD660 == 9999, NA_real_, PAD660),
    
    # For Vigorous Work Activity (PAD615)
    # If PAQ605 is 'No' AND PAD615 is currently NA, change it to 0.
    # Otherwise, leave it as it is.
    PAD615 = ifelse(PAQ605 == "No" & is.na(PAD615), 0, PAD615),
    
    # Assigning NA for "Don't know" code
    PAD615 = ifelse(PAD615 == 9999, NA_real_, PAD615),
    
    # For Moderate Recreational Activity (PAD675)
    # If PAQ665 is 'No' AND PAD675 is currently NA, change it to 0.
    # Otherwise, leave it as it is.
    PAD675 = ifelse(PAQ665 == "No" & is.na(PAD675), 0, PAD675),
    
    # Assigning NA for "Don't know" code 
    PAD675 = ifelse(PAD675 == 9999, NA_real_, PAD675),
    
    # For Moderate Work Activity (PAD630)
    # If PAQ620 is 'No' AND PAD630 is currently NA, change it to 0.
    # Otherwise, leave it as it is.
    PAD630 = ifelse(PAQ620 == "No" & is.na(PAD630), 0, PAD630),
    
    # Assigning NA for "Don't know" code 
    PAD630 = ifelse(PAD630 == 9999, NA_real_, PAD630),
    
    # For Travel Activity (PAD645)
    # If PAQ635 is 'No' AND PAD645 is currently NA, change it to 0.
    # Otherwise, leave it as it is.
    PAD645 = ifelse(PAQ635 == "No" & is.na(PAD645), 0, PAD645),
    
    # Assigning NA for "Don't know" code 
    PAD645 = ifelse(PAD645 == 9999, NA_real_, PAD645)
    
  ) %>%
  select(-c(PAQ665, PAQ650, PAQ605, PAQ620, PAQ635)) # Deselect gatekeeper question variables

# Proportion missing of minutes of physical activity variables after replacement

sum(is.na(imputation_vars_full$PAD660))/nrow(imputation_vars_full) # 0.04% missing
sum(is.na(imputation_vars_full$PAD675))/nrow(imputation_vars_full) # 0.09% missing
sum(is.na(imputation_vars_full$PAD615))/nrow(imputation_vars_full) # 0.2% missing
sum(is.na(imputation_vars_full$PAD630))/nrow(imputation_vars_full) # 0.3% missing
sum(is.na(imputation_vars_full$PAD645))/nrow(imputation_vars_full) # 0.2% missing

# ---- Cleaning alcohol variables and managing sick quitter effect ---- #

imputation_vars_full.2 <- imputation_vars_full %>%
  
  # --- Step 1: Harmonise the two different frequency variables into one ---
  mutate(
    Drinking_Days_Per_Year = case_when(
      
      # For older cycles, use the direct numeric value from ALQ120Q
      SurveyCycle %in% c("2011-2012", "2013-2014", "2015-2016") & ALQ120Q %in% c(777, 999) ~ NA_real_,
      SurveyCycle %in% c("2011-2012", "2013-2014", "2015-2016") ~ ALQ120Q,
      
      # For the 2017-2018 cycle, convert the categories from ALQ121
      SurveyCycle == "2017-2018" ~ case_when(
        ALQ121 %in% c(77, 99) ~ NA_real_,
        ALQ121 == "Never in the last year" ~ 0,      # Never
        ALQ121 == "Every day" ~ 365,    # Every day
        ALQ121 == "Nearly every day" ~ 312,    # Nearly every day (~6/wk)
        ALQ121 == "3 to 4 times a week" ~ 182,    # 3-4 times a week (~3.5/wk)
        ALQ121 == "2 times a week" ~ 104,    # 2 times a week
        ALQ121 == "Once a week" ~ 52,     # Once a week
        ALQ121 == "2 to 3 times a month" ~ 30,     # 2-3 times a month (~2.5/mo)
        ALQ121 == "Once a month" ~ 12,     # Once a month
        ALQ121 == "7 to 11 times in the last year" ~ 9,      # 7-11 times a year (avg 9)
        ALQ121 == "3 to 6 times in the last year" ~ 4.5,    # 3-6 times a year (avg 4.5)
        ALQ121 == "1 to 2 times in the last year" ~ 1.5,   # 1-2 times a year (avg 1.5)
        TRUE ~ NA_real_
      )
    )
  ) %>%
  
  # --- Step 2: Clean the "drinks per day" variable (ALQ130) ---
  mutate(
    # First, handle the special codes for Refused/Don't Know
    Clean_ALQ130 = case_when(
      ALQ130 %in% c(777, 999) ~ NA_real_, 
      TRUE ~ ALQ130
    ),
    
    # Now, use  harmonised variable to set non-drinkers to 0
    # handles the structural missingness correctly
    Clean_ALQ130 = ifelse(Drinking_Days_Per_Year == 0, 0, Clean_ALQ130)
  ) %>%
  
  # --- Step 3: Calculate the final average daily intake ---
  mutate(
    AvgDailyDrinks = (Clean_ALQ130 * Drinking_Days_Per_Year) / 365
  ) %>%
  
  # --- Step 4: Cleaning ALQ110 and creating the single LifetimeDrinkerFlag ---
  mutate(
    Clean_ALQ110 = case_when(
      # If they are a current regular drinker (ALQ101 == 1), they must have drunk in their lifetime.
      SurveyCycle %in% c("2011-2012", "2013-2014", "2015-2016") & ALQ101 == "Yes" & is.na(ALQ110) ~ "Yes",
      ALQ110 %in% c(7, 9) ~ NA_character_, 
      # Otherwise, keep the original ALQ110 value for now
      SurveyCycle %in% c("2011-2012", "2013-2014", "2015-2016") ~ as.character(ALQ110),
      TRUE ~ NA_character_ # This column doesn't exist in the 2017-2018 cycle
    )
  ) %>%
  
  mutate(
    LifetimeDrinkerFlag = case_when(
      SurveyCycle %in% c("2011-2012", "2013-2014", "2015-2016") & Clean_ALQ110 == "No" ~ "Never",
      SurveyCycle %in% c("2011-2012", "2013-2014", "2015-2016") & Clean_ALQ110 == "Yes" ~ "Ever",
      
      SurveyCycle == "2017-2018" & ALQ111 == "No" ~ "Never",
      SurveyCycle == "2017-2018" & ALQ111 == "Yes" ~ "Ever",
      
      # Handle any remaining special codes
      ALQ111 %in% c(7, 9) ~ NA_character_,
      
      TRUE ~ NA_character_
    )
  ) %>%
  
  # --- Step 5: Correct AvgDailyDrinks for Never drinkers ---
  mutate(
    AvgDailyDrinks = ifelse(LifetimeDrinkerFlag == "Never", 0, AvgDailyDrinks),
    
    # Convert the character column to a factor and set the levels
    LifetimeDrinkerFlag = factor(LifetimeDrinkerFlag, 
                                 levels = c("Never", "Ever"))
  ) %>%
  
  # --- Step 6: Final selection ---
  select(-c("ALQ120Q", "ALQ130", "ALQ101", "ALQ110", "ALQ121", "ALQ111", "Drinking_Days_Per_Year", "Clean_ALQ110",
            "Clean_ALQ130"))


# ---- Cleaning smoking variables ---- #

imputation_vars_full.3 <- imputation_vars_full.2 %>%
  mutate(
    Smoking_Status = factor(case_when(
      
      # Condition 1: If they answered "No" to ever smoking 100 cigarettes, they are a Never Smoker.
      SMQ020 == "No" ~ "Never Smoker",
      
      # Condition 2: If they HAVE smoked 100 cigarettes (SMQ020 == 1),
      # then look at their answer to SMQ040.
      SMQ020 == "Yes" ~ case_when(
        # If they smoke "Every day" or "Some days", they are a Current Smoker.
        SMQ040 %in% c("Every day", "Some days") ~ "Current Smoker",
        # If they answered "Not at all", they are a Former Smoker.
        SMQ040 == "Not at all" ~ "Former Smoker",
        # Handle any remaining special codes (like Refused/Don't Know) as NA
        TRUE ~ NA_character_
      ),
      
      # A fallback for any other case (e.g., if SMQ020 itself is missing, refused or don't know)
      TRUE ~ NA_character_
    ),
    # Set the order of the factor levels for clear regression output
    levels = c("Never Smoker", "Former Smoker", "Current Smoker"))
  ) %>%
  select(-c("SMQ020", "SMQ040"))

# ---- Cleaning sleep data ---- #

imputation_vars_full.4 <- imputation_vars_full.3 %>%
  mutate(
    # Create a single, harmonised sleep variable in hours
    AvgNightlySleep = case_when(
      
      # For 2011-2014 cycles, use SLD010H
      SurveyCycle %in% c("2011-2012", "2013-2014") & SLD010H %in% c(77, 99) ~ NA_real_,
      SurveyCycle %in% c("2011-2012", "2013-2014") ~ SLD010H, # Treat the top-code of 12 as 12
      
      # For the 2015-2016 cycle, use SLD012
      SurveyCycle == "2015-2016" & SLD012 > 24 ~ NA_real_, # General plausibility check
      SurveyCycle == "2015-2016" ~ SLD012,
      
      # For the 2017-2018 cycle, calculate the weighted average
      SurveyCycle == "2017-2018" ~ {
        # Handle the special codes first
        weekday_sleep <- ifelse(SLD012 > 24, NA, SLD012)
        weekend_sleep <- ifelse(SLD013 > 24, NA, SLD013)
        
        # Calculate the weighted average
        ((weekday_sleep * 5) + (weekend_sleep * 2)) / 7
      },
      
      # A fall-back for any other case
      TRUE ~ NA_real_
    )
  ) %>%
  select(-c("SLD010H", "SLD012", "SLD013", "SurveyCycle"))

# Final adjustments pre-imputation
imputation_vars_1118 <- imputation_vars_full.4 %>%
  mutate(Phys = PAD660 + PAD675 + PAD630 + PAD615,
         Height_m = BMXHT / 100) %>%
  select(-c(BMXHT, PAD660, PAD675)) %>%
  rename(
    ID = SEQN,
    PSU = SDMVPSU,
    Strata = SDMVSTRA,
    SurvWeight = WTMEC2YR,
    Gender = RIAGENDR,
    Age_yrs = RIDAGEYR,
    Race = RIDRETH3,
    FamIncPov_Ratio =INDFMPIR,
    Bfp_perc = DXDTOPF,
    WaistCircum_cm = BMXWAIST,
    Hba1c_perc = LBXGH,
    RightArmLean_g = DXDRALE,
    LeftArmLean_g = DXDLALE,
    RightLegLean_g = DXDRLLE,
    LeftLegLean_g = DXDLLLE,
    HEI = score
  )
