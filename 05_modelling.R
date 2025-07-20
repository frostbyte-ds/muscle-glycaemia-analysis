# -------------------------------------------------- #
# Model Fitting with Stepwise Addition of Covariates
# -------------------------------------------------- #

# Survey design list 
design_list <- lapply(imputed_1118, function(dataset) {
  svydesign(
    id = ~PSU,
    strata = ~Strata,
    weights = ~SurvWeight,
    nest = TRUE,
    data = dataset
  )
})

# Normal vs Elevated - Onset

# at_risk_or_worse ~ Almi
logistic_onset <- lapply(design_list, function(design_obj) {
  svyglm(at_risk_or_worse ~ Almi,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_onset <- MIcombine(logistic_onset)
summary(pooled_logistic_onset)

# at_risk_or_worse ~ Almi + Age_yrs + Gender + Race
logistic_onset.2 <- lapply(design_list, function(design_obj) {
  svyglm(at_risk_or_worse ~ Almi + Age_yrs + Gender + Race,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_onset.2 <- MIcombine(logistic_onset.2)
summary(pooled_logistic_onset.2)

# at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc
logistic_onset.3 <- lapply(design_list, function(design_obj) {
  svyglm(at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_onset.3 <- MIcombine(logistic_onset.3)
summary(pooled_logistic_onset.3)

# First model with this FamIncPov_Ratio

# at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm
logistic_onset.4 <- lapply(design_list, function(design_obj) {
  svyglm(at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_onset.4 <- MIcombine(logistic_onset.4)
summary(pooled_logistic_onset.4)

# at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio
logistic_onset.5 <- lapply(design_list, function(design_obj) {
  svyglm(at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_onset.5 <- MIcombine(logistic_onset.5)
summary(pooled_logistic_onset.5)

# at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys
logistic_onset.6 <- lapply(design_list, function(design_obj) {
  svyglm(at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_onset.6 <- MIcombine(logistic_onset.6)
summary(pooled_logistic_onset.6)

# at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI
logistic_onset.7 <- lapply(design_list, function(design_obj) {
  svyglm(at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_onset.7 <- MIcombine(logistic_onset.7)
summary(pooled_logistic_onset.7)

# at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Alcohol_Status
logistic_onset.8 <- lapply(design_list, function(design_obj) {
  svyglm(at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Alcohol_Status,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_onset.8 <- MIcombine(logistic_onset.8)
summary(pooled_logistic_onset.8)

# Alcohol status behaves counter-intuitively and does not appear to be a strong confounder so it is safe to remove

# at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Smoking_Status
logistic_onset.9 <- lapply(design_list, function(design_obj) {
  svyglm(at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Smoking_Status,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_onset.9 <- MIcombine(logistic_onset.9)
summary(pooled_logistic_onset.9)

# Smoking status variable behaves as expected - no strange issues like the alcohol variable

# at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Smoking_Status + AvgNightlySleep
logistic_onset.10 <- lapply(design_list, function(design_obj) {
  svyglm(at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Smoking_Status + AvgNightlySleep,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_onset.10 <- MIcombine(logistic_onset.10)
summary(pooled_logistic_onset.10)

# Almi not significant when moving from normal to elevated hba1c.

# Non-Diabetic vs Diabetic - Progression

# is_diabetic ~ Almi
logistic_progression <- lapply(design_list, function(design_obj) {
  svyglm(is_diabetic ~ Almi,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_progression <- MIcombine(logistic_progression)
summary(pooled_logistic_progression)

# is_diabetic ~ Almi + Age_yrs + Gender + Race
logistic_progression.2 <- lapply(design_list, function(design_obj) {
  svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_progression.2 <- MIcombine(logistic_progression.2)
summary(pooled_logistic_progression.2)

# is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc
logistic_progression.3 <- lapply(design_list, function(design_obj) {
  svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_progression.3 <- MIcombine(logistic_progression.3)
summary(pooled_logistic_progression.3)

# is_diabetic ~ Almi + Age_yrs + Gender + Race  + Bfp_perc + WaistCircum_cm
logistic_progression.4 <- lapply(design_list, function(design_obj) {
  svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_progression.4 <- MIcombine(logistic_progression.4)
summary(pooled_logistic_progression.4)

# is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio
logistic_progression.5 <- lapply(design_list, function(design_obj) {
  svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_progression.5 <- MIcombine(logistic_progression.5)
summary(pooled_logistic_progression.5)

# is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys
logistic_progression.6 <- lapply(design_list, function(design_obj) {
  svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_progression.6 <- MIcombine(logistic_progression.6)
summary(pooled_logistic_progression.6)

# is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI
logistic_progression.7 <- lapply(design_list, function(design_obj) {
  svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_progression.7 <- MIcombine(logistic_progression.7)
summary(pooled_logistic_progression.7)

# is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + Phys + HEI + Alcohol_Status
logistic_progression.8 <- lapply(design_list, function(design_obj) {
  svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Alcohol_Status,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_progression.8 <- MIcombine(logistic_progression.8)
summary(pooled_logistic_progression.8)

# is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Smoking_Status
logistic_progression.9 <- lapply(design_list, function(design_obj) {
  svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Smoking_Status,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_progression.9 <- MIcombine(logistic_progression.9)
summary(pooled_logistic_progression.9)

# is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Smoking_Status + AvgNightlySleep
logistic_progression.10 <- lapply(design_list, function(design_obj) {
  svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm + FamIncPov_Ratio + Phys + HEI + Smoking_Status + AvgNightlySleep,
         design = design_obj, family = quasibinomial)
})
pooled_logistic_progression.10 <- MIcombine(logistic_progression.10)
summary(pooled_logistic_progression.10)