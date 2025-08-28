# ----------------- #
# Model Diagnostics
# ----------------- #

# --- Archer-Lemeshow Goodness-of-Fit Test --- #

# Goodness-of-fit function 
svy_archer_lemeshow <- function(model, design, groups = 10) {
  residuals <- residuals(model, type = "response")
  fitted_values <- fitted(model)
  
  fitted_groups <- cut(fitted_values,
                       breaks = quantile(fitted_values, probs = seq(0, 1, 1 / groups), na.rm = TRUE),
                       include.lowest = TRUE)
  
  design <- update(design,
                   resids = residuals,
                   fitted_groups = fitted_groups)
  
  gof_model <- svyglm(resids ~ fitted_groups, design = design)
  
  regTermTest(gof_model, ~fitted_groups)
}

# Obtaining a p-value diagnosing the fit of each pooled model
# p > 0.05 indicates good fit

# Onset

# --- Step 1: Organize all un-pooled model lists into a single master list ---

all_onset_model_lists <- list(
  logistic_onset,
  logistic_onset.2,
  logistic_onset.3,
  logistic_onset.4,
  logistic_onset.5,
  logistic_onset.6,
  logistic_onset.7,
  logistic_onset.8,
  logistic_onset.9,
  logistic_onset.10
)

# Create vector of model names
model_names <- paste("Model", 1:10)

# --- Step 2: Initialize an array to store the final p-values ---
onset.gof.pvalue <- c()

# --- Step 3: Loop through each model list, run the test, and store the result ---

for (i in 1:10) {
  
  cat("Processing", model_names[i], "...\n")
  
  # Select the current list of model fits
  onset.model.list <- all_onset_model_lists[[i]]
  
  # Initialize an empty vector for F-statistics for the current model
  onset.f.statistics <- c()
  
  # Loop through each imputation for the current model
  for (j in 1:length(onset.model.list)) {
    current_model <- onset.model.list[[j]]
    current_design <- design_list[[j]]
    
    # Run the goodness-of-fit test
    gof_test_result <- svy_archer_lemeshow(current_model, current_design)
    
    # Extract and store the F-statistic
    onset.f.statistics[j] <- gof_test_result$Ftest
  }
  
  # Numerator degrees of freedom (df1)
  df1 <- gof_test_result$df
  
  # Pool the F-statistics for the current model
  pooled_gof_test <- micombine.F(onset.f.statistics, df1 = df1)
  
  # Extract the final pooled p-value
  final_p_value <- pooled_gof_test[2]
  
  # Add the result to array
  onset.gof.pvalue[i] = final_p_value
  
}

# Progression

# --- Step 1: Organize all unpooled model lists into a single master list ---

all_prog_model_lists <- list(
  logistic_progression,
  logistic_progression.2,
  logistic_progression.3,
  logistic_progression.4,
  logistic_progression.5,
  logistic_progression.6,
  logistic_progression.7,
  logistic_progression.8,
  logistic_progression.9,
  logistic_progression.10
)

# --- Step 2: Initialize an array to store the final p-values ---
prog.gof.pvalue <- c()

# --- Step 3: Loop through each model list, run the test, and store the result ---

for (i in 1:10) {
  
  cat("Processing", model_names[i], "...\n")
  
  # Select the current list of model fits
  prog.model.list <- all_prog_model_lists[[i]]
  
  # Initialize an empty vector for F-statistics for the current model
  prog.f.statistics <- c()
  
  # Loop through each imputation for the current model
  for (j in 1:length(prog.model.list)) {
    current_model <- prog.model.list[[j]]
    current_design <- design_list[[j]]
    
    # Run the goodness-of-fit test
    gof_test_result <- svy_archer_lemeshow(current_model, current_design)
    
    # Extract and store the F-statistic
    prog.f.statistics[j] <- gof_test_result$Ftest
  }
  
  # Numerator degrees of freedom (df1)
  df1 <- gof_test_result$df
  
  # Pool the F-statistics for the current model
  pooled_gof_test <- micombine.F(prog.f.statistics, df1 = df1)
  
  # Extract the final pooled p-value
  final_p_value <- pooled_gof_test[2]
  
  # Add the result to array
  prog.gof.pvalue[i] = final_p_value
  
}

# --- Creating lists of mean McFadden Pseudo R² values for each pooled model --- #

# Onset

# Function for finding the mean McFadden Pseudo R² from list of models
mean_fit_stat <- function(model_list) {
  # Apply fit.svyglm to each model
  fit_results <- lapply(model_list, fit.svyglm)
  
  # Extract the first element from each result
  fit_values <- sapply(fit_results, function(x) x[1])
  
  # Compute and return the mean
  mean(fit_values)
}

# Create the names of the 9 lists as strings
onset.model.names <- c("logistic_onset", paste0("logistic_onset.", 2:10))

# Apply mean_fit_stat to each model list by name
onset.pseudo.r2.list <- sapply(onset.model.names, function(name) {
  model_list <- get(name)
  mean_fit_stat(model_list)
})

# Show results
print(onset.pseudo.r2.list)

# Progression

# Create the names of the 9 lists as strings
prog.model.names <- c("logistic_progression", paste0("logistic_progression.", 2:10))

# Apply mean_fit_stat to each model list by name
prog.pseudo.r2.list <- sapply(prog.model.names, function(name) {
  model_list <- get(name)
  mean_fit_stat(model_list)
})

# Show results
print(prog.pseudo.r2.list)

# Removing array element names for each pseudo R^2 list
names(onset.pseudo.r2.list) <- NULL
names(prog.pseudo.r2.list) <- NULL

# The computed diagnostic statistics are compiled into concise summary tables in script 7 (summaries)

# --- Residual Diagnostics --- #

# The response variable is binary.
# The residuals are not expected to be normally distributed.
# Survey-weighted estimation can distort residual behaviour.

# binnedplot in the arm package is ideal for this case

# Seek to check binned residuals of the fully adjusted onset model and parsimonious progression model
# for all imputations to assess model fit and if there are any significant
# differences between imputed datasets

# Will only include imputation 1 in thesis to save space

# Onset

# Loop through the imputations for the onset model
for (i in 1:30) {
  
  model_fit <- logistic_onset.10[[i]]
  
  binnedplot(
    fitted(model_fit),
    residuals(model_fit, type = "response"),
    nclass = NULL,
    xlab = "Expected Values",
    ylab = "Average residual",
    main = paste("Onset Model - Imputation", i), # Add a dynamic title
    cex.pts = 0.8,
    col.pts = 1,
    col.int = "gray"
  )
}

# Progression

# Loop through the imputations for the progression model
for (i in 1:30) {
  
  model_fit <- logistic_progression.4[[i]]
  
  binnedplot(
    fitted(model_fit),
    residuals(model_fit, type = "response"),
    nclass = NULL,
    xlab = "Expected Values",
    ylab = "Average residual",
    main = paste("Progression Model - Imputation", i), 
    cex.pts = 0.8,
    col.pts = 1,
    col.int = "gray"
  )
}

# Residuals fine for onset model - some patterns evident in progression model
# No significant differences between imputations

# Now creating binned component + residual plots to justify the choice of linear
# covariates in both models

# Only using imputation 1 here

# Onset

# Get the working residuals and the specific predictor
onset.model.fit <- logistic_onset.10$imp1
onset.y.outcome <- onset.model.fit$survey.design$variables$at_risk_or_worse
onset.mu.fitted <- fitted(onset.model.fit)
onset.working.residuals <- (onset.y.outcome - onset.mu.fitted) / onset.mu.fitted

onset.almi.data <- onset.model.fit$survey.design$variables$Almi
onset.age.data <- onset.model.fit$survey.design$variables$Age_yrs
onset.bfp.data <- onset.model.fit$survey.design$variables$Bfp_perc
onset.waistcircum.data <- onset.model.fit$survey.design$variables$WaistCircum_cm
onset.pir.data <- onset.model.fit$survey.design$variables$FamIncPov_Ratio
onset.phys.data <- onset.model.fit$survey.design$variables$Phys
onset.hei.data <- onset.model.fit$survey.design$variables$HEI
onset.sleep.data <- onset.model.fit$survey.design$variables$AvgNightlySleep

# Create the binned plot of residuals vs. the predictor
par(mfrow = c(2, 4))
binnedplot(x = onset.almi.data, y = onset.working.residuals,
           xlab = "ALMI (kg/m²)",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. ALMI")

binnedplot(x = onset.age.data, y = onset.working.residuals,
           xlab = "Age (years)",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. Age")

binnedplot(x = onset.bfp.data, y = onset.working.residuals,
           xlab = "Body Fat Percentage (%)",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. Body Fat Percentage")

binnedplot(x = onset.waistcircum.data, y = onset.working.residuals,
           xlab = "Waist Circumference (cm)",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. Waist Circumference")

binnedplot(x = onset.pir.data, y = onset.working.residuals,
           xlab = "Poverty Income Ratio",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. Poverty Income Ratio")

binnedplot(x = onset.phys.data, y = onset.working.residuals,
           xlab = "Physical Activity (mins/day)",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. Physical Activity")

binnedplot(x = onset.hei.data, y = onset.working.residuals,
           xlab = "Healthy Eating Index",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. Healthy Eating Index")

binnedplot(x = onset.sleep.data, y = onset.working.residuals,
           xlab = "Average Nightly Sleep (hrs)",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. Average Nightly Sleep")

par(mfrow = c(2, 2))

# Prog
prog.model.fit <- logistic_progression.4$imp1
prog.y.outcome <- prog.model.fit$survey.design$variables$is_diabetic
prog.mu.fitted <- fitted(prog.model.fit)
prog.working.residuals <- (prog.y.outcome - prog.mu.fitted) / prog.mu.fitted

prog.almi.data <- prog.model.fit$survey.design$variables$Almi
prog.age.data <- prog.model.fit$survey.design$variables$Age_yrs
prog.bfp.data <- prog.model.fit$survey.design$variables$Bfp_perc
prog.waistcircum.data <- prog.model.fit$survey.design$variables$WaistCircum_cm

# Plotting 

binnedplot(x = prog.almi.data, y = prog.working.residuals,
           xlab = "ALMI (kg/m²)",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. ALMI")

binnedplot(x = prog.age.data, y = prog.working.residuals,
           xlab = "Age (years)",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. Age")

binnedplot(x = prog.bfp.data, y = prog.working.residuals,
           xlab = "Body Fat Percentage (%)",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. Body Fat Percentage")

binnedplot(x = prog.waistcircum.data, y = prog.working.residuals,
           xlab = "Waist Circumference (cm)",
           ylab = "Average Working Residual",
           main = "Binned Residuals vs. Waist Circumference")

par(mfrow = c(1, 1))

# No evidence that higher order terms are needed

# ---- Checking for influential points ---- #

# Using survey weighted cook's distance plots

# Onset

# Onset model 4 shows most cooks distances cluster below 10
# It is useful to investigate the outlier observations and how the model performs without them
# We do not want conclusions to depend on a small amount of influential points

# Finding and plotting cooks distances for onset model 10 on imputation 1
# Crucially, the results are very similar on other imputations, so only considering imputation 1 here is okay
# Earlier residual checks also showed no significant differences between imputations
onset.cooks <- svyCooksD(logistic_onset.10$imp1, doplot=TRUE)
# Saving indices of those points with the top 1% cooks distance
onset_cooks_threshold <- quantile(onset.cooks, probs = 0.99)
onset_influential_indices <- which(onset.cooks > onset_cooks_threshold)

# Creating a data frame with these influential observations
# Nothing immediately stands out with these
onset_influential_obs <- design_list$imp1$variables[onset_influential_indices,]
onset_influential_obs$CooksD <- onset.cooks[onset_influential_indices]

# Seek to refit the model without these points

# Create a new data frame by EXCLUDING the influential rows
onset_reduced_data <- design_list$imp1$variables[-onset_influential_indices, ]

# Create a new survey design object with this reduced dataset
onset_reduced_design <- svydesign(id = ~PSU, 
                                  strata = ~Strata, 
                                  weights = ~SurvWeight,
                                  nest = TRUE,
                                  data = onset_reduced_data)

# Refit the model using the reduced design object
onset_without_influential <- svyglm(at_risk_or_worse ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm,
                                    design = onset_reduced_design, 
                                    family = quasibinomial)

# Almi coefficient is still not significant
# This means that removing influential points does not change the findings
summary(onset_without_influential)

# Now checking this for progression model 4

prog.cooks <- svyCooksD(logistic_progression.4$imp1, doplot=TRUE)
# Saving indices of those points with the top 1% cooks distance
prog_cooks_threshold <- quantile(prog.cooks, probs = 0.99)
prog_influential_indices <- which(prog.cooks > prog_cooks_threshold)

# Creating a data frame with these influential observations
# Nothing immediately stands out
prog_influential_obs <- design_list$imp1$variables[prog_influential_indices,]
prog_influential_obs$CooksD <- prog.cooks[prog_influential_indices]

# Refit the model without these points

# Create a new data frame without the influential rows
prog_reduced_data <- design_list$imp1$variables[-prog_influential_indices, ]

# Create a new survey design object with this reduced dataset
prog_reduced_design <- svydesign(id = ~PSU, 
                                 strata = ~Strata, 
                                 weights = ~SurvWeight,
                                 nest = TRUE,
                                 data = prog_reduced_data)

# Refit the model using the reduced design object
prog_without_influential <- svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm,
                                   design = prog_reduced_design, 
                                   family = quasibinomial)

# Once again, nothing has significantly changed.
# Can conclude that findings are robust and not dependant on influential outliers.
summary(prog_without_influential)