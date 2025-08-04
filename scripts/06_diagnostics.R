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

# --- Step 1: Organize all unpooled model lists into a single master list ---

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

# ---- Diagnostics tables ---- #

# Array of covariate changes for diagnostic and summary tables
covariates <- c("Crude", "+ Demographics", "+ Body Fat Percentage (%)", "+ Waist Circumference (cm)",
                "+ Ratio of Family Income to Poverty", "+ Physical Activity (mins)", "+ Healthy Eating Index", "+ Alcohol Status",
                "+ Smoking Status", "+ Average Daily Sleep (hrs)")

# Onset

# Removing array element names
names(onset.pseudo.r2.list) <- NULL

# Data frame
onset.diag.df <- data.frame(
  Model = paste("Model", 1:10),
  `Covariate Changes` = covariates,
  `McFadden Pseudo R²` = onset.pseudo.r2.list,
  `Archer-Lemeshow GoF Test P-value` = onset.gof.pvalue,
  check.names = FALSE
)

# Creating table
onset.diag.table <- onset.diag.df %>%
  gt() %>%
  
  # Format numeric columns nicely
  fmt_number(
    columns = c(`McFadden Pseudo R²`),
    decimals = 3
  ) %>%
  
  # Format p-values in scientific notation
  fmt_number(
    columns = `Archer-Lemeshow GoF Test P-value`,
    decimals = 3
  ) %>%
  
  # Align columns (right align numbers, left align text)
  cols_align(
    align = "left",
    columns = c(Model, `Covariate Changes`)
  ) %>%
  cols_align(
    align = "right",
    columns = c(`McFadden Pseudo R²`, `Archer-Lemeshow GoF Test P-value`)
  ) %>%
  
  # Add a title and subtitle (optional)
  tab_header(
    title = "Diagnostics of Logistic Regression Onset Models",
    subtitle = "McFadden Pseudo R² and Archer-Lemeshow GoF Test P-value"
  ) 

# Print the table
onset.diag.table

# Prog

# Removing array element names
names(prog.pseudo.r2.list) <- NULL

# Data frame
prog.diag.df <- data.frame(
  Model = paste("Model", 1:10),
  `Covariate Changes` = covariates,
  `McFadden Pseudo R²` = prog.pseudo.r2.list,
  `Archer-Lemeshow GoF Test P-value` = prog.gof.pvalue,
  check.names = FALSE
)

# Creating table
prog.diag.table <- prog.diag.df %>%
  gt() %>%
  
  # Format numeric columns nicely
  fmt_number(
    columns = c(`McFadden Pseudo R²`, `Archer-Lemeshow GoF Test P-value`),
    decimals = 3
  ) %>%
  
  # Align columns (right align numbers, left align text)
  cols_align(
    align = "left",
    columns = c(Model, `Covariate Changes`)
  ) %>%
  cols_align(
    align = "right",
    columns = c(`McFadden Pseudo R²`, `Archer-Lemeshow GoF Test P-value`)
  ) %>%
  
  # Add a title and subtitle (optional)
  tab_header(
    title = "Diagnostics of Logistic Regression Progression Models",
    subtitle = "McFadden Pseudo R² and Archer-Lemeshow GoF Test P-value"
  ) 

# Print the table
prog.diag.table

# --- Residual Diagnostics --- #

# The response variable is binary.
# The residuals are not expected to be normally distributed.
# Survey-weighted estimation can distort residual behavior.

# binnedplot in the arm package is ideal for this case

# Seek to plot binned residuals of the parsimonious onset and progression models
# for the first 9 imputations to assess model fit and if there are any significant
# differences between imputed datasets

# Onset
par(mfrow = c(3, 3))

# Loop through the first 9 imputations for the onset model
for (i in 1:9) {
  # Assuming logistic_onset.4 is a list of model fits
  model_fit <- logistic_onset.4[[i]]
  
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

# Loop through the first 9 imputations for the progression model
for (i in 1:9) {
  # Assuming logistic_progression.4 is a list of model fits
  model_fit <- logistic_progression.4[[i]]
  
  binnedplot(
    fitted(model_fit),
    residuals(model_fit, type = "response"),
    nclass = NULL,
    xlab = "Expected Values",
    ylab = "Average residual",
    main = paste("Progression Model - Imputation", i), # Add a dynamic title
    cex.pts = 0.8,
    col.pts = 1,
    col.int = "gray"
  )
}

par(mfrow = c(1, 1))

# Both look good - residuals fine
# No significant differences between imputations

# Now creating binned component + residual plots to justify the choice of linear
# covariates in both models

# Only using imputation 1 here

# Onset

# Get the working residuals and the specific predictor
onset.model.fit <- logistic_onset.4$imp1
onset.y.outcome <- onset.model.fit$survey.design$variables$at_risk_or_worse
onset.mu.fitted <- fitted(onset.model.fit)
onset.working.residuals <- (onset.y.outcome - onset.mu.fitted) / onset.mu.fitted

onset.almi.data <- onset.model.fit$survey.design$variables$Almi
onset.age.data <- onset.model.fit$survey.design$variables$Age_yrs
onset.bfp.data <- onset.model.fit$survey.design$variables$Bfp_perc
onset.waistcircum.data <- onset.model.fit$survey.design$variables$WaistCircum_cm

# Create the binned plot of residuals vs. the predictor
par(mfrow = c(2, 2))
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

par(mfrow = c(1, 1))

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
par(mfrow = c(2, 2))

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
# Random scatter of residuals largely within the grey lines

# ---- Checking for influential points ---- #

# Using survey weighted cook's distance plots

# Onset

# Onset model 4 shows most cooks distances cluster below 20
# There are a cloud of points higher than 20 and one that is much higher at around 100
# It is useful to investigate these observations and how the model performs without them
# We do not want conclusions to depend on a small amount of influential points

# Finding and plotting cooks distances for onset model 4 on imputation 1
# Crucially, the results are very similar on other imputations, so only considering imputation 1 here is okay
onset.cooks <- svyCooksD(logistic_onset.4$imp1, doplot=TRUE)
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
# Make sure to use the same formula for strata, id, and weights as the original design
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
# This means that removing influential points does not change findings
summary(onset_without_influential)

# Now checking this for the progression model

prog.cooks <- svyCooksD(logistic_progression.4$imp1, doplot=TRUE)
# Saving indices of those points with the top 1% cooks distance
prog_cooks_threshold <- quantile(prog.cooks, probs = 0.99)
prog_influential_indices <- which(prog.cooks > prog_cooks_threshold)

# Creating a data frame with these influential observations
# Nothing immediately stands out with these
prog_influential_obs <- design_list$imp1$variables[prog_influential_indices,]
prog_influential_obs$CooksD <- prog.cooks[prog_influential_indices]

# Seek to refit the model without these points

# Create a new data frame by EXCLUDING the influential rows
prog_reduced_data <- design_list$imp1$variables[-prog_influential_indices, ]

# Create a new survey design object with this reduced dataset
# Make sure to use the same formula for strata, id, and weights as the original design
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