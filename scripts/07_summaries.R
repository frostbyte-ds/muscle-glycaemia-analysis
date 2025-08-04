# --------------------------------------------- #
# Model summaries and coefficient visualisation
# --------------------------------------------- #

# Extracting pooled model ALMI coefficients and confidence intervals
# Also finding the % change of each ALMI coefficient progressing through the models

# Onset
pooled.onset.names <- c("pooled_logistic_onset", paste0("pooled_logistic_onset.", 2:10))

onset.almi.coeffs <- sapply(pooled.onset.names, function(name) {
  model_list <- get(name)
  coef(model_list)["Almi"]
})

onset.almi.confint <- sapply(pooled.onset.names, function(name) {
  model_list <- get(name)
  c(confint(model_list)[2,1], confint(model_list)[2,2])
})

# % changes

onset.percent.changes <- c("N/A")  # First % is NA as there is no comparison
for (i in 1:9) {
  onset.percent.changes[i+1] <- ((abs(onset.almi.coeffs[i + 1]) - abs(onset.almi.coeffs[i])) / abs(onset.almi.coeffs[i])) * 100
}

# Progression

pooled.prog.names <- c("pooled_logistic_progression", paste0("pooled_logistic_progression.", 2:10))

prog.almi.coeffs <- sapply(pooled.prog.names, function(name) {
  model_list <- get(name)
  coef(model_list)["Almi"]
})

prog.almi.confint <- sapply(pooled.prog.names, function(name) {
  model_list <- get(name)
  c(confint(model_list)[2,1], confint(model_list)[2,2])
})

# % changes

prog.percent.changes <- c("N/A")
for (i in 1:9) {
  prog.percent.changes[i+1] <- ((abs(prog.almi.coeffs[i + 1]) - abs(prog.almi.coeffs[i])) / abs(prog.almi.coeffs[i])) * 100
}

# ----- ONSET SUMMARY TABLE ----- #

# Removing names from coeffs and confint objects
names(onset.almi.coeffs) <- NULL
colnames(onset.almi.confint) <- NULL

# Compute exponentiated confidence intervals
onset.lower.bounds <- exp(onset.almi.confint[1,])
onset.upper.bounds <- exp(onset.almi.confint[2,])

# Combine into a single string column
onset.ci.combined <- paste0("(", sprintf("%.3f", onset.lower.bounds), ", ", sprintf("%.3f", onset.upper.bounds), ")")

# Formatting percentage change column
onset.percent.changes.formatted <- c("N/A")
value <- c()
for (i in 2:10) {
  value[i] <- as.numeric(onset.percent.changes[i])
  onset.percent.changes.formatted[i] = paste0(sprintf("%.2f", value[i]), "%")
}

# Data frame
onset.summary.df <- data.frame(
  Model = paste("Model", 1:10),
  `Covariate Changes` = covariates,
  `Odds Ratio (OR)` = exp(onset.almi.coeffs),
  `95% Confidence Interval (OR)` = onset.ci.combined,
  `ALMI Coefficient (log OR)` = onset.almi.coeffs,
  `Percentage Change in Coefficient` = onset.percent.changes.formatted,
  check.names = FALSE
  
)

# Creating table
onset.summary.table <- onset.summary.df %>%
  gt() %>%
  
  # Format numeric columns nicely
  fmt_number(
    columns = c(`Odds Ratio (OR)`, `ALMI Coefficient (log OR)`),
    decimals = 3
  ) %>%
  
  
  # Align columns (right align numbers, left align text)
  cols_align(
    align = "left",
    columns = c(Model, `Covariate Changes`)
  ) %>%
  cols_align(
    align = "right",
    columns = c(`Odds Ratio (OR)`, `95% Confidence Interval (OR)`, `ALMI Coefficient (log OR)`, `Percentage Change in Coefficient`)
  ) %>%
  
  # Add a title and subtitle 
  tab_header(
    title = "Summary of Logtistic Regression Onset Model Results",
    subtitle = "Relative Risks, Coefficients, and Percentage Changes"
  ) 

# Print the table
onset.summary.table

# ----- PROG SUMMARY TABLE ----- #

names(prog.almi.coeffs) <- NULL
colnames(prog.almi.confint) <- NULL

# Compute exponentiated confidence intervals
prog.lower.bounds <- exp(prog.almi.confint[1,])
prog.upper.bounds <- exp(prog.almi.confint[2,])

# Combine into a single string column
prog.ci.combined <- paste0("(", sprintf("%.3f", prog.lower.bounds), ", ", sprintf("%.3f", prog.upper.bounds), ")")

# Formatting percentage change column
prog.percent.changes.formatted <- c("N/A")
value <- c()
for (i in 2:10) {
  value[i] <- as.numeric(prog.percent.changes[i])
  prog.percent.changes.formatted[i] = paste0(sprintf("%.2f", value[i]), "%")
}

# Data frame
prog.summary.df <- data.frame(
  Model = paste("Model", 1:10),
  `Covariate Changes` = covariates,
  `Odds Ratio (OR)` = exp(prog.almi.coeffs),
  `95% Confidence Interval (OR)` = prog.ci.combined,
  `ALMI Coefficient (log OR)` = prog.almi.coeffs,
  `Percentage Change in Coefficient` = prog.percent.changes.formatted,
  check.names = FALSE
  
)

# Creating table
prog.summary.table <- prog.summary.df %>%
  gt() %>%
  
  # Format numeric columns nicely
  fmt_number(
    columns = c(`Odds Ratio (OR)`, `ALMI Coefficient (log OR)`),
    decimals = 3
  ) %>%
  
  
  # Align columns (right align numbers, left align text)
  cols_align(
    align = "left",
    columns = c(Model, `Covariate Changes`)
  ) %>%
  cols_align(
    align = "right",
    columns = c(`Odds Ratio (OR)`, `95% Confidence Interval (OR)`, `ALMI Coefficient (log OR)`, `Percentage Change in Coefficient`)
  ) %>%
  
  # Add a title and subtitle 
  tab_header(
    title = "Summary of Logistic Regression Progression Model Results",
    subtitle = "Odds Ratios, Coefficients, and Percentage Changes"
  ) 

# Print the table
prog.summary.table

# --- Summary tables of all model coefficients from the fully adjusted models --- #

# -- VIF values evidencing the suppressor effect seen when moving from onset and progression models 3 to 4 -- #

# Check of survey adjusted VIF values of progression model 4 show high significant multicollinearity
# This is not a problem and explains the the suppressor effect that causes the ALMI coefficient
# becoming negative once waist circumference is added to the model.

# Design matrix from the model formula and data
X.prog.4 <- model.matrix(logistic_progression.4$imp1$formula, data = design_list$imp1$variables)
# Remove the intercept column
X.prog.4 <- X.prog.4[, -1]
# Extract the survey weights from the design object
weights <- weights(design_list$imp1)
# Run svyvif
prog_vif_values.4 <- svyvif(mobj = logistic_progression.4$imp1, X = X.prog.4, w = weights)
# Print the results
print(prog_vif_values.4$`Intercept adjusted`)

# Before waist circumference is added, there is no evidence of severe multicolinearity,
# as seen by the weighted VIF values of progression model 3.
# This further reinforces the findings.

# Design matrix from the model formula and data
X.prog.3 <- model.matrix(logistic_progression.3$imp1$formula, data = design_list$imp1$variables)
# Remove the intercept column
X.prog.3 <- X.prog.3[, -1]
# Run svyvif
prog_vif_values.3 <- svyvif(mobj = logistic_progression.3$imp1, X = X.prog.3, w = weights)
# Print the results
print(prog_vif_values.3$`Intercept adjusted`)

# --- Forest plots and tables of odds ratios from fully adjusted models --- #

# Useful to visualise coefficient estimates

# Onset Model 10

# --- Step 1: Extract the results into a clean dataframe --- #

# Get the full summary table from the pooled model on the log scale
onset_model_summary_log <- summary(pooled_logistic_onset.10)

# Create a clean dataframe from this summary object
onset.forest.plot.data <- data.frame(
  variable = rownames(onset_model_summary_log),
  log_rr = onset_model_summary_log$results,
  log_lower_ci = onset_model_summary_log$`(lower`,
  log_upper_ci = onset_model_summary_log$`upper)`
) %>%
  # Exponentiate everything to get Odds Ratios and their CIs
  mutate(
    rr = exp(log_rr),
    lower_ci = exp(log_lower_ci),
    upper_ci = exp(log_upper_ci)
  ) %>%
  # Remove the intercept row
  filter(variable != "(Intercept)") %>%
  mutate(variable = case_when(
    variable == "Almi" ~ "ALMI",
    variable == "Age_yrs" ~ "Age",
    variable == "GenderFemale" ~ "Female",
    variable == "RaceOther Hispanic" ~ "Hispanic",
    variable == "RaceNon-Hispanic White" ~ "Non-Hispanic White",
    variable == "RaceNon-Hispanic Black" ~ "Non-Hispanic Black",
    variable == "RaceNon-Hispanic Asian" ~ "Non-Hispanic Asian",
    variable == "RaceOther Race - Including Multi-Racial" ~ "Other Race",
    variable == "Bfp_perc" ~ "Body Fat Percentage",
    variable == "WaistCircum_cm" ~ "Waist Circumference",
    variable == "FamIncPov_Ratio" ~ "Ratio of Family Income to Poverty",
    variable == "Phys" ~ "Physical Activity",
    variable == "Hei" ~ "Healthy Eating Index",
    variable == "Alcohol_StatusFormer Drinker" ~ "Former Drinker",
    variable == "Alcohol_StatusModerate Drinker" ~ "Moderate Drinker",
    variable == "Alcohol_StatusHeavy Drinker" ~ "Heavy Drinker",
    variable == "Smoking_StatusFormer Smoker" ~ "Former Smoker",
    variable == "Smoking_StatusCurrent Smoker" ~ "Current Smoker",
    variable == "AvgNightlySleep" ~ "Average Nightly Sleep",
    TRUE ~ variable  # Keep original name if no match
  ))


# --- Step 2: Create the Forest Plot --- #

ggplot(onset.forest.plot.data, aes(x = rr, y = reorder(variable, rr))) +
  # Add the points for the Odds Ratios
  geom_point(size = 3, color = "darkblue") +
  
  # Add the lines for the 95% Confidence Intervals
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, color = "darkblue") +
  
  # Add a vertical line at OR = 1.0 for reference (no effect)
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
  
  # Use a logarithmic scale for the x-axis, which is standard for ratio plots
  scale_x_log10(breaks = c(0.5, 1.0, 1.5, 2.0, 2.5)) +
  
  labs(
    title = "Odds Ratio of Coefficients in the Fully Adjusted Model Onset Model",
    x = "Odds Ratio",
    y = "Predictor Variable"
  )

# Also making a table of these values

# Combining CIs into a single string column
onset.adjusted.ci.combined <- paste0("(", sprintf("%.3f", onset.forest.plot.data$lower_ci), ", ", sprintf("%.3f", onset.forest.plot.data$upper_ci), ")")

# Dataframe for table
onset.adjusted.summary.df <- onset.forest.plot.data[,c(1,5)] %>%
  rename(
    Predictor = variable,
    `Odds Ratio (OR)` = rr
  ) %>%
  mutate(
    `95% Confidence Interval (OR)` = onset.adjusted.ci.combined
  )

# Creating table
onset.adjusted.summary.tbl <- onset.adjusted.summary.df %>%
  gt() %>%
  
  # Format numeric columns nicely
  fmt_number(
    columns = c(`Odds Ratio (OR)`),
    decimals = 3
  ) %>%
  
  
  # Align columns (right align numbers, left align text)
  cols_align(
    align = "left",
    columns = c(Predictor)
  ) %>%
  cols_align(
    align = "right",
    columns = c(`Odds Ratio (OR)`, `95% Confidence Interval (OR)`)
  ) %>%
  
  # Add a title and subtitle 
  tab_header(
    title = "Summary of Fully Adjusted Onset Model",
    subtitle = "Coefficient Estimates and 95% Confidence Intervals"
  ) 

# Print the table
onset.adjusted.summary.tbl

# Progression Model 10

# --- Step 1: Extract the results into a clean dataframe --- #

# Get the full summary table from the pooled model on the log scale
prog_model_summary_log <- summary(pooled_logistic_progression.10)

# Create a clean dataframe from this summary object
prog.forest.plot.data <- data.frame(
  variable = rownames(prog_model_summary_log),
  log_rr = prog_model_summary_log$results,
  log_lower_ci = prog_model_summary_log$`(lower`,
  log_upper_ci = prog_model_summary_log$`upper)`
) %>%
  # Exponentiate everything to get Odds Ratios and their CIs
  mutate(
    rr = exp(log_rr),
    lower_ci = exp(log_lower_ci),
    upper_ci = exp(log_upper_ci)
  ) %>%
  # Remove the intercept row, as we don't usually plot it
  filter(variable != "(Intercept)") %>%
  mutate(variable = case_when(
    variable == "Almi" ~ "ALMI",
    variable == "Age_yrs" ~ "Age",
    variable == "GenderFemale" ~ "Female",
    variable == "RaceOther Hispanic" ~ "Hispanic",
    variable == "RaceNon-Hispanic White" ~ "Non-Hispanic White",
    variable == "RaceNon-Hispanic Black" ~ "Non-Hispanic Black",
    variable == "RaceNon-Hispanic Asian" ~ "Non-Hispanic Asian",
    variable == "RaceOther Race - Including Multi-Racial" ~ "Other Race",
    variable == "Bfp_perc" ~ "Body Fat Percentage",
    variable == "WaistCircum_cm" ~ "Waist Circumference",
    variable == "FamIncPov_Ratio" ~ "Ratio of Family Income to Poverty",
    variable == "Phys" ~ "Physical Activity",
    variable == "Hei" ~ "Healthy Eating Index",
    variable == "Alcohol_StatusFormer Drinker" ~ "Former Drinker",
    variable == "Alcohol_StatusModerate Drinker" ~ "Moderate Drinker",
    variable == "Alcohol_StatusHeavy Drinker" ~ "Heavy Drinker",
    variable == "Smoking_StatusFormer Smoker" ~ "Former Smoker",
    variable == "Smoking_StatusCurrent Smoker" ~ "Current Smoker",
    variable == "AvgNightlySleep" ~ "Average Nightly Sleep",
    TRUE ~ variable  # Keep original name if no match
  ))

# --- Step 2: Create the Forest Plot  --- #

ggplot(prog.forest.plot.data, aes(x = rr, y = reorder(variable, rr))) +
  # Add the points for the Odds Ratios
  geom_point(size = 3, color = "darkblue") +
  
  # Add the lines for the 95% Confidence Intervals
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, color = "darkblue") +
  
  # Add a vertical line at OR = 1.0 for reference (no effect)
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
  
  # Use a logarithmic scale for the x-axis, which is standard for ratio plots
  scale_x_log10(breaks = c(0.5, 1.0, 1.5, 2.0, 2.5)) +
  
  labs(
    title = "Odds Ratio of Coefficients in the Fully Adjusted Progression Model",
    x = "Odds Ratio",
    y = "Predictor Variable"
  )

# Now making a table for these values

# Combining CIs into a single string column
prog.adjusted.ci.combined <- paste0("(", sprintf("%.3f", prog.forest.plot.data$lower_ci), ", ", sprintf("%.3f", prog.forest.plot.data$upper_ci), ")")

# Dataframe for table
prog.adjusted.summary.df <- prog.forest.plot.data[,c(1,5)] %>%
  rename(
    Predictor = variable,
    `Odds Ratio (OR)` = rr
  ) %>%
  mutate(
    `95% Confidence Interval (OR)` = prog.adjusted.ci.combined
  )

# Creating table
prog.adjusted.summary.tbl <- prog.adjusted.summary.df %>%
  gt() %>%
  
  # Format numeric columns nicely
  fmt_number(
    columns = c(`Odds Ratio (OR)`),
    decimals = 3
  ) %>%
  
  
  # Align columns (right align numbers, left align text)
  cols_align(
    align = "left",
    columns = c(Predictor)
  ) %>%
  cols_align(
    align = "right",
    columns = c(`Odds Ratio (OR)`, `95% Confidence Interval (OR)`)
  ) %>%
  
  # Add a title and subtitle 
  tab_header(
    title = "Summary of Fully Adjusted Progression Model",
    subtitle = "Coefficient Estimates and 95% Confidence Intervals"
  ) 

# Print the table
prog.adjusted.summary.tbl