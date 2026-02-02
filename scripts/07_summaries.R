# ----------------------#
# Data & Model summaries 
# ----------------------#

# Descriptive statistics table of the final analytical sample

# Seek to obtain the mean and standard deviation of each continuous variable, and
# the count and proportion of each categorical variable from each imputed dataset.
# The survey structure must be taken into account for these tasks using functions
# such as svymean for survey-weighted means and svyvar for survey-weighted variances.

# These statistics will then be pooled and used to produce a concise summary statistics
# table.

# --- Step 1: Define a function to calculate summary stats for ONE dataset ---
get_summary_stats <- function(design) {
  # --- Continuous Variables ---
  continuous_vars <- c("Age_yrs", "Almi", "WaistCircum_cm", "Bfp_perc", 
                       "FamIncPov_Ratio", "HEI", "Phys", "AvgNightlySleep", "Hba1c_perc")
  
  cont_summary <- map_df(continuous_vars, ~{
    mean_val <- svymean(as.formula(paste0("~", .x)), design, na.rm = TRUE)
    var_val <- svyvar(as.formula(paste0("~", .x)), design, na.rm = TRUE)
    tibble(
      Variable = .x,
      Mean = as.numeric(coef(mean_val)),
      SD = sqrt(as.numeric(var_val)),
      Type = "Continuous"
    )
  })
  
  # --- Categorical Variables ---
  categorical_vars <- c("Gender", "Race", "Smoking_Status", "Alcohol_Status", "at_risk_or_worse", "is_diabetic")
  
  cat_summary <- map_df(categorical_vars, ~{
    tbl <- svytable(as.formula(paste0("~", .x)), design)
    props <- prop.table(tbl)
    tibble(
      Variable = paste0(.x, ", ", names(tbl)),
      Prop = as.numeric(props),
      Type = "Categorical"
    )
  })
  
  bind_rows(cont_summary, cat_summary)
}

# --- Step 2: Apply the function to all 30 design objects and pool ---
# map_df runs the function on each design object and stacks the results.
pooled_results <- map_df(design_list, get_summary_stats, .id = "imputation")

# Now, calculate the final pooled estimates by averaging across imputations
final_summary <- pooled_results %>%
  group_by(Variable, Type) %>%
  summarise(
    Pooled_Mean = mean(Mean, na.rm = TRUE),
    Pooled_SD = mean(SD, na.rm = TRUE),
    Pooled_Prop = mean(Prop, na.rm = TRUE),
    .groups = "drop"
  )

# --- Step 3: Format the final table ---
# Get total N from the first imputation (it's the same for all)
total_n <- nrow(imputed_1118[[1]])

# Format the continuous and categorical results separately
formatted_continuous <- final_summary %>%
  filter(Type == "Continuous") %>%
  mutate(Value = sprintf("%.1f (%.1f)", Pooled_Mean, Pooled_SD)) %>%
  select(Variable, Value)

formatted_categorical <- final_summary %>%
  filter(Type == "Categorical") %>%
  mutate(
    Pooled_N = Pooled_Prop * total_n,
    Value = sprintf("%.0f (%.1f%%)", Pooled_N, Pooled_Prop * 100)
  ) %>%
  select(Variable, Value)

variable_order <- c(
  # Continuous Variables
  "Age (years)", "ALMI (kg/m²)", "HbA1c (%)", "Body Fat Percentage (%)",
  "Waist Circumference (cm)", "Poverty Income Ratio", "Physical Activity (mins/day)",
  "Healthy Eating Index", "Average Nightly Sleep (hours)",
  # Categorical Variables
  "Normal HbA1c", "Elevated HbA1c", "Non-Diabetic", "Diabetic",
  "Male", "Female",
  "Mexican American", "Hispanic", "Non-Hispanic White",
  "Non-Hispanic Black", "Non-Hispanic Asian", "Other Race",
  "Never Drinker", "Former Drinker", "Moderate Drinker", "Heavy Drinker",
  "Never Smoker", "Former Smoker", "Current Smoker"
)

# Combine into the final data frame for the table
final_table_df <- bind_rows(formatted_continuous, formatted_categorical) %>%
  mutate(Variable_Clean = case_when(
    Variable == "Almi" ~ "ALMI (kg/m²)",
    Variable == "Hba1c_perc" ~ "HbA1c (%)",
    Variable == "at_risk_or_worse, 0" ~ "Normal HbA1c",
    Variable == "at_risk_or_worse, 1" ~ "Elevated HbA1c",
    Variable == "is_diabetic, 0" ~ "Non-Diabetic",
    Variable == "is_diabetic, 1" ~ "Diabetic",
    Variable == "Age_yrs" ~ "Age (years)",
    Variable == "Gender, Male" ~ "Male",
    Variable == "Gender, Female" ~ "Female",
    Variable == "Race, Mexican American" ~ "Mexican American",
    Variable == "Race, Other Hispanic" ~ "Hispanic",
    Variable == "Race, Non-Hispanic White" ~ "Non-Hispanic White",
    Variable == "Race, Non-Hispanic Black" ~ "Non-Hispanic Black",
    Variable == "Race, Non-Hispanic Asian" ~ "Non-Hispanic Asian",
    Variable == "Race, Other Race - Including Multi-Racial" ~ "Other Race",
    Variable == "Bfp_perc" ~ "Body Fat Percentage (%)",
    Variable == "WaistCircum_cm" ~ "Waist Circumference (cm)",
    Variable == "FamIncPov_Ratio" ~ "Poverty Income Ratio",
    Variable == "Phys" ~ "Physical Activity (mins/day)",
    Variable == "HEI" ~ "Healthy Eating Index",
    Variable == "Alcohol_Status, Never Drinker" ~ "Never Drinker",
    Variable == "Alcohol_Status, Former Drinker" ~ "Former Drinker",
    Variable == "Alcohol_Status, Moderate Drinker" ~ "Moderate Drinker",
    Variable == "Alcohol_Status, Heavy Drinker" ~ "Heavy Drinker",
    Variable == "Smoking_Status, Never Smoker" ~ "Never Smoker",
    Variable == "Smoking_Status, Former Smoker" ~ "Former Smoker",
    Variable == "Smoking_Status, Current Smoker" ~ "Current Smoker",
    Variable == "AvgNightlySleep" ~ "Average Nightly Sleep (hours)",
    TRUE ~ Variable  # Keep original name if no match
  )) %>%
  # Convert the clean Variable name to a factor to enforce order
  mutate(Variable_Clean = factor(Variable_Clean, levels = variable_order)) %>%
  # Arrange the data according to the factor levels
  arrange(Variable_Clean) %>%
  # Select the final columns for the table
  select(Variable = Variable_Clean, Value) %>%
  # Renaming variable column
  rename(Characteristic = Variable)

# --- Create the Table ---
descriptive.tbl.kbl <- kbl(
  final_table_df,
  format = "latex",
  booktabs = TRUE
) %>%
  kable_styling(
    font_size = 7,
    full_width = FALSE
  ) %>%
  pack_rows("Glycaemic Status (Onset)", 10, 11) %>%
  pack_rows("Diabetes Status (Progression)", 12, 13) %>%
  pack_rows("Gender", 14, 15) %>%
  pack_rows("Race", 16, 21) %>%
  pack_rows("Alcohol Consumption", 22, 25) %>%
  pack_rows("Smoking Status", 26, 28) %>%
  footnote(
    general = "ALMI: Appendicular Lean Mass Index; HbA1c: Haemoglobin A1c.",
    symbol = "Continuous variables presented as mean (SD), categorical variables presented as n (%).",
    threeparttable = TRUE, 
    footnote_as_chunk = TRUE
  )

# --- Hierarchical model building tables --- #

# Extracting pooled model ALMI coefficients and confidence intervals
# Also finding the % change of each ALMI coefficient progressing through the models

# Onset

# Model names
pooled.onset.names <- c("pooled_logistic_onset", paste0("pooled_logistic_onset.", 2:10))

# ALMI coefficients
onset.almi.coeffs <- sapply(pooled.onset.names, function(name) {
  model_list <- get(name)
  coef(model_list)["Almi"]
})

# ALMI confidence intervals
onset.almi.confint <- sapply(pooled.onset.names, function(name) {
  model_list <- get(name)
  c(confint(model_list)[2,1], confint(model_list)[2,2])
})

# % changes
onset.percent.changes <- c("N/A")  # First % is NA as there is no comparison
for (i in 1:9) {
  onset.percent.changes[i+1] <- ((onset.almi.coeffs[i + 1] - onset.almi.coeffs[i]) / abs(onset.almi.coeffs[i])) * 100
}

# Progression

# Model names
pooled.prog.names <- c("pooled_logistic_progression", paste0("pooled_logistic_progression.", 2:10))

# ALMI coefficients
prog.almi.coeffs <- sapply(pooled.prog.names, function(name) {
  model_list <- get(name)
  coef(model_list)["Almi"]
})

# ALMI confidence intervals
prog.almi.confint <- sapply(pooled.prog.names, function(name) {
  model_list <- get(name)
  c(confint(model_list)[2,1], confint(model_list)[2,2])
})

# % changes
prog.percent.changes <- c("N/A")
for (i in 1:9) {
  prog.percent.changes[i+1] <- ((prog.almi.coeffs[i + 1] - prog.almi.coeffs[i]) / abs(prog.almi.coeffs[i])) * 100
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

# Array of covariate changes for summary tables
covariates <- c("Crude", "+ Demographics", "+ Body Fat Percentage (%)", "+ Waist Circumference (cm)",
                "+ Poverty Income Ratio", "+ Physical Activity (mins)", "+ Healthy Eating Index", "+ Alcohol Status",
                "+ Smoking Status", "+ Average Daily Sleep (hrs)")

# Data frame
onset.summary.df <- data.frame(
  Model = paste("Model", 1:10),
  `Covariate Changes` = covariates,
  `ALMI Odds Ratio (OR)` = exp(onset.almi.coeffs),
  `95% Confidence Interval (OR)` = onset.ci.combined,
  `ALMI Coefficient (log OR)` = onset.almi.coeffs,
  `Percentage Change in Coefficient` = onset.percent.changes.formatted,
  `McFadden Pseudo R²` = onset.pseudo.r2.list,
  `Archer-Lemeshow GoF Test P-value` = onset.gof.pvalue,
  check.names = FALSE
  
)

# Creating table
onset.summary.table.kbl <- kbl(
  onset.summary.df,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  digits = c(NA, NA, 3, NA, 3, NA, 3, 3),
  align = c("l", "l", "r", "r", "r", "r", "r", "r")
) %>%
  kable_styling(
    font_size = 8
  ) %>%
  add_header_above(
    c("Odds Ratios, Coefficients, Percentage Changes, and Diagnostics" = 8),
    bold = FALSE
  ) %>%
  column_spec(column = 2, width = "15em") %>%
  column_spec(column = 3, width = "4em") %>%
  column_spec(column = 4, width = "7em") %>%
  column_spec(column = 5, width = "5em") %>%
  column_spec(column = 6, width = "5em") %>%
  column_spec(column = 7, width = "5em") %>%
  column_spec(column = 8, width = "5em")

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
  `ALMI Odds Ratio (OR)` = exp(prog.almi.coeffs),
  `95% Confidence Interval (OR)` = prog.ci.combined,
  `ALMI Coefficient (log OR)` = prog.almi.coeffs,
  `Percentage Change in Coefficient` = prog.percent.changes.formatted,
  `McFadden Pseudo R²` = prog.pseudo.r2.list,
  `Archer-Lemeshow GoF Test P-value` = prog.gof.pvalue,
  check.names = FALSE
  
)

# Creating table
prog.summary.table.kbl <- kbl(
  prog.summary.df,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  digits = c(NA, NA, 3, NA, 3, NA, 3, 3),
  align = c("l", "l", "r", "r", "r", "r", "r", "r")
) %>%
  kable_styling(
    font_size = 8
  ) %>%
  add_header_above(
    c("Odds Ratios, Coefficients, Percentage Changes, and Diagnostics" = 8),
    bold = FALSE
  ) %>%
  column_spec(column = 2, width = "15em") %>%
  column_spec(column = 3, width = "4em") %>%
  column_spec(column = 4, width = "7em") %>%
  column_spec(column = 5, width = "5em") %>%
  column_spec(column = 6, width = "5em") %>%
  column_spec(column = 7, width = "5em") %>%
  column_spec(column = 8, width = "5em")

# --- Forest plot for visualising final ALMI coefficients --- #

# This code is used for producing a forest plot presenting the ALMI coefficient from Onset Model 10
# and Progression Model 4

# --- Step 1: Extract the results for each model robustly ---

# -- Onset Model --
# Get the full summary table from the pooled model
onset_summary <- summary(pooled_logistic_onset.10)
# Extract each component into its own variable to ensure correct structure
onset_log_or <- onset_summary["Almi", "results"]
onset_lower_ci <- onset_summary["Almi", "(lower"]
onset_upper_ci <- onset_summary["Almi", "upper)"]

# -- Progression Model --
# Get the full summary table from the pooled model
prog_summary <- summary(pooled_logistic_progression.4)
# Extract each component into its own variable
prog_log_or <- prog_summary["Almi", "results"]
prog_lower_ci <- prog_summary["Almi", "(lower"]
prog_upper_ci <- prog_summary["Almi", "upper)"]


# --- Step 2: Combine the results into a single, clean dataframe ---
plot_data_forest <- tibble(
  Model = c("Onset", "Progression"),
  log_or = c(onset_log_or, prog_log_or),
  log_lower_ci = c(onset_lower_ci, prog_lower_ci),
  log_upper_ci = c(onset_upper_ci, prog_upper_ci)
) %>%
  # Exponentiate everything to get Odds Ratios and their CIs
  mutate(
    or = exp(log_or),
    lower_ci = exp(log_lower_ci),
    upper_ci = exp(log_upper_ci)
  ) %>%
  mutate(Model = factor(Model, levels = c("Progression", "Onset")))

# --- Step 3: Create the Forest Plot ---
almi_forest_plot <- ggplot(plot_data_forest, aes(x = or, y = Model)) +
  # Add the points for the Odds Ratios
  geom_point(size = 4, shape = 18, color = "darkblue") +
  
  # Add the lines for the 95% Confidence Intervals
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, linewidth = 1, color = "darkblue") +
  
  # Add a vertical line at OR = 1.0 for reference (no effect)
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Use a logarithmic scale for the x-axis, which is standard for ratio plots
  scale_x_log10() +
  
  # Add labels and a title
  labs(
    title = "Stage-Specific Association of ALMI with Glycaemic Control",
    subtitle = "Odds Ratios and 95% Confidence Intervals",
    x = "Odds Ratio (per 1 kg/m² increase in ALMI)",
    y = "Model Outcome"
  ) +
  
  # Apply custom theme
  theme_dissertation()

# Display the final plot
almi_forest_plot

# -- VIF values evidencing the masking effect of waist circumference seen when moving from onset
# and progression models 3 to 4 -- #

# Check of survey adjusted VIF values of progression model 4 show high significant multicollinearity
# This is not a problem - it explains the effect that causes the ALMI coefficient
# becoming negative once waist circumference is added to the model.

# Onset

# Model 3
# Design matrix from the model formula and data
X.onset.3 <- model.matrix(logistic_onset.3$imp1$formula, data = design_list$imp1$variables)
# Remove the intercept column
X.onset.3 <- X.onset.3[, -1]
# Extract the survey weights from the design object
weights <- weights(design_list$imp1)
# Run svyvif
onset_vif_values.3 <- svyvif(mobj = logistic_onset.3$imp1, X = X.onset.3, w = weights)
# Print the results
print(onset_vif_values.3$`Intercept adjusted`)

# Model 4
X.onset.4 <- model.matrix(logistic_onset.4$imp1$formula, data = design_list$imp1$variables)
X.onset.4 <- X.onset.4[, -1]
onset_vif_values.4 <- svyvif(mobj = logistic_onset.4$imp1, X = X.onset.4, w = weights)
print(onset_vif_values.4$`Intercept adjusted`)

# Progression

# Model 3
X.prog.3 <- model.matrix(logistic_progression.3$imp1$formula, data = design_list$imp1$variables)
X.prog.3 <- X.prog.3[, -1]
prog_vif_values.3 <- svyvif(mobj = logistic_progression.3$imp1, X = X.prog.3, w = weights)
print(prog_vif_values.3$`Intercept adjusted`)

# Model 4
X.prog.4 <- model.matrix(logistic_progression.4$imp1$formula, data = design_list$imp1$variables)
X.prog.4 <- X.prog.4[, -1]
prog_vif_values.4 <- svyvif(mobj = logistic_progression.4$imp1, X = X.prog.4, w = weights)
print(prog_vif_values.4$`Intercept adjusted`)

# Before waist circumference is added, there is no evidence of severe multicollinearity,
# as seen by the weighted VIF values of progression model 3.
# This further reinforces the findings.