# ----------------- #
# Model Predictions
# ----------------- #

# Using Progression model 4 to find predictions of the probability of becoming diabetic vs ALMI
# Predictions will be made on all 30 imputations and then combined using Rubin's Rules
# Will produce predictions for both male and female because model coefficients suggested
# odds of diabetes were largely different between men and women

# --- Step 1: Define model and data objects ---
m <- 30 # Number of imputations

# Use the first imputed dataset as a template for non-imputed variables and ranges
template_data <- imputed_1118[[1]]

# Calculate the pooled mean for the continuous variables that were involved in imputation
pooled_mean_bfp <- mean(all_imputed_data$Bfp_perc)
pooled_mean_waist <- mean(all_imputed_data$WaistCircum_cm)

# For variables that were NOT imputed, take the value from the template
# This is more efficient as the value is the same across all imputed datasets.
pooled_mean_age <- mean(template_data$Age_yrs)

# Calculate the mode for Race from the template data
pooled_mode_race <- names(which.max(table(template_data$Race)))

# --- Step 2: Create prediction data frames (one for each gender) ---

# Create a sequence of ALMI values to predict over
almi_range <- seq(min(template_data$Almi),
                  max(template_data$Almi),
                  length.out = 1000)

# Base prediction df with average/modal values for all other confounders
predict_df_base <- data.frame(
  Almi = almi_range,
  Age_yrs = pooled_mean_age,
  Race = factor(pooled_mode_race, levels = levels(template_data$Race)),
  Bfp_perc = pooled_mean_bfp,
  WaistCircum_cm = pooled_mean_waist
)

# Create a specific version for Males
predict_df_male <- predict_df_base %>%
  mutate(Gender = factor("Male", levels = levels(template_data$Gender)))

# Create a specific version for Females
predict_df_female <- predict_df_base %>%
  mutate(Gender = factor("Female", levels = levels(template_data$Gender)))

# Combine into a single newdata object for easier looping
predict_df_combined <- rbind(predict_df_male, predict_df_female)

# --- Step 3: Predict on ALL Imputations ---
# Loop through each of the 30 models and get predictions.

prediction_list <- lapply(logistic_progression.4, function(model) {
  
  # Get predictions and the standard error for each in the logit scale
  # This prevents negative probability confidence intervals
  preds <- predict(model, newdata = predict_df_combined, type = "link", se.fit = TRUE)
  
  # Combine the predictors and the predictions into a single dataframe
  imputation_results <- cbind(predict_df_combined, as.data.frame(preds))
  
  return(imputation_results)
})

# --- Step 4: Pool the Results Using Rubin's Rules ---
# Now combine the 30 sets of predictions into one final result.

# First, stack all the data frames from the list into one big data frame,
# adding an ID for each imputation.
all_predictions <- bind_rows(prediction_list, .id = "imputation_id")

# Now, group by each prediction point and apply Rubin's rules
plot_data_pooled <- all_predictions %>%
  # Rename columns for clarity
  rename(logit_pred = link, se = SE) %>%
  group_by(Almi, Age_yrs, Gender, Race, Bfp_perc, WaistCircum_cm) %>%
  summarise(
    # Pool the logit predictions
    pooled_logit = mean(logit_pred),
    within_imputation_var = mean(se^2),
    between_imputation_var = var(logit_pred),
    total_variance = within_imputation_var + (1 + 1/m) * between_imputation_var,
    pooled_se_logit = sqrt(total_variance),
    .groups = 'drop'
  ) %>%
  # Back-transform to the Probability Scale 
  mutate(
    # Back-transform the point estimate
    pooled_prevalence = plogis(pooled_logit),
    
    # Calculate the CI on the logit scale
    lower_ci_logit = pooled_logit - 1.96 * pooled_se_logit,
    upper_ci_logit = pooled_logit + 1.96 * pooled_se_logit,
    
    # Back-transform the CI bounds to the probability scale
    lower_ci = plogis(lower_ci_logit),
    upper_ci = plogis(upper_ci_logit)
  )

# --- Step 5: Create the final plot with two lines ---

pred.plot <- ggplot(plot_data_pooled, aes(x = Almi, y = pooled_prevalence, color = Gender, fill = Gender)) +
  # Add the prediction lines
  geom_line(linewidth = 1.2) +
  
  # Add the confidence ribbons
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, linetype = "blank") +
  
  # Customize colours and labels
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
  
  labs(
    title = "Predicted Prevalence of Diabetes by Muscle Mass (ALMI) and Gender",
    subtitle = "Pooled across 30 imputations for a person with average age and body composition",
    x = "Appendicular Lean Mass Index (kg/m²)",
    y = "Predicted Prevalence of Diabetes"
  ) +
  theme_dissertation()