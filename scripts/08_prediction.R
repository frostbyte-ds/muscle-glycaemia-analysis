# ----------------- #
# Model Predictions
# ----------------- #

# Using progression model 4 to find predictions of the probability of becoming diabetic vs ALMI
# Predictions will be made on all 30 imputations and then combined using Rubin's Rules
# Will produce predictions for both male and female because model coefficients suggested
# odds of diabetes were largely different between men and women

# --- Step 1: Define model and data objects ---
m <- 30 # Number of imputations

# Use the first imputed dataset as a template for creating the prediction grid
# This is a safe and standard practice
template_data <- design_list$imp1$variables

# --- Step 2: Create prediction dataframes (one for each gender) ---

# Create a sequence of Almi values to predict over
almi_range <- seq(min(template_data$Almi),
                  max(template_data$Almi),
                  length.out = 100)

# Base prediction df with average/modal values for all other confounders
predict_df_base <- data.frame(
  Almi = almi_range,
  Age_yrs = mean(template_data$Age_yrs),
  Race = factor("Non-Hispanic White", levels = levels(template_data$Race)),
  Bfp_perc = mean(template_data$Bfp_perc),
  WaistCircum_cm = mean(template_data$WaistCircum_cm)
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
  
  # Get predictions (response = probability) and the standard error for each
  preds <- predict(model, newdata = predict_df_combined, type = "response", se.fit = TRUE)
  
  # Combine the predictors and the predictions into a single dataframe
  imputation_results <- cbind(predict_df_combined, as.data.frame(preds))
  
  return(imputation_results)
})

# --- Step 4: Pool the Results Using Rubin's Rules ---
# Now combine the 30 sets of predictions into one final result.

# First, stack all the dataframes from the list into one big dataframe,
# adding an ID for each imputation.
all_predictions <- bind_rows(prediction_list, .id = "imputation_id")

# Now, group by each prediction point and apply Rubin's rules
plot_data_pooled <- all_predictions %>%
  # Make sure the SE from predict() is named consistently
  rename(prevalence = response, se = SE) %>%
  
  # Group by each unique combination of predictors
  group_by(Almi, Age_yrs, Gender, Race, Bfp_perc, WaistCircum_cm) %>%
  
  # Apply the pooling formulas
  summarise(
    # 1. Average the predicted probabilities
    pooled_prevalence = mean(prevalence),
    
    # 2. Calculate the 'within' imputation variance (average of the variances)
    within_imputation_var = mean(se^2),
    
    # 3. Calculate the 'between' imputation variance (variance of the averages)
    between_imputation_var = var(prevalence),
    
    # 4. Combine them for the total variance
    total_variance = within_imputation_var + (1 + 1/m) * between_imputation_var,
    
    # 5. Get the final pooled standard error
    pooled_se = sqrt(total_variance),
    
    .groups = 'drop' # Ungroup for the next steps
  ) %>%
  
  # 6. Calculate the final 95% confidence interval
  mutate(
    lower_ci = pooled_prevalence - 1.96 * pooled_se,
    upper_ci = pooled_prevalence + 1.96 * pooled_se
  )

# --- Step 5: Create the final plot with two lines ---

ggplot(plot_data_pooled, aes(x = Almi, y = pooled_prevalence, color = Gender, fill = Gender)) +
  # Add the prediction lines
  geom_line(linewidth = 1.2) +
  
  # Add the confidence ribbons
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, linetype = "blank") +
  
  # Customize colors and labels
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
  
  labs(
    title = "Predicted Prevalence of Diabetes by Muscle Mass (ALMI) and Gender",
    subtitle = "Pooled across 30 imputations for a person with average age and body composition",
    x = "Appendicular Lean Mass Index (kg/m²)",
    y = "Predicted Prevalence of Diabetes"
  ) +
  theme(legend.position = "bottom")