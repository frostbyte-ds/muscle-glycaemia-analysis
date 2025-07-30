# ----------------------- #
# K-Fold Cross Validation
# ----------------------- #

# Seek to choose one imputation and conduct k-fold cross validation parsimonious
# progression model 4

# Goal is to assess how well the model performance would be when trained using other datasets

# Using 5-fold cross validation for 10 repeats

# There is no neat function to do this for svyglms in the way that is required, so will use a
# custom algorithm

# The algorithm will calculate the optimal threshold my maximising Youden’s J statistic.
# This is the same as choosing the cut-off point closest to the (0, 1) corner of the ROC plane.
# This will of course be done using the training data for each loop.

# Also calculating the sensitivity and specificity for each loop for further helpful diagnostics

# Very important to take survey weights into account in this process

# Creating a duplicate of imputation 1
imp1.2 <- imputed_1118$imp1

# Parameters
k <- 5
n_repeats <- 10

# Setting seed for reproducibility
set.seed(123) 

# Dataframe to store summary results per repeat
repeat_summary <- data.frame(
  `repeat` = integer(n_repeats),
  mean_weighted_auc = numeric(n_repeats),
  mean_optimal_threshold = numeric(n_repeats),
  mean_weighted_error = numeric(n_repeats),
  mean_weighted_sensitivity = numeric(n_repeats),
  mean_weighted_specificity = numeric(n_repeats),
  check.names = FALSE
)

# Repeat process
for (rep in 1:n_repeats) {
  cat("Repeat", rep, "\n")
  
  # Assign new folds for this repeat
  imp1.2$fold_id <- sample(rep(1:k, length.out = nrow(imp1.2)))
  
  # Initialize storage
  weighted_aucs <- numeric(k)
  optimal_thresholds <- numeric(k)
  weighted_errors <- numeric(k)
  weighted_sensitivities <- numeric(k)   
  weighted_specificities <- numeric(k) 
  
  for (i in 1:k) {
    # Split data
    train_data <- imp1.2 %>% filter(fold_id != i)
    test_data  <- imp1.2 %>% filter(fold_id == i)
    
    # Survey design for training
    train_design <- svydesign(
      id = ~PSU,
      strata = ~Strata,
      weights = ~SurvWeight,
      nest = TRUE,
      data = train_data
    )
    
    # Fit model
    model <- svyglm(is_diabetic ~ Almi + Age_yrs + Gender + Race + Bfp_perc + WaistCircum_cm,
                    design = train_design,
                    family = quasibinomial)
    
    # Get predictions for both train and test sets
    train_preds <- predict(model, newdata = train_data, type = "response")
    test_preds  <- predict(model, newdata = test_data,  type = "response")
    
    # Weighted ROC and AUC (from training data)
    roc_obj <- WeightedROC(guess = train_preds, 
                           label = train_data$is_diabetic, 
                           weight = train_data$SurvWeight)
    
    weighted_aucs[i] <- WeightedAUC(roc_obj)
    
    # Optimal threshold from Youden’s J (from training data)
    roc_obj$YoudenJ <- roc_obj$TPR - roc_obj$FPR
    opt_idx <- which.max(roc_obj$YoudenJ)
    optimal_threshold <- roc_obj$threshold[opt_idx]
    optimal_thresholds[i] <- optimal_threshold
    
    # Apply optimal threshold to the test data to predict classes
    predicted_classes <- ifelse(test_preds >= optimal_threshold, 1, 0)
    test_data$predicted_class <- predicted_classes
    
    # Create an error column
    test_data$error <- ifelse(test_data$predicted_class != test_data$is_diabetic, 1, 0)
    
    # Now, define the survey design for the test set
    test_design <- svydesign(
      id = ~PSU,
      strata = ~Strata,
      weights = ~SurvWeight,
      nest = TRUE,
      data = test_data
    )
    
    # Calculate weighted error 
    weighted_errors[i] <- svymean(~error, design = test_design)[1]
    
    # Calculate Weighted Sensitivity & Specificity
    conf_matrix <- svytable(~predicted_class + is_diabetic, design = test_design)
    
    # Extract the four components
    TN <- conf_matrix[1, 1]
    FN <- conf_matrix[1, 2]
    FP <- conf_matrix[2, 1]
    TP <- conf_matrix[2, 2]
    
    # Calculate sensitivity and specificity
    weighted_sensitivities[i] <- TP / (TP + FN)
    weighted_specificities[i] <- TN / (TN + FP)
    
    # Print statement
    cat(sprintf("  Fold %d | AUC: %.3f | Thresh: %.3f | Sens: %.3f | Spec: %.3f | Err: %.3f\n",
                i, weighted_aucs[i], optimal_thresholds[i], 
                weighted_sensitivities[i], weighted_specificities[i], weighted_errors[i]))
  }
  
  # Store means from this repeat
  repeat_summary[rep, ] <- c(
    `repeat` = rep,
    mean_weighted_auc = mean(weighted_aucs),
    mean_optimal_threshold = mean(optimal_thresholds),
    mean_weighted_error = mean(weighted_errors),
    mean_weighted_sensitivity = mean(weighted_sensitivities, na.rm = TRUE), # na.rm is a safeguard
    mean_weighted_specificity = mean(weighted_specificities, na.rm = TRUE)  
  )
  
  # Summary print statement
  cat(sprintf("Repeat %d Summary | Mean AUC: %.3f | Mean Sens: %.3f | Mean Spec: %.3f\n\n",
              rep, repeat_summary$mean_weighted_auc[rep],
              repeat_summary$mean_weighted_sensitivity[rep],
              repeat_summary$mean_weighted_specificity[rep]))
}

# Making table of results

# Long-format data frame for the table that calculates the final average across
# all repeats for each metric.
cv_summary_long <- data.frame(
  Metric = c(
    "Weighted AUC",
    "Optimal Threshold",
    "Sensitivity",
    "Specificity",
    "Weighted Accuracy"
  ),
  Value = c(
    mean(repeat_summary$mean_weighted_auc),
    mean(repeat_summary$mean_optimal_threshold),
    mean(repeat_summary$mean_weighted_sensitivity),
    mean(repeat_summary$mean_weighted_specificity),
    (1 - mean(repeat_summary$mean_weighted_error)) # Convert error to accuracy
  )
)

# Build the final gt table
cv_summary_tbl <- cv_summary_long %>%
  gt() %>%
  # Add the main title and subtitle
  tab_header(
    title = md("**Results of 5-fold Cross-Validation Repeated 10 Times**"),
    subtitle = "Fitting Progression Model 4 to Imputation 1"
  ) %>%
  # Format the numbers in the 'Value' column
  fmt_number(
    columns = Value,
    rows = 1:2, # Format AUC and Threshold as numbers
    decimals = 3
  ) %>%
  fmt_percent(
    columns = Value,
    rows = 3:5, # Format Sens, Spec, and Accuracy as percentages
    decimals = 2
  ) %>%
  # Hide the default column labels to keep the table clean
  cols_label(
    Metric = "",
    Value = ""
  ) %>%
  # Add a source note for the overall method
  tab_source_note(
    source_note = "All metrics are averages from 10-times repeated 5-fold cross-validation."
  ) %>%
  # Add a specific source note explaining the thresholding method
  tab_source_note(
    source_note = "Optimal threshold, sensitivity, specificity, and accuracy were calculated at the threshold that maximises Youden's J statistic."
  ) %>%
  tab_options(
    table.width = px(600) 
  )

# Display table
cv_summary_tbl

# ----- Visualising ROC Curve ----- #

# This helps to convey what maximising Youden’s J statistic is doing

# The following code plots the ROC curve from the final loop in the cross validation,
# with the optimal threshold highlighted whose coordinates were found by maximising
# Youden's J statistic.

# Find the exact coordinates of the optimal point
optimal_point_coords <- roc_obj[which.max(roc_obj$YoudenJ), ]

# Plotting
roc_plot <- ggplot(roc_obj, aes(x = FPR, y = TPR)) +
  
  # 1. Draw a line connecting all the points
  geom_line(color = "#1f77b4", linewidth = 1) +
  
  # 2. Add the diagonal reference line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  
  # 3. Highlight single optimal point in orange
  geom_point(data = optimal_point_coords, 
             aes(x = FPR, y = TPR), 
             color = "#ff7f0e", 
             size = 5,
             shape = 18) +
  
  # Add a label for the optimal point
  annotate("text", x = optimal_point_coords$FPR, y = optimal_point_coords$TPR - 0.05,
           label = "Optimal Threshold",
           color = "#ff7f0e") +
  
  # Annotate the plot with this fold's AUC value
  annotate("text", x = 0.75, y = 0.25,
           label = paste("AUC =", round(weighted_aucs[k], 3)),
           size = 5) +
  
  # Add labels and a title
  labs(
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    title = "ROC Curve with Optimal Threshold Highlighted"
  ) 

# Display the plot
roc_plot
