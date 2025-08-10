# -------------------------------------------- #
# Exploratory data analysis prior to modelling
# -------------------------------------------- #

# Creating a custom theme 
theme_dissertation <- function() {
  theme_minimal() + # Start with minimal theme
    theme(
      # --- Plot Border ---
      # Allow for border around the figure but not around the plotting area
      plot.background = element_rect(color = "black", linewidth = 1),
      panel.border = element_blank(),
      
      # --- Axis Lines ---
      # Add solid black axis lines for x and y
      axis.line = element_line(color = "black", linewidth = 0.5),
      
      # --- Gridlines ---
      # Dotted horizontal gridlines, no vertical gridlines
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
      panel.grid.minor.y = element_blank(),
      
      # --- Legend ---
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11),
      
      # --- Text Elements ---
      # Bold title
      plot.title = element_text(face = "bold")
    )
}

# Storing imputation 1
imp1 <-imputed_1118$imp1

# --- Distributions of the outcome in continuous form (HbA1c) and the main predictor of interest (ALMI) --- #

# Using all imputations for these
# Bind all 30 imputed datasets into one large dataframe
all_imputed_data <- bind_rows(imputed_1118, .id = "imputation_id")

# Density plot for HbA1c
ggplot(all_imputed_data, aes(x = Hba1c_perc, group = imputation_id)) +
  geom_density(data = . %>% filter(imputation_id != "imp1"), alpha = .5, colour = "grey") +
  geom_density(data = . %>% filter(imputation_id == "imp1"), linewidth = 1, colour = "darkblue") +
  labs(title = "Distribution of Glycated Haemoglobin (HbA1c)", x = "HbA1c (%)", y = "Density") +
  theme_dissertation()

# Density plot for ALMI (Predictor)
ggplot(all_imputed_data, aes(x = Almi, group = imputation_id)) +
  geom_density(data = . %>% filter(imputation_id != "imp1"), alpha = .5, colour = "grey") +
  geom_density(data = . %>% filter(imputation_id == "imp1"), linewidth = 1, colour = "darkblue") +
  labs(title = "Distribution of Appendicular Lean Mass Index (ALMI)", x = "ALMI (kg/m²)", y = "Density") +
  theme_dissertation()

# --- Frequency of categorical variables --- #

# Bar chart for Gender predictor
ggplot(imp1, aes(x = as.factor(Gender))) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  labs(title = "Frequency of Genders", x = "Gender", y = "Count") +
  theme_dissertation()

# Bar chart for the onset model outcome 'at_risk_or_worse'
ggplot(imp1, aes(x = as.factor(at_risk_or_worse))) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  scale_x_discrete(labels=c("0" = "Healthy", "1" = "Dysglycemia")) +
  labs(title = "Frequency of Dysglycemia Status", x = "Blood Glucose Status", y = "Count") +
  theme_dissertation()

# Bar chart for the progression model outcome 'is_diabetic'
ggplot(imp1, aes(x = as.factor(is_diabetic))) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  scale_x_discrete(labels=c("0" = "Non-Diabetic", "1" = "Diabetic")) +
  labs(title = "Frequency of Diabetic Status", x = "Blood Glucose Status", y = "Count") +
  theme_dissertation()

# Bar chart for Alcohol_Status predictor
ggplot(imp1, aes(x = as.factor(Alcohol_Status))) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  labs(title = "Frequency of Alcohol Status Categories", x = "Alcohol Status", y = "Count") +
  theme_dissertation()

# Bar chart for Smoking_Status predictor
ggplot(imp1, aes(x = as.factor(Smoking_Status))) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  labs(title = "Frequency of Smoking Status Categories", x = "Smoking Status", y = "Count") +
  theme_dissertation()

# --- Explore how relationships differ across categories using box plots --- # 

# Box plot of HbA1c by Sex
ggplot(imp1, aes(x = Gender, y = Hba1c_perc, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(labels=c("1" = "Male", "2" = "Female")) +
  labs(title = "HbA1c by Sex", x = "Sex", y = "HbA1c (%)") +
  theme_dissertation()

# Box plot of ALMI by Sex
ggplot(imp1, aes(x = Gender, y = Almi, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(labels=c("1" = "Male", "2" = "Female")) +
  labs(title = "ALMI by Sex", x = "Sex", y = "ALMI (kg/m²)") +
  theme_dissertation()

# Box plot of HbA1c by Race/Ethnicity
ggplot(imp1, aes(x = Race, y = Hba1c_perc, fill = Race)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for readability
  labs(title = "HbA1c by Race/Ethnicity", x = "Race/Ethnicity", y = "HbA1c (%)") +
  theme_dissertation()

# Box plot of ALMI by Race/Ethnicity
ggplot(imp1, aes(x = Race, y = Almi, fill = Race)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "ALMI by Race/Ethnicity", x = "Race/Ethnicity", y = "ALMI (kg/m²)") +
  theme_dissertation()

# --- Visually inspect the relationship between ALMI and HbA1c --- #

# Scatter plot of HbA1c vs. ALMI
ggplot(imp1, aes(x = Almi, y = Hba1c_perc)) +
  geom_point(alpha = 0.4) + # Use alpha for transparency to see dense areas
  geom_smooth(method = "lm", color = "red") + # Add a linear model trendline
  labs(title = "HbA1c vs. ALMI",
       x = "Appendicular Lean Mass Index (kg/m²)",
       y = "Glycated Haemoglobin (HbA1c %)") +
  theme_dissertation()

# Scatter plot of HbA1c vs. ALMI by Gender
ggplot(imp1, aes(x = Almi, y = Hba1c_perc, colour=Gender)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", linewidth = 1) + 
  labs(title = "Glycated Haemoglobin vs. Appendicular Lean Mass Index by Gender",
       x = "ALMI (kg/m²)",
       y = "HbA1c (%)") +
  theme_dissertation()