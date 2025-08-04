# -------------------------------------------- #
# Exploratory data analysis prior to modelling
# -------------------------------------------- #

# Using the imputation 1 dataset

imp1 <- imputed_1118$imp1

# --- Distributions of the outcome in continuous form (HbA1c) and the main predictor of interest (ALMI) --- # 

# Histogram and density plot for HbA1c (Outcome)
ggplot(imp1, aes(x = Hba1c_perc)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.2, fill = "lightblue", color = "black") +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(title = "Distribution of HbA1c", x = "HbA1c (%)", y = "Density") +
  theme_minimal()

# Histogram and density plot for ALMI (Predictor)
ggplot(imp1, aes(x = Almi)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "lightgreen", color = "black") +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(title = "Distribution of Appendicular Lean Mass Index (Almi)", x = "Almi (kg/m²)", y = "Density") +
  theme_minimal()

# --- Frequency of categorical variables --- #

# Bar chart for Gender predictor
ggplot(imp1, aes(x = as.factor(Gender))) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  labs(title = "Frequency of Genders", x = "Gender", y = "Count")

# Bar chart for the onset model outcome 'at_risk_or_worse'
ggplot(imp1, aes(x = as.factor(at_risk_or_worse))) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  scale_x_discrete(labels=c("0" = "Healthy", "1" = "Dysglycemia")) +
  labs(title = "Frequency of Dysglycemia Status", x = "Blood Glucose Status", y = "Count")

# Bar chart for the progression model outcome 'is_diabetic'
ggplot(imp1, aes(x = as.factor(is_diabetic))) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  scale_x_discrete(labels=c("0" = "Non-Diabetic", "1" = "Diabetic")) +
  labs(title = "Frequency of Diabetic Status", x = "Blood Glucose Status", y = "Count")

# Bar chart for Alcohol_Status predictor
ggplot(imp1, aes(x = as.factor(Alcohol_Status))) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  labs(title = "Frequency of Alcohol Status Categories", x = "Alcohol Status", y = "Count")

# Bar chart for Smoking_Status predictor
ggplot(imp1, aes(x = as.factor(Smoking_Status))) +
  geom_bar(fill = "steelblue", color = "black", size = 1) +
  labs(title = "Frequency of Smoking Status Categories", x = "Smoking Status", y = "Count")

# --- Explore how relationships differ across categories using box plots --- # 

# Box plot of HbA1c by Sex
ggplot(imp1, aes(x = Gender, y = Hba1c_perc, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(labels=c("1" = "Male", "2" = "Female")) +
  labs(title = "HbA1c by Sex", x = "Sex", y = "HbA1c (%)")

# Box plot of ALMI by Sex
ggplot(imp1, aes(x = Gender, y = Almi, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(labels=c("1" = "Male", "2" = "Female")) +
  labs(title = "ALMI by Sex", x = "Sex", y = "ALMI (kg/m²)")

# Box plot of HbA1c by Race/Ethnicity
ggplot(imp1, aes(x = Race, y = Hba1c_perc, fill = Race)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for readability
  labs(title = "HbA1c by Race/Ethnicity", x = "Race/Ethnicity", y = "HbA1c (%)")

# Box plot of ALMI by Race/Ethnicity
ggplot(imp1, aes(x = Race, y = Almi, fill = Race)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "ALMI by Race/Ethnicity", x = "Race/Ethnicity", y = "ALMI (kg/m²)")

# --- Visually inspect the relationship between ALMI and HbA1c --- #

# Scatter plot of HbA1c vs. ALMI
ggplot(imp1, aes(x = Almi, y = Hba1c_perc)) +
  geom_point(alpha = 0.4) + # Use alpha for transparency to see dense areas
  geom_smooth(method = "lm", color = "red") + # Add a linear model trendline
  labs(title = "HbA1c vs. ALMI",
       x = "Appendicular Lean Mass Index (kg/m²)",
       y = "Glycated Hemoglobin (HbA1c %)")

# Scatter plot of HbA1c vs. ALMI by Gender
ggplot(imp1, aes(x = Almi, y = Hba1c_perc, colour=Gender)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "Glycated Hemoglobin vs. Appendicular Lean Mass Index by Gender",
       x = "ALMI (kg/m²)",
       y = "HbA1c (%)")