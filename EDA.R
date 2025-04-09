# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(DataExplorer)
library(stringr)

# Load the data
df <- read_excel("insurance_claims.xlsx")

# Clean column names for easier handling
df <- clean_names(df)

# Quick overview
glimpse(df)
summary(df)

# Check missing values
plot_missing(df)

# Separate numerical and categorical columns
numeric_cols <- df %>% select(where(is.numeric)) %>% colnames()
non_numeric_cols <- df[, !sapply(df, is.numeric)] %>% colnames()
# Print column types
cat("Numerical columns:\n")
print(numeric_cols)

cat("\nNon-numerical columns:\n")
print(non_numeric_cols)

# Check one-hot encoded fields (e.g., in_ured_, colli_ion_type, etc.)
one_hot_groups <- unique(str_extract(colnames(df), "^(policy_state|in_ured|incident_type|incident_city|incident_severity|authorities_contacted|collision_type|auto_make|number_of_vehicle|in_ured_occupation|in_ured_relation_hip|in_ured_education_level|in_ured_hobbie_)"))
one_hot_groups <- one_hot_groups[!is.na(one_hot_groups)]

cat("\nDetected one-hot encoded feature groups:\n")
print(one_hot_groups)

# EDA for numeric columns
plot_histogram(df[, numeric_cols])
plot_density(df[, numeric_cols])
plot_correlation(df[, numeric_cols], type = "c")

# EDA for target variable
if ("fraud_reported" %in% colnames(df)) {
  df$fraud_reported <- as.factor(df$fraud_reported)
  cat("\nTarget variable class distribution:\n")
  print(table(df$fraud_reported))
  
  ggplot(df, aes(fraud_reported)) +
    geom_bar(fill = "#4e79a7") +
    theme_minimal() +
    labs(title = "Class Distribution of Fraud Reported", x = "Fraud Reported", y = "Count")
}

# Optional: Profile the data
# CreateReport(df)

