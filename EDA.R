# Load required libraries
library(readxl)
library(ggplot2)
library(DataExplorer)
library(corrplot)
library(lubridate)

# Load data from Excel file
df <- read_excel("insurance_fraud_data.xlsx")  # <-- replace with your file name

# Check structure and summary
str(df)
summary(df)

# Convert date columns
df$policy_bind_date <- dmy(df$policy_bind_date)
df$incident_date <- dmy(df$incident_date)

# Convert relevant columns to factors
factor_cols <- c("insured_sex", "insured_education_level", "insured_occupation",
                 "insured_hobbies", "insured_relationship", "incident_type",
                 "collision_type", "incident_severity", "authorities_contacted",
                 "incident_state", "incident_city", "property_damage",
                 "police_report_available", "auto_make", "auto_model",
                 "fraud_reported")

df[factor_cols] <- lapply(df[factor_cols], as.factor)

# ---------------------------------------
# 🔍 Univariate Analysis
# ---------------------------------------

# Fraud Count
ggplot(df, aes(fraud_reported)) +
  geom_bar(fill = "darkred") +
  labs(title = "Fraud vs Non-Fraud Count", x = "Fraud Reported", y = "Count")

# Age Distribution
ggplot(df, aes(age)) +
  geom_histogram(fill = "skyblue", bins = 20) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# Policy Annual Premium
ggplot(df, aes(policy_annual_premium)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(title = "Annual Premium Distribution", x = "Annual Premium", y = "Count")

# Total Claim Amount
ggplot(df, aes(total_claim_amount)) +
  geom_histogram(fill = "lightgreen", bins = 30) +
  labs(title = "Total Claim Amount Distribution", x = "Total Claim", y = "Count")

# ---------------------------------------
# 🔍 Bivariate Analysis
# ---------------------------------------

# Fraud vs. Gender
ggplot(df, aes(insured_sex, fill = fraud_reported)) +
  geom_bar(position = "dodge") +
  labs(title = "Fraud by Gender", x = "Gender", y = "Count")

# Fraud by Occupation
ggplot(df, aes(insured_occupation, fill = fraud_reported)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  labs(title = "Fraud by Occupation", x = "Occupation", y = "Count")

# Fraud by Incident Type
ggplot(df, aes(incident_type, fill = fraud_reported)) +
  geom_bar(position = "dodge") +
  labs(title = "Fraud by Incident Type", x = "Incident Type", y = "Count")

# Boxplot: Total Claim by Fraud
ggplot(df, aes(fraud_reported, total_claim_amount, fill = fraud_reported)) +
  geom_boxplot() +
  labs(title = "Total Claim by Fraud Status", x = "Fraud Reported", y = "Total Claim")

# Scatter plot: Age vs Total Claim colored by Fraud
ggplot(df, aes(age, total_claim_amount, color = fraud_reported)) +
  geom_point(alpha = 0.6) +
  labs(title = "Age vs Total Claim by Fraud", x = "Age", y = "Total Claim Amount")

# ---------------------------------------
# 🔍 Correlation Matrix (Numeric Columns)
# ---------------------------------------

# Select numeric columns only
numeric_cols <- df[sapply(df, is.numeric)]

# Remove columns with all NA values
numeric_cols <- numeric_cols[, colSums(is.na(numeric_cols)) < nrow(numeric_cols)]

# Remove rows with NA for correlation
numeric_clean <- na.omit(numeric_cols)

# Plot correlation heatmap
cor_matrix <- cor(numeric_clean)
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.8)

# ---------------------------------------
# 🔍 Auto EDA Report (Optional)
# ---------------------------------------
create_report(df)

