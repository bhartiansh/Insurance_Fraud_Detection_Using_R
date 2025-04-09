library(readxl)
library(dplyr)
library(janitor)
library(openxlsx)


# Step 1: Load data
df <- read_excel("insurance_claims.xlsx")
print(table(df$fraud_reported))

# Step 2: Clean column names
df <- janitor::clean_names(df)

# Step 3: Replace "?", "NULL", "NaN", "null" only in character columns
df <- df %>%
  mutate(across(where(is.character), ~ na_if(., "?"))) %>%
  mutate(across(where(is.character), ~ na_if(., "NULL"))) %>%
  mutate(across(where(is.character), ~ na_if(., "NaN"))) %>%
  mutate(across(where(is.character), ~ na_if(., "null")))

# Step 4: Remove only rows with more than 5 NA values
df <- df[rowSums(is.na(df)) <= 5, ]

# Step 5: Remove irrelevant/problematic columns (excluding already missing ones)
irrelevant_cols <- c(
  "insured_zip", "insured_hobbies", "collision_type",
  "incident_city", "incident_location", "incident_hour_of_the_day",
  "number_of_vehicles_involved", "auto_model", "capital_gains"
)
df <- df %>% select(-any_of(irrelevant_cols))  # safer than one_of()

# Step 6: Drop rows with NA in target column
df <- df %>% filter(!is.na(fraud_reported))

# Step 7: Convert target column to binary factor
df$fraud_reported <- factor(df$fraud_reported, levels = c("no", "yes"), labels = c("no", "yes"))

# Step 8: Check class distribution
print(table(df$fraud_reported))

write.xlsx(df, "cleaned_insurance_claims.xlsx")

