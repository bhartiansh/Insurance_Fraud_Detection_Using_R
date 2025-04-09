# Load required libraries
library(readxl)
library(dplyr)
library(caret)
library(randomForest)

# Step 1: Load cleaned dataset
df <- read_excel("cleaned_insurance_claims.xlsx")

# Step 2: Drop problematic column if it exists
if ("capital-gains" %in% colnames(df)) {
  df <- df %>% select(-`capital-gains`)
} else if ("capital.gains" %in% colnames(df)) {
  df <- df %>% select(-capital.gains)
}

# Step 3: Convert target to factor
df$fraud_reported <- as.factor(df$fraud_reported)

# Step 4: Remove rows with NA
df <- na.omit(df)

# Step 5: Convert character columns to factors
cat_cols <- sapply(df, is.character)
df[, cat_cols] <- lapply(df[, cat_cols], as.factor)

# Step 6: Train/test split
set.seed(123)
split <- createDataPartition(df$fraud_reported, p = 0.9, list = FALSE)
train_data <- df[split, ]
test_data <- df[-split, ]

# Step 7: Train Random Forest
rf_model <- randomForest(fraud_reported ~ ., data = train_data, ntree = 200, importance = TRUE)

# Step 8: Training set prediction
train_pred <- predict(rf_model, train_data)
train_conf <- confusionMatrix(train_pred, train_data$fraud_reported)
cat("Training Accuracy:\n")
print(train_conf$overall['Accuracy'])

# Step 9: Test set prediction
test_pred <- predict(rf_model, test_data)
test_conf <- confusionMatrix(test_pred, test_data$fraud_reported)
cat("Test Accuracy:\n")
print(test_conf$overall['Accuracy'])

