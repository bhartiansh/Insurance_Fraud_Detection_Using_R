# Load required libraries
library(readxl)
library(dplyr)
library(caret)

# Step 1: Load cleaned dataset
df <- readxl::read_excel("cleaned_insurance_claims.xlsx")

# Step 2: Ensure target variable is a factor (binary)
df$fraud_reported <- as.factor(df$fraud_reported)

# Step 3: Remove problematic column (if exists)
if ("capital.gains" %in% colnames(df)) {
  df <- df %>% select(-capital.gains)
}

# Step 4: Train-test split (80-20)
set.seed(123)
split <- createDataPartition(df$fraud_reported, p = 0.9, list = FALSE)
train <- df[split, ]
test <- df[-split, ]

# Step 5: Ensure test and train have same columns
common_cols <- intersect(names(train), names(test))
train <- train[, common_cols]
test <- test[, common_cols]

# Step 6: Train logistic regression model
log_model <- glm(fraud_reported ~ ., data = train, family = binomial)

# Step 7: Predict probabilities on test set
pred_probs <- predict(log_model, newdata = test, type = "response")

# Step 8: Convert probabilities to class (threshold = 0.5)
pred_class <- ifelse(pred_probs >= 0.5, "1", "0")
pred_class <- as.factor(pred_class)

# Step 9: Evaluate with confusion matrix
conf_matrix <- confusionMatrix(pred_class, test$fraud_reported)
print(conf_matrix)

