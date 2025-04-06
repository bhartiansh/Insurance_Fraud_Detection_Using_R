# Load libraries
library(caret)
library(randomForest)
library(dplyr)

# ----------------------------
# Step 1: Remove Irrelevant Features
# ----------------------------
df <- df[, !names(df) %in% c("policy_number", "insured_zip", "incident_location")]

# ----------------------------
# Step 2: Remove Low Variance Features
# ----------------------------
nzv <- nearZeroVar(df, saveMetrics = TRUE)
low_var_cols <- rownames(nzv[nzv$nzv == TRUE, ])
df <- df[, !names(df) %in% low_var_cols]

# ----------------------------
# Step 3: Remove Highly Correlated Numeric Features
# ----------------------------
numeric_cols <- df[sapply(df, is.numeric)]
numeric_cols <- numeric_cols[, colSums(is.na(numeric_cols)) < nrow(numeric_cols)]  # Remove full-NA columns
cor_matrix <- cor(na.omit(numeric_cols))

# Find highly correlated features (cutoff = 0.9)
high_cor <- findCorrelation(cor_matrix, cutoff = 0.9)
df <- df[, !names(df) %in% names(numeric_cols)[high_cor]]

# ----------------------------
# Step 4: Feature Importance using Random Forest
# ----------------------------

# Ensure target variable is a factor
df$fraud_reported <- as.factor(df$fraud_reported)

# Split data into training and test set
set.seed(123)
train_index <- createDataPartition(df$fraud_reported, p = 0.7, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]

# Fix column names to replace - and spaces with _
names(df) <- gsub("-", "_", names(df))
names(df) <- gsub(" ", "_", names(df))

# Train Random Forest model
rf_model <- randomForest(fraud_reported ~ ., data = train, importance = TRUE)

# View Feature Importance
importance_df <- importance(rf_model)
print(importance_df)
varImpPlot(rf_model, main = "Feature Importance (Random Forest)")

# ----------------------------
# Step 5: Recursive Feature Elimination (Optional)
# ----------------------------
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

rfe_result <- rfe(train[, -which(names(train) == "fraud_reported")],
                  train$fraud_reported,
                  sizes = c(5, 10, 15, 20),
                  rfeControl = ctrl)

# Best features suggested
print(rfe_result$optVariables)
plot(rfe_result, type = c("g", "o"), main = "Recursive Feature Elimination Result")

