library(ggplot2)
library(lattice)
library(caret)
library(randomForest)
library(pROC)

# ---------------------------------------
# Step 1: Convert target to factor
# ---------------------------------------
df$fraud_reported <- as.factor(df$fraud_reported)

# ---------------------------------------
# Step 2: Split data into train/test sets
# ---------------------------------------
set.seed(123)
train_index <- createDataPartition(df$fraud_reported, p = 0.7, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]

# ---------------------------------------
# Step 3: Train Random Forest model
# ---------------------------------------
rf_model <- randomForest(fraud_reported ~ ., data = train, importance = TRUE)

# ---------------------------------------
# Step 4: Predict on test set
# ---------------------------------------
predictions <- predict(rf_model, newdata = test)

# ---------------------------------------
# Step 5: Evaluate model - Confusion Matrix
# ---------------------------------------
conf_matrix <- confusionMatrix(predictions, test$fraud_reported)
print(conf_matrix)

# ---------------------------------------
# Step 6: ROC Curve and AUC
# ---------------------------------------
# Get predicted probabilities
prob_predictions <- predict(rf_model, newdata = test, type = "prob")

# Build ROC object
roc_obj <- roc(response = test$fraud_reported, predictor = prob_predictions[, "Y"])
plot(roc_obj, col = "blue", main = "ROC Curve - Random Forest")
auc_value <- auc(roc_obj)
cat("AUC:", auc_value, "\n")

# ---------------------------------------
# Step 7: Save the trained model (optional)
# ---------------------------------------
saveRDS(rf_model, "fraud_model_rf.rds")

