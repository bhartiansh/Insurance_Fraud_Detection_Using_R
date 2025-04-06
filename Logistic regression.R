# 📦 Load required packages
library(readxl)
library(dplyr)
library(caret)
library(ggplot2)
library(pROC)

# 📂 Read the Excel file
data <- read_excel("insurance_fraud_data.xlsx")

# 🧹 Data Preprocessing
data <- data %>%
  mutate(fraud_reported = as.factor(fraud_reported),
         policy_bind_date = as.Date(policy_bind_date),
         incident_date = as.Date(incident_date)) %>%
  select(-policy_number, -insured_zip, -incident_location)  # drop irrelevant columns

# Replace "-" in column names for R compatibility
colnames(data) <- make.names(colnames(data))

# Convert character columns to factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

# 🚂 Split the data into training and testing sets
set.seed(123)
split <- createDataPartition(data$fraud_reported, p = 0.7, list = FALSE)
train <- data[split, ]
test <- data[-split, ]

# 🤖 Train the Logistic Regression model
log_model <- glm(fraud_reported ~ ., data = train, family = binomial)

# 📊 Summary of the model
summary(log_model)

# 🔮 Predict on test set
pred_probs <- predict(log_model, test, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, "Y", "N")
pred_classes <- as.factor(pred_classes)

# 📈 Evaluation
conf_mat <- confusionMatrix(pred_classes, test$fraud_reported)
print(conf_mat)

# ROC Curve
roc_obj <- roc(test$fraud_reported, pred_probs, levels = c("N", "Y"))
plot(roc_obj, col = "blue", main = "ROC Curve - Logistic Regression")
auc(roc_obj)

