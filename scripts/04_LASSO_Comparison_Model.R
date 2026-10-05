# ============================================================
# Member 4 - Task 5b
# LASSO Logistic Regression Comparison Model
# ============================================================

# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(glmnet)
library(pROC)
library(caret)

# ------------------------------------------------------------
# 2. Load the same train/test data used in Task 5a
# ------------------------------------------------------------

train_data <- readRDS("data/train_5a.rds")
test_data <- readRDS("data/test_5a.rds")

# Check the data
dim(train_data)
dim(test_data)

str(train_data)
str(test_data)

# ------------------------------------------------------------
# 3. Convert predictors into a numerical matrix
# ------------------------------------------------------------

combined_data <- rbind(
  train_data,
  test_data
)

# Create predictor matrix
x_all <- model.matrix(
  churn ~ .,
  data = combined_data
)[, -1]

# Create outcome vector
y_all <- combined_data$churn

# Identify which rows belong to training and testing
n_train <- nrow(train_data)

x_train <- x_all[1:n_train, ]
x_test <- x_all[(n_train + 1):nrow(x_all), ]

y_train <- y_all[1:n_train]
y_test <- y_all[(n_train + 1):length(y_all)]

# Check dimensions
dim(x_train)
dim(x_test)
length(y_train)
length(y_test)

# ------------------------------------------------------------
# 4. Fit LASSO Logistic Regression
# ------------------------------------------------------------

set.seed(42)

lasso_cv <- cv.glmnet(
  x_train,
  y_train,
  family = "binomial",
  alpha = 1,
  type.measure = "deviance",
  nfolds = 10
)

# Display the cross-validation results
lasso_cv

# Best lambda value
lasso_cv$lambda.min

# Lambda using the 1-standard-error rule
lasso_cv$lambda.1se

# ------------------------------------------------------------
# 5. Plot LASSO cross-validation results
# ------------------------------------------------------------

plot(lasso_cv)

# ------------------------------------------------------------
# 6. Examine LASSO coefficients
# ------------------------------------------------------------

lasso_coefficients <- coef(
  lasso_cv,
  s = "lambda.min"
)

lasso_coefficients

# ------------------------------------------------------------
# 6b. Examine coefficients at lambda.1se
# ------------------------------------------------------------

lasso_coefficients_1se <- coef(
  lasso_cv,
  s = "lambda.1se"
)

lasso_coefficients_1se

# ------------------------------------------------------------
# 6c. LASSO Variable Selection Summary
# ------------------------------------------------------------

lasso_coef_table <- as.matrix(lasso_coefficients_1se)

lasso_variable_selection <- data.frame(
  Variable = rownames(lasso_coef_table),
  Coefficient = as.numeric(lasso_coef_table[, 1])
)

lasso_variable_selection$Selected <- ifelse(
  lasso_variable_selection$Coefficient != 0,
  "Yes",
  "No"
)

lasso_variable_selection

# ------------------------------------------------------------
# 7. Predict churn probabilities on the test set
# ------------------------------------------------------------

lasso_prob <- predict(
  lasso_cv,
  newx = x_test,
  s = "lambda.min",
  type = "response"
)

lasso_prob <- as.vector(lasso_prob)

summary(lasso_prob)

# ------------------------------------------------------------
# 8. Convert probabilities into predicted classes
# ------------------------------------------------------------

lasso_pred <- ifelse(
  lasso_prob >= 0.5,
  1,
  0
)

lasso_pred <- factor(
  lasso_pred,
  levels = c(0, 1)
)

y_test <- factor(
  y_test,
  levels = c(0, 1)
)

# ------------------------------------------------------------
# 9. Confusion Matrix
# ------------------------------------------------------------

lasso_cm <- confusionMatrix(
  lasso_pred,
  y_test,
  positive = "1",
  mode = "everything"
)

lasso_cm

# ------------------------------------------------------------
# 10. Classification Metrics
# ------------------------------------------------------------

lasso_accuracy <- lasso_cm$overall["Accuracy"]

lasso_precision <- lasso_cm$byClass["Pos Pred Value"]

lasso_recall <- lasso_cm$byClass["Sensitivity"]

lasso_specificity <- lasso_cm$byClass["Specificity"]

lasso_f1 <- 2 * (
  lasso_precision * lasso_recall
) / (
  lasso_precision + lasso_recall
)

lasso_accuracy
lasso_precision
lasso_recall
lasso_specificity
lasso_f1

# ------------------------------------------------------------
# 11. ROC Curve and AUC
# ------------------------------------------------------------

lasso_roc <- roc(
  y_test,
  lasso_prob,
  levels = c("0", "1"),
  direction = "<"
)

lasso_auc <- auc(lasso_roc)

lasso_auc

plot(
  lasso_roc,
  main = "ROC Curve - LASSO Logistic Regression"
)

# ------------------------------------------------------------
# 11b. Alternative Classification Threshold - Youden
# ------------------------------------------------------------

# First predict probabilities on the TRAINING data
# This is used only to choose the classification threshold

lasso_prob_train <- predict(
  lasso_cv,
  newx = x_train,
  s = "lambda.min",
  type = "response"
)

lasso_prob_train <- as.vector(lasso_prob_train)

# Create ROC curve using TRAINING data
lasso_roc_train <- roc(
  y_train,
  lasso_prob_train,
  levels = c("0", "1"),
  direction = "<"
)

# Find Youden threshold using TRAINING data
lasso_threshold <- coords(
  lasso_roc_train,
  "best",
  best.method = "youden",
  ret = "threshold"
)

lasso_threshold <- as.numeric(lasso_threshold)

lasso_threshold

# ------------------------------------------------------------
# Apply the training-selected threshold to TEST data
# ------------------------------------------------------------

lasso_pred_youden <- factor(
  ifelse(
    lasso_prob >= lasso_threshold,
    1,
    0
  ),
  levels = c(0, 1)
)

# Confusion matrix
lasso_cm_youden <- confusionMatrix(
  lasso_pred_youden,
  y_test,
  positive = "1",
  mode = "everything"
)

lasso_cm_youden

# Extract metrics
lasso_accuracy_youden <- lasso_cm_youden$overall["Accuracy"]

lasso_precision_youden <- lasso_cm_youden$byClass["Pos Pred Value"]

lasso_recall_youden <- lasso_cm_youden$byClass["Sensitivity"]

lasso_specificity_youden <- lasso_cm_youden$byClass["Specificity"]

lasso_f1_youden <- 2 * (
  lasso_precision_youden * lasso_recall_youden
) / (
  lasso_precision_youden + lasso_recall_youden
)

lasso_accuracy_youden
lasso_precision_youden
lasso_recall_youden
lasso_specificity_youden
lasso_f1_youden

# ------------------------------------------------------------
# 12. Final LASSO Performance Summary
# ------------------------------------------------------------

lasso_results <- data.frame(
  Model = "LASSO Logistic Regression",
  Accuracy = as.numeric(lasso_accuracy),
  Precision = as.numeric(lasso_precision),
  Recall = as.numeric(lasso_recall),
  F1 = as.numeric(lasso_f1),
  AUC = as.numeric(lasso_auc)
)

lasso_results

# ------------------------------------------------------------
# 13. Model Comparison - Threshold 0.5
# ------------------------------------------------------------

comparison_05 <- data.frame(
  Model = c(
    "Logistic Regression",
    "LASSO Logistic Regression"
  ),
  
  Threshold = c(
    0.5,
    0.5
  ),
  
  Accuracy = c(
    0.8204134,
    as.numeric(lasso_accuracy)
  ),
  
  Precision = c(
    0.6689655,
    as.numeric(lasso_precision)
  ),
  
  Recall = c(
    0.06147022,
    as.numeric(lasso_recall)
  ),
  
  F1 = c(
    0.11259,
    as.numeric(lasso_f1)
  ),
  
  AUC = c(
    0.7471,
    as.numeric(lasso_auc)
  )
)

comparison_05

comparison_youden <- data.frame(
  Model = c(
    "Logistic Regression",
    "LASSO Logistic Regression"
  ),
  
  Threshold = c(
    0.2083598,
    lasso_threshold
  ),
  
  Accuracy = c(
    0.7539,
    as.numeric(lasso_accuracy_youden)
  ),
  
  Precision = c(
    0.3939,
    as.numeric(lasso_precision_youden)
  ),
  
  Recall = c(
    0.6084,
    as.numeric(lasso_recall_youden)
  ),
  
  F1 = c(
    0.4782,
    as.numeric(lasso_f1_youden)
  )
)

comparison_youden

# ------------------------------------------------------------
# 14. Save Final LASSO Outputs
# ------------------------------------------------------------

write.csv(
  lasso_results,
  "reports/member4_lasso_results.csv",
  row.names = FALSE
)

write.csv(
  lasso_variable_selection,
  "reports/member4_lasso_variable_selection.csv",
  row.names = FALSE
)

write.csv(
  comparison_05,
  "reports/member4_model_comparison_05.csv",
  row.names = FALSE
)

write.csv(
  comparison_youden,
  "reports/member4_model_comparison_youden.csv",
  row.names = FALSE
)

saveRDS(
  lasso_cv,
  "reports/member4_lasso_model.rds"
)

saveRDS(
  lasso_threshold,
  "data/lasso_threshold_youden.rds"
)

# ------------------------------------------------------------
# 15. Final Model Comparison Summary
# ------------------------------------------------------------

final_comparison <- data.frame(
  Model = c(
    "Logistic Regression",
    "LASSO Logistic Regression",
    "Logistic Regression",
    "LASSO Logistic Regression"
  ),
  
  Threshold_Type = c(
    "Default 0.5",
    "Default 0.5",
    "Youden",
    "Youden"
  ),
  
  Threshold = c(
    0.5,
    0.5,
    0.2083598,
    lasso_threshold
  ),
  
  Accuracy = c(
    0.8204134,
    as.numeric(lasso_accuracy),
    0.7539,
    as.numeric(lasso_accuracy_youden)
  ),
  
  Precision = c(
    0.6689655,
    as.numeric(lasso_precision),
    0.3939,
    as.numeric(lasso_precision_youden)
  ),
  
  Recall = c(
    0.06147022,
    as.numeric(lasso_recall),
    0.6084,
    as.numeric(lasso_recall_youden)
  ),
  
  F1 = c(
    0.11259,
    as.numeric(lasso_f1),
    0.4782,
    as.numeric(lasso_f1_youden)
  ),
  
  AUC = c(
    0.7471,
    as.numeric(lasso_auc),
    0.7471,
    as.numeric(lasso_auc)
  )
)

final_comparison

write.csv(
  final_comparison,
  "reports/member4_final_model_comparison.csv",
  row.names = FALSE
)