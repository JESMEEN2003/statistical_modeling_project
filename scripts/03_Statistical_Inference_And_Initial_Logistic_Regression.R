# ============================================================
# Statistical Inference & Initial Logistic Regression
# ============================================================

#Load packages 
install.packages("effectsize")
install.packages("FSA")

# ------------------------------------------------------------
# 1. Load the cleaned dataset
# ------------------------------------------------------------

data <- read.csv(
  "data/churn_prediction_clean.csv",
  stringsAsFactors = FALSE
)

# Inspect the data
str(data)
dim(data)

# Check churn distribution
table(data$churn)
prop.table(table(data$churn)) * 100

# ============================================================
# TASK 4 - STATISTICAL INFERENCE
# Hypothesis 1: Current Balance vs Churn
# ============================================================

# ------------------------------------------------------------
# 2.1 Descriptive statistics by churn group
# ------------------------------------------------------------

aggregate(
  current_balance ~ churn,
  data = data,
  FUN = function(x) {
    c(
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE)
    )
  }
)


# ------------------------------------------------------------
# 2.2 Check distribution of current balance
# ------------------------------------------------------------

hist(
  data$current_balance,
  main = "Distribution of Current Balance",
  xlab = "Current Balance"
)

# Boxplot by churn status
boxplot(
  current_balance ~ churn,
  data = data,
  main = "Current Balance by Churn Status",
  xlab = "Churn (0 = Retained, 1 = Churned)",
  ylab = "Current Balance"
)

# ------------------------------------------------------------
# 2.3 Check equality of variances
# ------------------------------------------------------------

var.test(
  current_balance ~ churn,
  data = data
)
library(car)
leveneTest(current_balance ~ factor(churn), data = data)

# ------------------------------------------------------------
# 2.4 Welch's independent-samples t-test
# ------------------------------------------------------------

t_test_balance <- t.test(
  current_balance ~ churn,
  data = data,
  var.equal = FALSE
)

t_test_balance


# ------------------------------------------------------------
# 2.5 Mann-Whitney U test
# ------------------------------------------------------------

wilcox_balance <- wilcox.test(
  current_balance ~ churn,
  data = data,
  exact = FALSE
)

wilcox_balance

library(effectsize)
cohens_d(current_balance ~ factor(churn), data = data)


# ============================================================
# Age vs Churn
# ============================================================

# Descriptive statistics
aggregate(
  age ~ churn,
  data = data,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
)

# Variance comparison
var.test(age ~ churn, data = data)

# Welch two-sample t-test
t.test(
  age ~ churn,
  data = data,
  var.equal = FALSE
)

# Wilcoxon rank-sum test
wilcox.test(
  age ~ churn,
  data = data
)
leveneTest(age ~ factor(churn), data = data)
cohens_d(age ~ factor(churn), data = data)

# ============================================================
# Churn vs Occupation
# ============================================================

# Create contingency table
occupation_churn <- table(data$occupation, data$churn)

occupation_churn

# Row percentages
prop.table(occupation_churn, margin = 1) * 100

# Chi-square test
chisq.test(occupation_churn)

cramers_v(occupation_churn)

# Expected frequencies
chisq_result <- chisq.test(occupation_churn)

chisq_result$expected

min(chisq_result$expected)

# ============================================================
# Gender vs Churn
# ============================================================

# Create contingency table
gender_churn <- table(data$gender, data$churn)

gender_churn

# Row percentages
prop.table(gender_churn, margin = 1) * 100

# Chi-square test
chisq_gender <- chisq.test(gender_churn)

chisq_gender

cramers_v(gender_churn)

# Expected frequencies
chisq_gender$expected

# Check minimum expected frequency
min(chisq_gender$expected)


# ============================================================
# Customer Net Worth Category vs Churn
# ============================================================

# Create contingency table
nw_churn <- table(data$customer_nw_category, data$churn)

nw_churn

# Row percentages
prop.table(nw_churn, margin = 1) * 100

# Chi-square test
chisq_nw <- chisq.test(nw_churn)

chisq_nw

cramers_v(nw_churn)

# ============================================================
# ANOVA: Current Balance Across Occupation Groups
# ============================================================

# Descriptive statistics by occupation
aggregate(
  current_balance ~ occupation,
  data = data,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    n = length(na.omit(x))
  )
)

# One-way ANOVA
anova_model <- aov(
  current_balance ~ occupation,
  data = data
)

summary(anova_model)

eta_squared(anova_model)

# ANOVA means
model.tables(anova_model, type = "means")

# Tukey post-hoc test
TukeyHSD(anova_model)

# Expected frequencies
chisq_nw$expected

# Check minimum expected frequency
min(chisq_nw$expected)

# ============================================================
# Sensitivity Analysis: ANOVA excluding Unknown occupation
# ============================================================

known_occupation <- subset(
  data,
  occupation != "Unknown"
)

# Descriptive statistics
aggregate(
  current_balance ~ occupation,
  data = known_occupation,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    n = length(na.omit(x))
  )
)

# ANOVA
anova_known <- aov(
  current_balance ~ occupation,
  data = known_occupation
)

summary(anova_known)
eta_squared(anova_known)

# Tukey post-hoc test
TukeyHSD(anova_known)

# ============================================================
# Kruskal-Wallis robustness check
# ============================================================

kruskal.test(
  current_balance ~ occupation,
  data = known_occupation
)


oneway.test(current_balance ~ occupation, data = known_occupation)
FSA::dunnTest(current_balance ~ occupation, data = known_occupation, method = "bonferroni")


# ============================================================
# TASK 5a - INITIAL LOGISTIC REGRESSION MODEL
# ============================================================


# ============================================================
# 1. PREPARE DATA FOR LOGISTIC REGRESSION
# ============================================================

# Convert categorical variables to factors
data$gender <- as.factor(data$gender)
data$occupation <- as.factor(data$occupation)

# Convert churn to a factor
data$churn <- as.factor(data$churn)

# Select variables required for the logistic regression model
model_data <- data[, c(
  "age",
  "gender",
  "dependents",
  "occupation",
  "customer_nw_category",
  "current_balance",
  "current_month_credit",
  "previous_month_credit",
  "current_month_debit",
  "previous_month_debit",
  "current_month_balance",
  "previous_month_balance",
  "dependents_missing",
  "last_transaction_missing",
  "churn"
)]

# Check dimensions of the modelling dataset
dim(model_data)

# Check structure of the modelling dataset
str(model_data)


# ============================================================
# 2. CHECK CORRELATION BETWEEN NUMERIC PREDICTORS
# ============================================================

# Select continuous/numeric predictors
numeric_vars <- model_data[, c(
  "age",
  "dependents",
  "customer_nw_category",
  "current_balance",
  "current_month_credit",
  "previous_month_credit",
  "current_month_debit",
  "previous_month_debit",
  "current_month_balance",
  "previous_month_balance"
)]

# Correlation matrix
cor(numeric_vars)


# ============================================================
# 3. SPLIT DATA INTO TRAINING AND TESTING SETS
# ============================================================

library(caret)

# Set seed to make the train/test split reproducible
set.seed(42)

# Create a 70% training and 30% testing split
train_index <- createDataPartition(
  model_data$churn,
  p = 0.70,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Check dimensions
dim(train_data)
dim(test_data)

# Check churn proportions in training data
prop.table(table(train_data$churn))

# Check churn proportions in testing data
prop.table(table(test_data$churn))


# ============================================================
# 4. FIT THE INITIAL LOGISTIC REGRESSION MODEL
# ============================================================

logistic_model <- glm(
  churn ~ age +
    gender +
    dependents +
    occupation +
    customer_nw_category +
    current_balance +
    current_month_credit +
    previous_month_credit +
    current_month_debit +
    previous_month_debit +
    current_month_balance +
    previous_month_balance +
    dependents_missing +
    last_transaction_missing,
  data = train_data,
  family = binomial
)

# Display model summary
summary(logistic_model)


# ============================================================
# 5. CHECK FOR MODEL FITTING WARNINGS
# ============================================================

# Display warnings generated during model fitting
warnings()

# Check the range of fitted probabilities
range(fitted(logistic_model))

# Count fitted probabilities extremely close to 0
sum(fitted(logistic_model) < 1e-10)

# Count fitted probabilities extremely close to 1
sum(fitted(logistic_model) > 1 - 1e-10)


# ============================================================
# 6. LOGISTIC REGRESSION COEFFICIENTS
# ============================================================

# Display coefficient estimates, standard errors,
# z-values and p-values
summary(logistic_model)$coefficients


# ============================================================
# 7. ODDS RATIOS
# ============================================================

# Convert logistic regression coefficients into odds ratios
odds_ratios <- exp(coef(logistic_model))

odds_ratios


# ============================================================
# 8. ODDS RATIOS WITH 95% CONFIDENCE INTERVALS
# ============================================================

# Calculate odds ratios and their 95% confidence intervals
odds_ratio_table <- exp(cbind(
  Odds_Ratio = coef(logistic_model),
  Lower_95 = coef(logistic_model) -
    1.96 * sqrt(diag(vcov(logistic_model))),
  Upper_95 = coef(logistic_model) +
    1.96 * sqrt(diag(vcov(logistic_model)))
))

odds_ratio_table


# ============================================================
# 9. CHECK MULTICOLLINEARITY USING VIF
# ============================================================

library(car)

# Calculate Variance Inflation Factors
vif_values <- vif(logistic_model)

vif_values


# ============================================================
# 10. COOK'S DISTANCE - INFLUENTIAL OBSERVATIONS
# ============================================================

# Calculate Cook's distance for each training observation
cooks_d <- cooks.distance(logistic_model)

# Summary of Cook's distance values
summary(cooks_d)

# Define the commonly used Cook's distance threshold
cook_threshold <- 4 / nrow(train_data)

# Count observations above the threshold
sum(cooks_d > cook_threshold)

# Display the maximum Cook's distance
max(cooks_d)

# Identify the 10 observations with the largest Cook's distance
top_cooks <- head(
  sort(cooks_d, decreasing = TRUE),
  10
)

top_cooks

# Inspect the observations with the largest Cook's distance
train_data[names(top_cooks), ]

# Check the churn status of the influential observations
train_data[names(top_cooks), "churn"]


# ============================================================
# 11. CHECK LINEARITY OF CONTINUOUS PREDICTORS IN THE LOGIT
# ============================================================

# Component-plus-residual plots are used to assess
# whether continuous predictors have an approximately
# linear relationship with the logit

# Reset the plotting layout
par(mfrow = c(1, 1))

# Increase the plotting margins
par(mar = c(3, 3, 2, 1))

# Check linearity of continuous predictors in the logit
crPlots(logistic_model)



# ============================================================
# 12. PREDICT CHURN PROBABILITIES ON THE TEST SET
# ============================================================

# Predict the probability of churn for each test observation
test_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

# Display summary of predicted probabilities
summary(test_prob)


# ============================================================
# 13. CONVERT PROBABILITIES INTO PREDICTED CLASSES
# ============================================================

# Use 0.5 as the initial classification threshold
test_pred <- ifelse(
  test_prob >= 0.5,
  1,
  0
)

# Convert predictions to a factor
test_pred <- factor(
  test_pred,
  levels = c(0, 1)
)


# ============================================================
# 14. CONFUSION MATRIX AND CLASSIFICATION METRICS
# ============================================================

# Generate confusion matrix
cm <- confusionMatrix(
  test_pred,
  test_data$churn,
  positive = "1",
  mode = "everything"
)

cm


# ============================================================
# 15. EXTRACT IMPORTANT CLASSIFICATION METRICS
# ============================================================

# Extract accuracy
accuracy <- cm$overall["Accuracy"]

# Extract sensitivity / recall
sensitivity <- cm$byClass["Sensitivity"]

# Extract specificity
specificity <- cm$byClass["Specificity"]

# Extract positive predictive value / precision
precision <- cm$byClass["Pos Pred Value"]

# Extract balanced accuracy
balanced_accuracy <- cm$byClass["Balanced Accuracy"]

# Display metrics
accuracy
sensitivity
specificity
precision
balanced_accuracy


# ============================================================
# 16. ROC CURVE AND AUC
# ============================================================

library(pROC)

# Calculate ROC curve using test-set predictions
roc_curve <- roc(
  test_data$churn,
  test_prob,
  levels = c("0", "1"),
  direction = "<"
)

# Calculate Area Under the Curve (AUC)
auc_value <- auc(roc_curve)

auc_value


# Plot ROC curve
plot(
  roc_curve,
  main = "ROC Curve - Logistic Regression",
  col = "blue",
  lwd = 2
)

# Add the reference line representing random classification
abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "gray"
)


# ============================================================
# 17. OVERALL MODEL FIT
# ============================================================

# Calculate McFadden's pseudo R-squared
mcfadden_r2 <- 1 -
  logistic_model$deviance /
  logistic_model$null.deviance

mcfadden_r2


# ============================================================
# 18. OVERALL LIKELIHOOD-RATIO TEST
# ============================================================

# Compare the fitted model with the null model
lr_p_value <- pchisq(
  logistic_model$null.deviance -
    logistic_model$deviance,
  df = logistic_model$df.null -
    logistic_model$df.residual,
  lower.tail = FALSE
)

lr_p_value


# ============================================================
# 19. TYPE II ANALYSIS OF DEVIANCE
# ============================================================

# Test the contribution of individual predictors
# while accounting for the other variables in the model
Anova(
  logistic_model,
  type = "II"
)


# ============================================================
# 20. OPTIONAL - ALTERNATIVE CLASSIFICATION THRESHOLD
# ============================================================

# Select an alternative threshold using Youden's J statistic
# based only on the training data
train_roc <- roc(
  train_data$churn,
  fitted(logistic_model),
  levels = c("0", "1"),
  direction = "<"
)

# Find the threshold that maximizes Youden's J statistic
thr <- as.numeric(
  coords(
    train_roc,
    "best",
    best.method = "youden",
    ret = "threshold"
  )
)

# Display the selected threshold
thr

# Apply the alternative threshold to the test data
pred_thr <- factor(
  ifelse(
    test_prob >= thr,
    1,
    0
  ),
  levels = c(0, 1)
)

# Evaluate the alternative threshold on the test data
confusionMatrix(
  pred_thr,
  test_data$churn,
  positive = "1",
  mode = "everything"
)


# ============================================================
# 21. SAVE OUTPUTS FOR TASK 5b / MEMBER 4
# ============================================================
saveRDS(train_index,    "data/train_index.rds")
saveRDS(train_data,     "data/train_5a.rds")
saveRDS(test_data,      "data/test_5a.rds")
saveRDS(thr,            "data/threshold_youden.rds")
saveRDS(logistic_model, "data/logistic_model_5a.rds")

write.csv(train_data, "data/train_5a.csv", row.names = FALSE)
write.csv(test_data,  "data/test_5a.csv",  row.names = FALSE)
