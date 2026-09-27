# ============================================================
# Bank Customer Churn - Exploratory Data Analysis
# ============================================================
# Purpose:
#   This script explores the cleaned Bank Customer Churn dataset.
#
#   The analysis follows a logical sequence:
#   1. Understand the distribution of financial variables
#   2. Identify extreme values and outliers
#   3. Assess skewness
#   4. Examine negative and very small transaction values
#   5. Investigate correlations among balance variables
#
# Important:
#   - No observations are removed in this script.
#   - No original financial values are modified.
#   - Transformations are used for visualization only.
# ============================================================


# ------------------------------------------------------------
# 1. Load the cleaned dataset
# ------------------------------------------------------------

data <- read.csv("data/churn_prediction_clean.csv")


# Define variable groups used throughout the analysis
balance_vars <- c(
  "current_balance",
  "previous_month_end_balance",
  "average_monthly_balance_prevQ",
  "average_monthly_balance_prevQ2",
  "current_month_balance",
  "previous_month_balance"
)

credit_debit_vars <- c(
  "current_month_credit",
  "previous_month_credit",
  "current_month_debit",
  "previous_month_debit"
)


# ============================================================
# PART A - BALANCE VARIABLES
# ============================================================
# Research question:
# How are customer balances distributed, and are there
# extreme values that could affect statistical analysis?
# ============================================================


# ------------------------------------------------------------
# 2. Summary statistics
# ------------------------------------------------------------
# First, examine the minimum, quartiles, median, mean and
# maximum to understand the overall scale and distribution.
# ------------------------------------------------------------

summary(data[balance_vars])


# ------------------------------------------------------------
# 3. Raw-scale boxplots
# ------------------------------------------------------------
# Boxplots are used to identify extreme observations.
# The raw scale allows the magnitude of extreme balances
# to be seen directly.
# ------------------------------------------------------------

boxplot(
  data[balance_vars],
  main = "Boxplots of Balance Variables",
  ylab = "Balance",
  las = 2
)


# ------------------------------------------------------------
# 4. Signed-log boxplots
# ------------------------------------------------------------
# The balance variables contain both positive and negative
# values. Therefore, a normal logarithm cannot be applied.
#
# A signed-log transformation is used ONLY for visualization.
# It compresses very large values while preserving whether
# the original balance was positive or negative.
# ------------------------------------------------------------

balance_log <- data.frame(
  lapply(data[balance_vars], function(x) {
    sign(x) * log1p(abs(x))
  })
)

names(balance_log) <- balance_vars

boxplot(
  balance_log,
  main = "Balance Variables After Signed Log Transformation",
  ylab = "Signed log(1 + |Balance|)",
  las = 2
)


# ------------------------------------------------------------
# 5. Measure skewness
# ------------------------------------------------------------
# Skewness provides a numerical measure of asymmetry.
# Positive values indicate right-skewness.
# Extremely large values may be strongly influenced by
# a small number of extreme observations.
# ------------------------------------------------------------

balance_skewness <- sapply(
  data[balance_vars],
  function(x) {
    x <- x[!is.na(x)]
    mean((x - mean(x))^3) / sd(x)^3
  }
)

balance_skewness


# ------------------------------------------------------------
# 6. Identify statistical outliers using the IQR method
# ------------------------------------------------------------
# The IQR method identifies observations below:
#
#   Q1 - 1.5 × IQR
#
# or above:
#
#   Q3 + 1.5 × IQR
#
# These are statistical outliers and are NOT automatically
# treated as data errors.
# ------------------------------------------------------------

balance_iqr_outliers <- sapply(
  data[balance_vars],
  function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    lower <- Q1 - 1.5 * IQR_value
    upper <- Q3 + 1.5 * IQR_value
    
    sum(x < lower | x > upper, na.rm = TRUE)
  }
)

balance_iqr_outliers


# ------------------------------------------------------------
# 7. Investigate negative balances
# ------------------------------------------------------------
# Negative balances are rare but present in several variables.
# They may represent legitimate account conditions such as
# overdraft positions, so they are reported rather than removed.
# ------------------------------------------------------------

negative_balances <- sapply(
  data[balance_vars],
  function(x) {
    sum(x < 0, na.rm = TRUE)
  }
)

negative_balances


# ============================================================
# PART B - CREDIT AND DEBIT VARIABLES
# ============================================================
# Research question:
# How are customer transaction amounts distributed, and do
# credit and debit variables show the same pattern?
# ============================================================


# ------------------------------------------------------------
# 8. Summary statistics
# ------------------------------------------------------------

summary(data[credit_debit_vars])


# ------------------------------------------------------------
# 9. Raw-scale boxplots
# ------------------------------------------------------------
# This visualization highlights the extreme transaction
# amounts and shows how much they stretch the overall scale.
# ------------------------------------------------------------

boxplot(
  data[credit_debit_vars],
  main = "Boxplots of Credit and Debit Variables",
  ylab = "Transaction Amount",
  las = 2
)


# ------------------------------------------------------------
# 10. Log-transformed boxplots
# ------------------------------------------------------------
# All four transaction variables are positive, so log1p()
# can be used to compress extreme upper-tail values.
#
# The transformed values are used for visualization only.
# The original dataset remains unchanged.
# ------------------------------------------------------------

credit_debit_log <- data.frame(
  lapply(data[credit_debit_vars], log1p)
)

names(credit_debit_log) <- credit_debit_vars

boxplot(
  credit_debit_log,
  main = "Credit and Debit Variables After Log Transformation",
  ylab = "log(1 + Transaction Amount)",
  las = 2
)


# ------------------------------------------------------------
# 11. Measure skewness
# ------------------------------------------------------------
# This provides numerical evidence for the visual pattern
# observed in the boxplots.
# ------------------------------------------------------------

credit_debit_skewness <- sapply(
  data[credit_debit_vars],
  function(x) {
    x <- x[!is.na(x)]
    mean((x - mean(x))^3) / sd(x)^3
  }
)

credit_debit_skewness


# ------------------------------------------------------------
# 12. Identify statistical outliers using the IQR method
# ------------------------------------------------------------

credit_debit_iqr_outliers <- sapply(
  data[credit_debit_vars],
  function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    lower <- Q1 - 1.5 * IQR_value
    upper <- Q3 + 1.5 * IQR_value
    
    sum(x < lower | x > upper, na.rm = TRUE)
  }
)

credit_debit_iqr_outliers


# ------------------------------------------------------------
# 13. Check minimum transaction values
# ------------------------------------------------------------
# The raw summary output displayed very small values close
# to zero. This check confirms the actual minimum recorded
# value for each variable.
# ------------------------------------------------------------

sapply(
  data[credit_debit_vars],
  function(x) min(x, na.rm = TRUE)
)


# ============================================================
# PART C - CORRELATION AMONG BALANCE VARIABLES
# ============================================================
# Research question:
# Do the balance variables contain overlapping information?
#
# Strong correlations may indicate redundancy or
# multicollinearity and provide justification for considering
# dimensionality-reduction methods such as PCA.
# ============================================================


# ------------------------------------------------------------
# 14. Calculate the correlation matrix
# ------------------------------------------------------------

balance_correlation <- cor(
  data[balance_vars],
  use = "complete.obs",
  method = "pearson"
)

round(balance_correlation, 2)


# ------------------------------------------------------------
# 15. Visualize the correlation matrix
# ------------------------------------------------------------
# Darker colours represent stronger positive correlations.
# ------------------------------------------------------------

heatmap(
  balance_correlation,
  Rowv = NA,
  Colv = NA,
  scale = "none",
  margins = c(10, 10),
  col = colorRampPalette(
    c("white", "lightblue", "darkblue")
  )(100),
  breaks = seq(0.7, 1, length.out = 101),
  main = "Correlation Heatmap of Balance Variables"
)


# ============================================================
# END OF INITIAL FINANCIAL EDA
# ============================================================
# Main findings identified so far:
#
#   - Balance variables are extremely right-skewed.
#   - Large statistical outliers are present.
#   - A small number of negative balances are present.
#   - Credit and debit variables are also strongly right-skewed.
#   - Credit variables show greater skewness than debit variables.
#   - Balance variables are strongly correlated with one another.
#
# These findings will inform the subsequent descriptive
# statistics, visualizations and modelling decisions.


# ============================================================
# PART D - DESCRIPTIVE STATISTICS BY CHURN STATUS
# ============================================================
# Research question:
# Do churned and non-churned customers differ in their
# age and financial characteristics?
# ============================================================


# Variables to compare
descriptive_vars <- c(
  "age",
  "current_balance",
  "previous_month_end_balance",
  "average_monthly_balance_prevQ",
  "average_monthly_balance_prevQ2",
  "current_month_balance",
  "previous_month_balance"
)


# ------------------------------------------------------------
# 16. Mean values by churn status
# ------------------------------------------------------------

mean_by_churn <- aggregate(
  data[descriptive_vars],
  by = list(churn = data$churn),
  FUN = mean,
  na.rm = TRUE
)

mean_by_churn


# ------------------------------------------------------------
# 17. Median values by churn status
# ------------------------------------------------------------

median_by_churn <- aggregate(
  data[descriptive_vars],
  by = list(churn = data$churn),
  FUN = median,
  na.rm = TRUE
)

median_by_churn
# ============================================================
# ------------------------------------------------------------
# 18. Churn distribution by categorical variables
# ------------------------------------------------------------

# Gender
prop.table(table(data$gender, data$churn), margin = 1) * 100

# ------------------------------------------------------------
# 19. Churn distribution by occupation
# ------------------------------------------------------------

prop.table(table(data$occupation, data$churn), margin = 1) * 100

# ------------------------------------------------------------
# 20. Customer churn distribution
# ------------------------------------------------------------
# This chart shows the overall distribution of the target
# variable and highlights the class imbalance.
# ------------------------------------------------------------

churn_counts <- table(data$churn)
churn_percent <- prop.table(churn_counts) * 100

bar_positions <- barplot(
  churn_counts,
  main = "Customer Churn Distribution",
  xlab = "Churn Status",
  ylab = "Number of Customers",
  names.arg = c("Did Not Churn", "Churned"),
  ylim = c(0, max(churn_counts) * 1.15)
)

text(
  x = bar_positions,
  y = churn_counts,
  labels = paste0(round(churn_percent, 1), "%"),
  pos = 3
)
# ------------------------------------------------------------
# 21. Age distribution by churn status
# ------------------------------------------------------------

boxplot(
  age ~ churn,
  data = data,
  main = "Age Distribution by Churn Status",
  xlab = "Churn Status",
  ylab = "Age",
  names = c("Did Not Churn", "Churned")
)

# ------------------------------------------------------------
# 22. Current balance distribution by churn status
# ------------------------------------------------------------

boxplot(
  sign(current_balance) * log1p(abs(current_balance)) ~ churn,
  data = data,
  main = "Current Balance by Churn Status (Signed Log Scale)",
  xlab = "Churn Status",
  ylab = "Signed log(1 + |Current Balance|)",
  names = c("Did Not Churn", "Churned")
)

# ------------------------------------------------------------
# 24. Negative current balances by churn status
# ------------------------------------------------------------

negative_current_balance <- table(
  data$churn,
  data$current_balance < 0
)

negative_current_balance

prop.table(
  negative_current_balance,
  margin = 1
) * 100
# ------------------------------------------------------------
# 25. Median balance by churn status
# ------------------------------------------------------------

balance_medians_by_churn <- aggregate(
  data[balance_vars],
  by = list(churn = data$churn),
  FUN = median,
  na.rm = TRUE
)

balance_medians_by_churn

# ------------------------------------------------------------
# 26. Median credit/debit values by churn status
# ------------------------------------------------------------

credit_debit_medians_by_churn <- aggregate(
  data[credit_debit_vars],
  by = list(churn = data$churn),
  FUN = median,
  na.rm = TRUE
)

credit_debit_medians_by_churn

# ------------------------------------------------------------
# 27. Credit and debit distributions by churn status
# ------------------------------------------------------------

par(mfrow = c(2, 2))

for (var in credit_debit_vars) {
  
  boxplot(
    log1p(data[[var]]) ~ data$churn,
    main = var,
    xlab = "Churn Status",
    ylab = "log(1 + Transaction Amount)",
    names = c("Did Not Churn", "Churned")
  )
}

par(mfrow = c(1, 1))


# ------------------------------------------------------------
# 28. Sample sizes by churn status
# ------------------------------------------------------------

table(data$churn)

# ------------------------------------------------------------
# 29. Churn rate by gender
# ------------------------------------------------------------

gender_churn_rate <- prop.table(
  table(data$gender, data$churn),
  margin = 1
)[, "1"] * 100

barplot(
  gender_churn_rate,
  main = "Churn Rate by Gender",
  xlab = "Gender",
  ylab = "Churn Rate (%)",
  ylim = c(0, max(gender_churn_rate) * 1.2)
)

text(
  x = seq_along(gender_churn_rate),
  y = gender_churn_rate,
  labels = paste0(round(gender_churn_rate, 1), "%"),
  pos = 3
)

# ------------------------------------------------------------
# 30. Churn rate by occupation
# ------------------------------------------------------------

occupation_churn_rate <- prop.table(
  table(data$occupation, data$churn),
  margin = 1
)[, "1"] * 100

barplot(
  occupation_churn_rate,
  main = "Churn Rate by Occupation",
  xlab = "Occupation",
  ylab = "Churn Rate (%)",
  ylim = c(0, max(occupation_churn_rate) * 1.2),
  las = 2
)

text(
  x = seq_along(occupation_churn_rate),
  y = occupation_churn_rate,
  labels = paste0(round(occupation_churn_rate, 1), "%"),
  pos = 3
)

# ------------------------------------------------------------
# 31. Vintage by churn status
# ------------------------------------------------------------

vintage_by_churn <- aggregate(
  vintage ~ churn,
  data = data,
  FUN = function(x) {
    c(
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE)
    )
  }
)

vintage_by_churn

# ------------------------------------------------------------
# 32. Vintage distribution by churn status
# ------------------------------------------------------------

boxplot(
  vintage ~ churn,
  data = data,
  main = "Vintage Distribution by Churn Status",
  xlab = "Churn Status",
  ylab = "Vintage",
  names = c("Did Not Churn", "Churned")
)


# ============================================================
# PART E - CATEGORICAL VARIABLES BY CHURN
# ============================================================


# ------------------------------------------------------------
# 33. Churn rate by customer network category
# ------------------------------------------------------------

network_churn_rate <- prop.table(
  table(data$customer_nw_category, data$churn),
  margin = 1
)[, "1"] * 100

network_churn_rate


barplot(
  network_churn_rate,
  main = "Churn Rate by Customer Network Category",
  xlab = "Customer Network Category",
  ylab = "Churn Rate (%)",
  ylim = c(0, max(network_churn_rate) * 1.2)
)

text(
  x = seq_along(network_churn_rate),
  y = network_churn_rate,
  labels = paste0(round(network_churn_rate, 1), "%"),
  pos = 3
)


# ------------------------------------------------------------
# 34. Churn rate by city
# ------------------------------------------------------------
# City contains many unique codes, so displaying every city
# would produce an unreadable chart.
# We first identify the 10 most common city codes and compare
# their churn rates.
# ------------------------------------------------------------

top_cities <- names(
  head(
    sort(table(data$city), decreasing = TRUE),
    10
  )
)

city_churn_rate <- prop.table(
  table(
    data$city[data$city %in% top_cities],
    data$churn[data$city %in% top_cities]
  ),
  margin = 1
)[, "1"] * 100

city_churn_rate


barplot(
  city_churn_rate,
  main = "Churn Rate for the 10 Most Common City Codes",
  xlab = "City Code",
  ylab = "Churn Rate (%)",
  ylim = c(0, max(city_churn_rate) * 1.2),
  las = 2
)

text(
  x = seq_along(city_churn_rate),
  y = city_churn_rate,
  labels = paste0(round(city_churn_rate, 1), "%"),
  pos = 3
)


# ------------------------------------------------------------
# 35. Churn rate by branch
# ------------------------------------------------------------
# Branch code is also high-cardinality, so we focus on the
# 10 branches with the largest number of customers.
# ------------------------------------------------------------

top_branches <- names(
  head(
    sort(table(data$branch_code), decreasing = TRUE),
    10
  )
)

branch_churn_rate <- prop.table(
  table(
    data$branch_code[data$branch_code %in% top_branches],
    data$churn[data$branch_code %in% top_branches]
  ),
  margin = 1
)[, "1"] * 100

branch_churn_rate


barplot(
  branch_churn_rate,
  main = "Churn Rate for the 10 Most Common Branches",
  xlab = "Branch Code",
  ylab = "Churn Rate (%)",
  ylim = c(0, max(branch_churn_rate) * 1.2),
  las = 2
)

text(
  x = seq_along(branch_churn_rate),
  y = branch_churn_rate,
  labels = paste0(round(branch_churn_rate, 1), "%"),
  pos = 3
)

# ------------------------------------------------------------
# 36. Customer counts for the most common branches
# ------------------------------------------------------------

branch_counts <- sort(
  table(data$branch_code),
  decreasing = TRUE
)

top_branch_counts <- branch_counts[
  names(branch_counts) %in% top_branches
]

top_branch_counts
