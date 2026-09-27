# ============================================================
# Bank Customer Churn - Data Preparation
# ============================================================
# Purpose:
#   1. Load and inspect the raw dataset
#   2. Identify and investigate missing values
#   3. Document decisions for handling missing values
#   4. Apply the agreed preprocessing
#   5. Validate the cleaned dataset
#   6. Save the cleaned dataset for the group
#
# Note:
#   The original CSV file is never modified.
# ============================================================


# ------------------------------------------------------------
# 1. Load the raw data
# ------------------------------------------------------------

data <- read.csv(
  "data/churn_prediction.csv",
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 2. Initial inspection
# ------------------------------------------------------------

View(data)

# Column names
names(data)

# Structure of the dataset
str(data)


# ------------------------------------------------------------
# 3. Initial missing-value check
# ------------------------------------------------------------

colSums(is.na(data))


# ------------------------------------------------------------
# 4. Investigate categorical missing values
# ------------------------------------------------------------

# Gender
table(data$gender, useNA = "ifany")

# Occupation
table(data$occupation, useNA = "ifany")

# Last transaction
table(data$last_transaction, useNA = "ifany")


# ------------------------------------------------------------
# 5. Investigate the target variable: churn
# ------------------------------------------------------------

table(data$churn)

# Percentage of customers in each churn class
prop.table(table(data$churn)) * 100


# ------------------------------------------------------------
# 6. Investigate whether missingness is associated
#    with churn
# ------------------------------------------------------------

missing_check <- do.call(rbind, lapply(
  list(
    dependents = is.na(data$dependents),
    city = is.na(data$city),
    gender = trimws(data$gender) == "",
    occupation = trimws(data$occupation) == "",
    last_transaction = data$last_transaction == "NaT"
  ),
  function(m) {
    data.frame(
      missing_n = sum(m),
      missing_pct = mean(m) * 100,
      churn_when_missing = mean(data$churn[m]) * 100,
      churn_when_not_missing = mean(data$churn[!m]) * 100
    )
  }
))

missing_check


# ------------------------------------------------------------
# 7. Investigate 'last_transaction'
# ------------------------------------------------------------
# "NaT" means that a transaction date is unavailable.
# We investigate whether these observations appear to
# represent completely inactive customers.
# ------------------------------------------------------------

activity_check <- aggregate(
  cbind(
    current_balance,
    current_month_credit,
    current_month_debit,
    previous_month_credit,
    previous_month_debit
  ) ~ (last_transaction == "NaT"),
  data = data,
  FUN = median,
  na.rm = TRUE
)

activity_check


# Percentage of "NaT" customers with zero activity
with(
  data[data$last_transaction == "NaT", ],
  mean(
    current_month_debit == 0 &
      previous_month_debit == 0 &
      current_month_credit == 0 &
      previous_month_credit == 0
  ) * 100
)


# Check each transaction variable separately
data.frame(
  current_debit_zero = mean(
    data$current_month_debit[data$last_transaction == "NaT"] == 0
  ) * 100,
  
  previous_debit_zero = mean(
    data$previous_month_debit[data$last_transaction == "NaT"] == 0
  ) * 100,
  
  current_credit_zero = mean(
    data$current_month_credit[data$last_transaction == "NaT"] == 0
  ) * 100,
  
  previous_credit_zero = mean(
    data$previous_month_credit[data$last_transaction == "NaT"] == 0
  ) * 100
)


# ------------------------------------------------------------
# 8. Investigate 'dependents'
# ------------------------------------------------------------

table(data$dependents, useNA = "ifany")

summary(data$dependents)

# Investigate unusually high values
data[
  !is.na(data$dependents) & data$dependents >= 10,
  c(
    "customer_id",
    "age",
    "dependents",
    "gender",
    "occupation",
    "churn"
  )
]


# ------------------------------------------------------------
# 9. Investigate 'city'
# ------------------------------------------------------------

# Number of unique city codes
length(unique(data$city))

# Ten most frequent city codes
head(
  sort(table(data$city, useNA = "no"), decreasing = TRUE),
  10
)


# ------------------------------------------------------------
# 10. Investigate 'gender'
# ------------------------------------------------------------

table(data$gender, useNA = "ifany")


# ------------------------------------------------------------
# 11. Investigate 'occupation'
# ------------------------------------------------------------

table(data$occupation, useNA = "ifany")


# ============================================================
# 12. APPLY AGREED MISSING-VALUE TREATMENT
# ============================================================


# ------------------------------------------------------------
# 12.1 Dependents
# ------------------------------------------------------------
# Decision:
#   Missing values -> 0
#   Keep a missingness indicator.
#
# Reason:
#   Median and mode are both 0, and the variable is heavily
#   concentrated at zero. The indicator preserves information
#   about whether the original value was missing.
# ------------------------------------------------------------

data$dependents_missing <- as.integer(
  is.na(data$dependents)
)

data$dependents[is.na(data$dependents)] <- 0


# ------------------------------------------------------------
# 12.2 City
# ------------------------------------------------------------
# Decision:
#   Missing values -> "Unknown"
#
# Reason:
#   City values are identifiers/codes rather than continuous
#   measurements, so mean or median imputation is inappropriate.
# ------------------------------------------------------------

data$city <- as.character(data$city)

data$city[is.na(data$city)] <- "Unknown"


# ------------------------------------------------------------
# 12.3 Gender
# ------------------------------------------------------------
# Decision:
#   Blank/missing values -> "Unknown"
#
# Reason:
#   Mode imputation would assign all missing observations to
#   the most common gender without evidence.
# ------------------------------------------------------------

data$gender <- trimws(data$gender)

data$gender[
  data$gender == "" | is.na(data$gender)
] <- "Unknown"


# ------------------------------------------------------------
# 12.4 Occupation
# ------------------------------------------------------------
# Decision:
#   Blank/missing values -> "Unknown"
#
# Reason:
#   The missing proportion is very small, but assigning the
#   modal occupation would introduce unsupported information.
# ------------------------------------------------------------

data$occupation <- trimws(data$occupation)

data$occupation[
  data$occupation == "" | is.na(data$occupation)
] <- "Unknown"


# ------------------------------------------------------------
# 12.5 Last transaction
# ------------------------------------------------------------
# Decision:
#   Do NOT impute an artificial transaction date.
#   Create a missingness indicator.
#   Convert "NaT" to NA.
#
# Reason:
#   Investigation showed that "NaT" customers still have
#   recorded financial activity. Therefore, "NaT" cannot
#   simply be interpreted as no transaction activity.
# ------------------------------------------------------------

data$last_transaction_missing <- as.integer(
  is.na(data$last_transaction) |
    data$last_transaction == "NaT"
)

data$last_transaction[
  data$last_transaction == "NaT"
] <- NA


# ============================================================
# 13. VALIDATE THE CLEANED DATA
# ============================================================

# Check remaining missing values
colSums(is.na(data))

# Check dependents missing indicator
table(data$dependents_missing, useNA = "ifany")

# Check last transaction missing indicator
table(data$last_transaction_missing, useNA = "ifany")

# Check that gender no longer contains blank values
table(data$gender, useNA = "ifany")

# Check that occupation no longer contains blank values
table(data$occupation, useNA = "ifany")

# Check city after conversion to "Unknown"
table(data$city == "Unknown", useNA = "ifany")


# ------------------------------------------------------------
# 14. Save the cleaned dataset
# ------------------------------------------------------------

write.csv(
  data,
  "data/churn_prediction_clean.csv",
  row.names = FALSE
)

