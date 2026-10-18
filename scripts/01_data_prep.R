# 01_data_preparation.R
# Purpose: minimal data cleaning and preparation

# 1. Load original dataset
data <- read.csv("data/churn_prediction.csv", stringsAsFactors = FALSE)

# 2. Check dataset structure and dimensions
dim(data)
str(data)
summary(data)

# 3. Check duplicate records and customer IDs
sum(duplicated(data))
sum(duplicated(data$customer_id))
sum(is.na(data$customer_id))

# 4. Check missing values
colSums(is.na(data))

# Count blank values in character columns
sapply(data[sapply(data, is.character)], function(x) sum(trimws(x) == "", na.rm = TRUE))

# Check the "NaT" placeholder in last_transaction
sum(toupper(trimws(data$last_transaction)) == "NAT", na.rm = TRUE)

# 5. Check target variable
table(data$churn, useNA = "ifany")
round(prop.table(table(data$churn)) * 100, 2)

# 6. Check selected invalid or unusual values
sum(data$age < 0, na.rm = TRUE)
sum(data$dependents < 0, na.rm = TRUE)
sum(data$current_balance < 0, na.rm = TRUE)
sum(data$current_month_credit < 0, na.rm = TRUE)

# 7. Clean blank character values
clean_data <- data
for (col in names(clean_data)) {
  if (is.character(clean_data[[col]])) {
    clean_data[[col]][trimws(clean_data[[col]]) == ""] <- NA
  }
}

# Convert "NaT" to NA only in last_transaction
clean_data$last_transaction[
  toupper(trimws(clean_data$last_transaction)) == "NAT"
] <- NA

# 8. Check missingness against churn
missing_churn_rate <- function(x, treat_nat = FALSE) {
  x <- as.character(x)
  missing <- is.na(x) | trimws(x) == ""
  if (treat_nat) missing <- missing | toupper(trimws(x)) == "NAT"
  
  valid_churn <- !is.na(clean_data$churn) & clean_data$churn %in% c(0, 1)
  
  c(
    Missing_Percentage = round(mean(missing) * 100, 2),
    Churn_Rate_Missing = if (any(missing & valid_churn)) {
      round(mean(clean_data$churn[missing & valid_churn] == 1) * 100, 2)
    } else NA_real_,
    Churn_Rate_Not_Missing = if (any(!missing & valid_churn)) {
      round(mean(clean_data$churn[!missing & valid_churn] == 1) * 100, 2)
    } else NA_real_
  )
}

rbind(
  dependents = missing_churn_rate(clean_data$dependents),
  city = missing_churn_rate(clean_data$city),
  gender = missing_churn_rate(clean_data$gender),
  occupation = missing_churn_rate(clean_data$occupation),
  last_transaction = missing_churn_rate(clean_data$last_transaction, TRUE)
)

# 9. Save only the cleaned dataset
write.csv(clean_data, "data/churn_prediction_clean.csv", row.names = FALSE)
