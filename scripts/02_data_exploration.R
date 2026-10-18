# 02_data_exploration.R
# Purpose: explore numerical distributions and churn patterns.
# Creates two figures and prints results in the Console.
# No intermediate CSV files are saved.

data <- read.csv("data/churn_prediction_clean.csv", stringsAsFactors = FALSE)

# 1. Descriptive statistics
balance_vars <- c(
  "current_balance", "previous_month_end_balance",
  "average_monthly_balance_prevQ", "average_monthly_balance_prevQ2",
  "current_month_balance", "previous_month_balance"
)

credit_debit_vars <- c(
  "current_month_credit", "previous_month_credit",
  "current_month_debit", "previous_month_debit"
)

numeric_vars <- c(
  "age", "vintage", "dependents",
  "current_balance", "average_monthly_balance_prevQ",
  "average_monthly_balance_prevQ2", "current_month_credit",
  "previous_month_credit", "current_month_debit", "previous_month_debit"
)

descriptive_numeric_vars <- unique(c("age", "vintage", "dependents",
                                     balance_vars, credit_debit_vars))

required_vars <- unique(c(
  "customer_id", "gender", "occupation", "churn",
  descriptive_numeric_vars
))

missing_vars <- setdiff(required_vars, names(data))
if (length(missing_vars) > 0) {
  stop("Missing required columns: ", paste(missing_vars, collapse = ", "))
}

# Descriptive statistics: count, missing values, mean, median, SD, range and quartiles
descriptive_statistics <- do.call(rbind, lapply(descriptive_numeric_vars, function(var) {
  x <- data[[var]]
  z <- x[!is.na(x)]
  data.frame(
    variable = var,
    n_nonmissing = length(z),
    n_missing = sum(is.na(x)),
    mean = if (length(z)) mean(z) else NA_real_,
    median = if (length(z)) median(z) else NA_real_,
    standard_deviation = if (length(z) > 1) sd(z) else NA_real_,
    minimum = if (length(z)) min(z) else NA_real_,
    q1 = if (length(z)) as.numeric(quantile(z, .25, names = FALSE)) else NA_real_,
    q3 = if (length(z)) as.numeric(quantile(z, .75, names = FALSE)) else NA_real_,
    maximum = if (length(z)) max(z) else NA_real_
  )
}))
cat("\nDESCRIPTIVE STATISTICS\n")
print(descriptive_statistics, row.names = FALSE)

# 2. IQR outlier review (flags only; values are not changed)
detect_iqr_outliers <- function(x) {
  z <- x[!is.na(x)]
  if (!length(z)) {
    return(c(n_nonmissing = 0, q1 = NA, q3 = NA, iqr = NA,
             lower_fence = NA, upper_fence = NA, n_outliers = 0,
             percent_outliers = NA))
  }
  q1 <- as.numeric(quantile(z, .25, names = FALSE))
  q3 <- as.numeric(quantile(z, .75, names = FALSE))
  iq <- q3 - q1
  lo <- q1 - 1.5 * iq
  hi <- q3 + 1.5 * iq
  n <- sum(z < lo | z > hi)
  c(n_nonmissing = length(z), q1 = q1, q3 = q3, iqr = iq,
    lower_fence = lo, upper_fence = hi, n_outliers = n,
    percent_outliers = 100 * n / length(z))
}

outlier_results <- do.call(rbind, lapply(descriptive_numeric_vars, function(var) {
  res <- detect_iqr_outliers(data[[var]])
  data.frame(variable = var, as.list(res), check.names = FALSE)
}))
outlier_results$interpretation <- ifelse(
  !is.na(outlier_results$iqr) & outlier_results$iqr == 0,
  "IQR is zero; interpret flagged count cautiously",
  "Statistical flags only; not confirmed data errors"
)
cat("\nIQR OUTLIER REVIEW\n")
print(outlier_results, row.names = FALSE)

# Negative balances are reported only; they are not automatically errors.
negative_balance_summary <- data.frame(
  variable = balance_vars,
  negative_count = sapply(balance_vars, function(v) sum(data[[v]] < 0, na.rm = TRUE)),
  nonmissing_count = sapply(balance_vars, function(v) sum(!is.na(data[[v]])))
)
negative_balance_summary$percent_negative <- with(
  negative_balance_summary,
  ifelse(nonmissing_count > 0, 100 * negative_count / nonmissing_count, NA_real_)
)
cat("\nNEGATIVE BALANCE CHECK\n")
print(negative_balance_summary, row.names = FALSE)

# Review the dependents distribution and unusually high values.
cat("\nDEPENDENTS FREQUENCY\n")
print(table(data$dependents, useNA = "ifany"))

high_dependents_records <- data[
  !is.na(data$dependents) & data$dependents >= 25,
  intersect(c("customer_id", "age", "dependents", "occupation", "churn"), names(data)),
  drop = FALSE
]
if (nrow(high_dependents_records) > 0) {
  cat("\nRecords with dependents >= 25 (review only; not removed):\n")
  print(high_dependents_records, row.names = FALSE)
}

# 3. Churn summaries
valid_churn <- data$churn[!is.na(data$churn) & data$churn %in% c(0, 1)]
churn_counts <- table(factor(valid_churn, levels = c(0, 1)))
names(churn_counts) <- c("Retained", "Churned")

churn_summary <- data.frame(
  churn_status = names(churn_counts),
  customer_count = as.integer(churn_counts),
  percentage = 100 * as.integer(churn_counts) / sum(churn_counts)
)
cat("\nOVERALL CHURN\n")
print(churn_summary, row.names = FALSE)

get_churn_summary <- function(group, churn) {
  group_chr <- trimws(as.character(group))
  valid <- !is.na(group_chr) & group_chr != "" &
    !is.na(churn) & churn %in% c(0, 1)
  group_chr <- group_chr[valid]
  churn_valid <- churn[valid]
  
  if (!length(group_chr)) {
    return(data.frame(Group = character(), Customers = integer(),
                      Churned = integer(), ChurnRate = numeric()))
  }
  
  groups <- unique(group_chr)
  do.call(rbind, lapply(groups, function(g) {
    selected <- group_chr == g
    n_customers <- sum(selected)
    n_churned <- sum(churn_valid[selected] == 1)
    data.frame(
      Group = g, Customers = n_customers, Churned = n_churned,
      ChurnRate = 100 * n_churned / n_customers,
      stringsAsFactors = FALSE
    )
  }))
}

# Churn rate by age group
age_group <- cut(
  data$age,
  breaks = c(-Inf, 20, 30, 40, 50, 60, 70, Inf),
  labels = c("Under 20", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"),
  right = FALSE
)
age_summary <- get_churn_summary(age_group, data$churn)
age_summary$Group <- factor(age_summary$Group, levels = levels(age_group), ordered = TRUE)
age_summary <- age_summary[order(age_summary$Group), , drop = FALSE]
age_summary$Group <- as.character(age_summary$Group)
cat("\nCHURN BY AGE GROUP\n")
print(age_summary, row.names = FALSE)

# Churn rate by occupation
occupation_summary <- get_churn_summary(data$occupation, data$churn)
occupation_order <- c("company", "retired", "salaried", "self_employed", "student")
occupation_summary$Group <- factor(occupation_summary$Group, levels = occupation_order)
occupation_summary <- occupation_summary[order(occupation_summary$Group), , drop = FALSE]
occupation_summary$Group <- as.character(occupation_summary$Group)
cat("\nCHURN BY OCCUPATION\n")
print(occupation_summary, row.names = FALSE)

# Churn rate by current-balance quartile
balance_values <- data$current_balance
balance_breaks <- unique(as.numeric(quantile(
  balance_values, probs = c(0, .25, .50, .75, 1), na.rm = TRUE, names = FALSE
)))

if (length(balance_breaks) >= 3) {
  balance_group <- cut(
    balance_values, breaks = balance_breaks,
    include.lowest = TRUE, ordered_result = TRUE
  )
  balance_summary <- get_churn_summary(balance_group, data$churn)
  balance_summary$Group <- factor(
    balance_summary$Group, levels = levels(balance_group), ordered = TRUE
  )
  balance_summary <- balance_summary[order(balance_summary$Group), , drop = FALSE]
  
  if (nrow(balance_summary) == 4) {
    balance_summary$Group <- c("Lowest", "Lower-middle", "Upper-middle", "Highest")
  } else {
    balance_summary$Group <- as.character(balance_summary$Group)
  }
} else {
  balance_summary <- data.frame(Group = character(), Customers = integer(),
                                Churned = integer(), ChurnRate = numeric())
  warning("Too few unique current-balance values to create balance groups.")
}
cat("\nCHURN BY CURRENT-BALANCE GROUP\n")
print(balance_summary, row.names = FALSE)

# 4. Figure 1: numerical distributions.
# Financial values are signed log-transformed for plotting only.
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 5), mar = c(3.5, 3.5, 2.5, .8),
    oma = c(0, 0, 2, 0), cex.axis = .75, cex.main = .78)

for (var in numeric_vars) {
  x <- data[[var]]
  if (var %in% c("age", "vintage", "dependents")) {
    plot_values <- x
    x_label <- var
  } else {
    plot_values <- sign(x) * log10(1 + abs(x))
    x_label <- "Signed log10 scale"
  }
  plot_values <- plot_values[!is.na(plot_values)]
  if (length(plot_values)) {
    hist(plot_values, breaks = 30, main = var, xlab = x_label,
         ylab = "Frequency", col = "lightblue", border = "white")
  } else {
    plot.new()
    title(main = var)
    text(.5, .5, "No non-missing values")
  }
}
mtext("Figure 1. Numerical Distributions", outer = TRUE, cex = 1.1, font = 2)
par(old_par)

# 5. Figure 2: overall churn and churn rates by age, occupation and balance.
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mar = c(5, 4, 3, 1),
    oma = c(0, 0, 2, 0), cex.axis = .8, cex.main = .9)

counts <- as.numeric(churn_counts)
bar_pos <- barplot(
  counts, names.arg = names(churn_counts),
  main = "Overall Churn Distribution", xlab = "",
  ylab = "Number of Customers", col = "lightblue",
  border = "white", ylim = c(0, max(counts) * 1.18)
)
text(bar_pos, counts,
     labels = paste0(format(counts, big.mark = ","), "\n(",
                     round(100 * counts / sum(counts), 1), "%)"),
     pos = 3, cex = .75)

if (nrow(age_summary) > 0) {
  rates <- age_summary$ChurnRate
  names(rates) <- age_summary$Group
  pos <- barplot(rates, main = "Churn Rate by Age Group",
                 xlab = "Age Group", ylab = "Churn Rate (%)",
                 col = "lightblue", border = "white",
                 ylim = c(0, max(rates, na.rm = TRUE) * 1.25), las = 2)
  text(pos, rates, labels = paste0(round(rates, 1), "%"), pos = 3, cex = .7)
} else {
  plot.new()
  title("Churn Rate by Age Group")
  text(.5, .5, "No data available")
}

if (nrow(occupation_summary) > 0) {
  rates <- occupation_summary$ChurnRate
  names(rates) <- occupation_summary$Group
  pos <- barplot(rates, main = "Churn Rate by Occupation",
                 xlab = "Occupation", ylab = "Churn Rate (%)",
                 col = "lightblue", border = "white",
                 ylim = c(0, max(rates, na.rm = TRUE) * 1.25), las = 2)
  text(pos, rates, labels = paste0(round(rates, 1), "%"), pos = 3, cex = .7)
} else {
  plot.new()
  title("Churn Rate by Occupation")
  text(.5, .5, "No data available")
}

if (nrow(balance_summary) > 0) {
  rates <- balance_summary$ChurnRate
  names(rates) <- balance_summary$Group
  pos <- barplot(rates, main = "Churn Rate by Current-Balance Group",
                 xlab = "Current-Balance Group", ylab = "Churn Rate (%)",
                 col = "lightblue", border = "white",
                 ylim = c(0, max(rates, na.rm = TRUE) * 1.25), las = 2)
  text(pos, rates, labels = paste0(round(rates, 1), "%"), pos = 3, cex = .7)
} else {
  plot.new()
  title("Churn Rate by Current-Balance Group")
  text(.5, .5, "Balance groups could not be calculated")
}

mtext("Figure 2. Customer Churn Analysis", outer = TRUE, cex = 1.1, font = 2)
par(old_par)

cat("\nEDA completed. Exactly two figures generated; no analysis CSV files saved.\n")
