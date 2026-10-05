# ============================================================
# Member 4 - Task 9
# Time Series Analysis and Recency Feature
# ============================================================

# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(ggplot2)
library(dplyr)

# ------------------------------------------------------------
# 2. Load cleaned dataset
# ------------------------------------------------------------

data <- read.csv(
  "data/churn_prediction_clean.csv",
  stringsAsFactors = FALSE
)

# Check dataset
dim(data)
str(data)
summary(data)

# ------------------------------------------------------------
# 3. Check last_transaction variable
# ------------------------------------------------------------

print("Checking last_transaction variable:")

summary(data$last_transaction)

head(data$last_transaction)

# ------------------------------------------------------------
# 4. Convert last_transaction to Date
# ------------------------------------------------------------

data$last_transaction <- as.Date(
  data$last_transaction
)

# Check conversion
summary(data$last_transaction)

# ------------------------------------------------------------
# 5. Check missing dates
# ------------------------------------------------------------

missing_dates <- sum(
  is.na(data$last_transaction)
)

cat(
  "Number of missing last_transaction values:",
  missing_dates,
  "\n"
)

# ------------------------------------------------------------
# 6. Display date range
# ------------------------------------------------------------

minimum_date <- min(
  data$last_transaction,
  na.rm = TRUE
)

maximum_date <- max(
  data$last_transaction,
  na.rm = TRUE
)

cat(
  "Earliest transaction date:",
  as.character(minimum_date),
  "\n"
)

cat(
  "Latest transaction date:",
  as.character(maximum_date),
  "\n"
)

# ------------------------------------------------------------
# 7. Explain the time-series limitation
# ------------------------------------------------------------

cat("\n")
cat("TIME SERIES LIMITATION\n")
cat("----------------------\n")

cat(
  "The dataset contains one last_transaction date per customer.\n"
)

cat(
  "It does not contain repeated monthly or weekly observations\n"
)

cat(
  "for each customer. Therefore, a genuine customer-level time\n"
)

cat(
  "series model such as ARIMA cannot be validly fitted using\n"
)

cat(
  "the current dataset.\n"
)

# ------------------------------------------------------------
# 8. Create recency feature
# ------------------------------------------------------------

# Use the latest transaction date in the dataset
# as the reference date.

reference_date <- max(
  data$last_transaction,
  na.rm = TRUE
)

data$days_since_last_transaction <- as.numeric(
  reference_date - data$last_transaction
)

# Check the new variable
summary(
  data$days_since_last_transaction
)

# ------------------------------------------------------------
# 9. Check recency missing values
# ------------------------------------------------------------

missing_recency <- sum(
  is.na(data$days_since_last_transaction)
)

cat(
  "Missing recency values:",
  missing_recency,
  "\n"
)

# ------------------------------------------------------------
# 10. Recency statistics
# ------------------------------------------------------------

recency_summary <- data %>%
  summarise(
    Mean_Days = mean(
      days_since_last_transaction,
      na.rm = TRUE
    ),
    
    Median_Days = median(
      days_since_last_transaction,
      na.rm = TRUE
    ),
    
    Minimum_Days = min(
      days_since_last_transaction,
      na.rm = TRUE
    ),
    
    Maximum_Days = max(
      days_since_last_transaction,
      na.rm = TRUE
    )
  )

print(recency_summary)

# ------------------------------------------------------------
# 11. Compare recency by churn status
# ------------------------------------------------------------

recency_by_churn <- data %>%
  group_by(churn) %>%
  summarise(
    Customers = n(),
    
    Mean_Days = mean(
      days_since_last_transaction,
      na.rm = TRUE
    ),
    
    Median_Days = median(
      days_since_last_transaction,
      na.rm = TRUE
    ),
    
    Minimum_Days = min(
      days_since_last_transaction,
      na.rm = TRUE
    ),
    
    Maximum_Days = max(
      days_since_last_transaction,
      na.rm = TRUE
    )
  )

print(recency_by_churn)

# ------------------------------------------------------------
# 12. Boxplot - Recency by Churn
# ------------------------------------------------------------

recency_boxplot <- ggplot(
  data,
  aes(
    x = factor(churn),
    y = days_since_last_transaction
  )
) +
  geom_boxplot() +
  labs(
    title = "Days Since Last Transaction by Churn Status",
    x = "Churn (0 = Retained, 1 = Churned)",
    y = "Days Since Last Transaction"
  ) +
  theme_minimal()

print(recency_boxplot)

# ------------------------------------------------------------
# 13. Save boxplot
# ------------------------------------------------------------

ggsave(
  "reports/member4_recency_boxplot.png",
  recency_boxplot,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 14. Histogram of recency
# ------------------------------------------------------------

recency_histogram <- ggplot(
  data,
  aes(
    x = days_since_last_transaction
  )
) +
  geom_histogram(
    bins = 30
  ) +
  labs(
    title = "Distribution of Days Since Last Transaction",
    x = "Days Since Last Transaction",
    y = "Number of Customers"
  ) +
  theme_minimal()

print(recency_histogram)

# ------------------------------------------------------------
# 15. Save histogram
# ------------------------------------------------------------

ggsave(
  "reports/member4_recency_histogram.png",
  recency_histogram,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 16. Churn rate by recency group
# ------------------------------------------------------------

data$recency_group <- cut(
  data$days_since_last_transaction,
  
  breaks = c(
    -Inf,
    30,
    60,
    90,
    180,
    365,
    Inf
  ),
  
  labels = c(
    "0-30 days",
    "31-60 days",
    "61-90 days",
    "91-180 days",
    "181-365 days",
    "366+ days"
  ),
  
  right = TRUE
)

recency_churn_summary <- data %>%
  group_by(recency_group) %>%
  summarise(
    Customers = n(),
    
    Churned = sum(
      churn == 1,
      na.rm = TRUE
    ),
    
    Churn_Rate = mean(
      churn == 1,
      na.rm = TRUE
    )
  )

print(recency_churn_summary)

# ------------------------------------------------------------
# 17. Plot churn rate by recency group
# ------------------------------------------------------------

recency_churn_plot <- ggplot(
  recency_churn_summary,
  aes(
    x = recency_group,
    y = Churn_Rate
  )
) +
  geom_col() +
  labs(
    title = "Churn Rate by Days Since Last Transaction",
    x = "Recency Group",
    y = "Churn Rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(recency_churn_plot)

# ------------------------------------------------------------
# 18. Save churn-rate plot
# ------------------------------------------------------------

ggsave(
  "reports/member4_recency_churn_rate.png",
  recency_churn_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 19. Save recency summary
# ------------------------------------------------------------

write.csv(
  recency_summary,
  "reports/member4_recency_summary.csv",
  row.names = FALSE
)

write.csv(
  recency_by_churn,
  "reports/member4_recency_by_churn.csv",
  row.names = FALSE
)

write.csv(
  recency_churn_summary,
  "reports/member4_recency_churn_groups.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 20. Save dataset with recency feature
# ------------------------------------------------------------

write.csv(
  data,
  "reports/member4_data_with_recency.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 21. Future time-series requirements
# ------------------------------------------------------------

cat("\n")
cat("FUTURE TIME-SERIES REQUIREMENTS\n")
cat("--------------------------------\n")

cat(
  "For genuine time-series modelling, the bank would need\n"
)

cat(
  "repeated customer observations across multiple time periods,\n"
)

cat(
  "such as monthly account activity, transactions, balances,\n"
)

cat(
  "credits, debits and churn outcomes.\n"
)

cat(
  "Such data could support trend analysis, seasonal analysis,\n"
)

cat(
  "forecasting and ARIMA-type models.\n"
)

# ------------------------------------------------------------
# 22. Final Task 9 message
# ------------------------------------------------------------

cat("\n")
cat("TASK 9 COMPLETE\n")
cat("---------------\n")

cat(
  "The current dataset does not support genuine customer-level\n"
)

cat(
  "time-series modelling because each customer has only one\n"
)

cat(
  "last_transaction date.\n"
)

cat(
  "A days_since_last_transaction recency feature was created\n"
)

cat(
  "as a potential future predictor of churn.\n"
)