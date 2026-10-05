# ============================================================
# Member 4 - Task 12
# Final Recommendations
# Based on Actual Task 5, Task 9 and Task 10 Findings
# ============================================================


# ------------------------------------------------------------
# 1. Task information
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("MEMBER 4 - TASK 12\n")
cat("FINAL RECOMMENDATIONS\n")
cat("============================================================\n")


# ------------------------------------------------------------
# 2. LASSO findings
# ------------------------------------------------------------

lasso_finding <- paste(
  "The LASSO logistic regression produced performance very",
  "similar to the initial logistic regression. At the default",
  "0.5 classification threshold, the LASSO model achieved",
  "approximately 0.820 accuracy, 0.664 precision, 0.060 recall,",
  "0.110 F1 score and 0.747 AUC."
)

cat("\nLASSO FINDING:\n")
cat(lasso_finding, "\n")


# ------------------------------------------------------------
# 3. Threshold finding
# ------------------------------------------------------------

threshold_finding <- paste(
  "Using the Youden threshold of approximately 0.207 increased",
  "LASSO recall to approximately 0.613. However, accuracy,",
  "precision and F1 were lower than at the default 0.5 threshold."
)

cat("\nTHRESHOLD FINDING:\n")
cat(threshold_finding, "\n")


# ------------------------------------------------------------
# 4. Task 9 - Time-series finding
# ------------------------------------------------------------

time_series_finding <- paste(
  "The dataset contains one last_transaction date per customer.",
  "Therefore, genuine customer-level time-series modelling such",
  "as ARIMA cannot be validly performed using the current dataset.",
  "A recency feature, days_since_last_transaction, was created",
  "as a potential future predictor."
)

cat("\nTIME-SERIES FINDING:\n")
cat(time_series_finding, "\n")


# ------------------------------------------------------------
# 5. Task 9 - Actual recency findings
# ------------------------------------------------------------

total_customers <- 28382
missing_transaction_dates <- 3223

missing_percentage <- round(
  (missing_transaction_dates / total_customers) * 100,
  2
)

mean_recency <- 69.99781
median_recency <- 30

highest_recency_group <- "31-60 days"
highest_recency_churn_rate <- 0.231

second_recency_group <- "61-90 days"
second_recency_churn_rate <- 0.219

recency_0_30_rate <- 0.201
recency_91_180_rate <- 0.190
recency_181_365_rate <- 0.134
missing_date_churn_rate <- 0.0928


cat("\nTASK 9 ACTUAL FINDINGS:\n")

cat(
  "Total customers:",
  total_customers,
  "\n"
)

cat(
  "Missing transaction dates:",
  missing_transaction_dates,
  "\n"
)

cat(
  "Percentage with missing transaction dates:",
  missing_percentage,
  "%\n"
)

cat(
  "Mean days since last transaction:",
  mean_recency,
  "\n"
)

cat(
  "Median days since last transaction:",
  median_recency,
  "\n"
)

cat(
  "Highest observed churn group:",
  highest_recency_group,
  "\n"
)

cat(
  "Churn rate for 31-60 days:",
  highest_recency_churn_rate * 100,
  "%\n"
)


# ------------------------------------------------------------
# 6. Innovation finding
# ------------------------------------------------------------

innovation_finding <- paste(
  "A Bank Customer Churn Early Warning System is proposed to",
  "combine customer data, predictive models, churn probabilities,",
  "customer risk groups, dashboard reporting and branch-level",
  "retention actions."
)

cat("\nINNOVATION FINDING:\n")
cat(innovation_finding, "\n")


# ------------------------------------------------------------
# 7. Final Recommendations
# ------------------------------------------------------------

recommendations <- data.frame(
  
  Recommendation_ID = 1:10,
  
  Recommendation = c(
    
    "Use predictive churn modelling",
    
    "Evaluate models using recall, precision, F1 and AUC",
    
    "Do not rely on accuracy alone",
    
    "Consider an appropriate probability threshold for retention",
    
    "Use LASSO as a regularized comparison model",
    
    "Consider days since last transaction as a future predictor",
    
    "Investigate customers with 31-60 days since last transaction",
    
    "Develop a customer churn early warning dashboard",
    
    "Continuously monitor model performance",
    
    "Use expert validation before operational deployment"
  ),
  
  Reason = c(
    
    "Predictive modelling can help identify customers at risk",
    
    "The churn outcome is imbalanced and multiple evaluation metrics are required",
    
    "The majority class represents approximately 81.5 percent of customers",
    
    "The LASSO results show a substantial difference in recall between thresholds",
    
    "LASSO provides regularization and variable-selection benefits",
    
    "Recency provides potentially useful customer activity information",
    
    "This group showed the highest observed churn rate at approximately 23.1 percent",
    
    "A dashboard can convert model outputs into operational decisions",
    
    "Customer behaviour and model performance may change over time",
    
    "Business feasibility, privacy, fairness and responsible use should be validated"
  )
)


print(recommendations)


# ------------------------------------------------------------
# 8. Evidence sources
# ------------------------------------------------------------

evidence_sources <- data.frame(
  
  Recommendation_ID = 1:10,
  
  Evidence_Source = c(
    
    "Task 5a and Task 5b modelling results",
    
    "Task 5a and Task 5b evaluation metrics",
    
    "Dataset churn distribution",
    
    "LASSO threshold analysis",
    
    "LASSO variable selection and model comparison",
    
    "Task 9 recency analysis",
    
    "Task 9 recency churn-group analysis",
    
    "Task 10 innovation proposal",
    
    "Future model monitoring requirement",
    
    "Task 10 implementation and governance considerations"
  )
)


print(evidence_sources)


# ------------------------------------------------------------
# 9. Business recommendation summary
# ------------------------------------------------------------

final_summary <- paste(
  "The analysis supports the development of a data-driven",
  "customer churn early warning approach. The comparison between",
  "logistic regression and LASSO produced broadly similar",
  "predictive performance. Because the churn outcome is imbalanced,",
  "recall, precision, F1 and AUC should be considered alongside",
  "accuracy. A lower classification threshold can substantially",
  "increase detection of potential churners, although this may",
  "reduce other performance measures. Task 9 showed that the",
  "current dataset does not support genuine customer-level",
  "time-series modelling because each customer has only one",
  "last_transaction date. However, a days_since_last_transaction",
  "feature was created. The 31-60 day recency group had the highest",
  "observed churn rate at approximately 23.1 percent, followed by",
  "the 61-90 day group at approximately 21.9 percent. Therefore,",
  "transaction recency should be investigated as a potential",
  "predictor in future churn models. The proposed early warning",
  "system should combine predictive modelling, customer risk",
  "classification and dashboard-based retention actions. Before",
  "operational deployment, the system should be validated with",
  "banking experts and monitored for model performance, fairness,",
  "privacy and changing customer behaviour."
)


cat("\n")
cat("============================================================\n")
cat("FINAL RECOMMENDATION SUMMARY\n")
cat("============================================================\n")
cat(final_summary)
cat("\n")


# ------------------------------------------------------------
# 10. Save recommendations
# ------------------------------------------------------------

write.csv(
  recommendations,
  "reports/member4_final_recommendations.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 11. Save evidence sources
# ------------------------------------------------------------

write.csv(
  evidence_sources,
  "reports/member4_recommendation_evidence.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 12. Save final summary
# ------------------------------------------------------------

writeLines(
  final_summary,
  "reports/member4_final_recommendation_summary.txt"
)


# ------------------------------------------------------------
# 13. Save Task 9 key findings
# ------------------------------------------------------------

task9_key_findings <- data.frame(
  
  Finding = c(
    "Total customers",
    "Missing last transaction dates",
    "Missing transaction percentage",
    "Mean days since last transaction",
    "Median days since last transaction",
    "Highest churn recency group",
    "Highest recency-group churn rate",
    "Second highest recency group",
    "Second highest recency-group churn rate"
  ),
  
  Result = c(
    "28,382",
    "3,223",
    "11.36%",
    "70.0 days",
    "30 days",
    "31-60 days",
    "23.1%",
    "61-90 days",
    "21.9%"
  )
)


write.csv(
  task9_key_findings,
  "reports/member4_task9_key_findings.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 14. Final message
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("MEMBER 4 TASK 12 COMPLETE\n")
cat("============================================================\n")

cat(
  "\nFinal recommendation files saved successfully.\n"
)

cat(
  "Task 9 findings have been incorporated into Task 12.\n"
)

cat(
  "Member 4 analysis is ready for final report integration.\n"
)

cat("\n")