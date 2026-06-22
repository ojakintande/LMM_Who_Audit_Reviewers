# ============================================================================
# STATISTICAL ANALYSIS: AUTOMATED TRI-MODEL LLM-AS-A-JUDGE CONSENSUS
# FOR UNAGGREGATED DATA (675 rows: 225 reviews × 3 judges)
# ============================================================================

# Load required packages
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(corrplot)
library(irr)
library(patchwork)
library(broom)
library(viridis)
library(reshape2)
library(RColorBrewer)
library(ggcorrplot)

# ============================================================================
# 1. IMPORT TRI-MODEL CONSENSUS DATA
# ============================================================================
mydata <- read.csv("Judges_Real_llm_review_failure_analysis_data_unaggregated.csv")
head(mydata)
dim(mydata)

# ============================================================================
# 2. SYSTEM BOUNDARY PREPARATION
# ============================================================================
failure_types <- c(
  "Hallucinated_Citations",
  "Fabricated_Comparisons",
  "Unsupported_Claims",
  "Verbatim_Hallucination",
  "Self_Correction_Blind_Spot",
  "Confirmation_Bias",
  "Overfitting_to_Prompt",
  "Defensive_Admission_Pattern",
  "Plausible_Reasoning_Gaps",
  "Omission_of_Key_Sources",
  "Suggestion_Fictional_Missing_Elements"
)

existing_failure_types <- failure_types[failure_types %in% names(mydata)]
conf_cols <- paste0(existing_failure_types, "_Conf")
conf_cols <- conf_cols[conf_cols %in% names(mydata)]

cat("Vetted Failure Types Detected:", paste(existing_failure_types, collapse=", "), "\n")
cat("Panel Confidence Vectors Detected:", paste(conf_cols, collapse=", "), "\n")

# ============================================================================
# 3. PREVALENCE ANALYSIS (AGGREGATED ACROSS JUDGES)
# Formula: P_j = n_j / N
# ============================================================================
prevalence_df <- data.frame(
  Failure_Type = existing_failure_types,
  Mean_Frequency_Per_Review = sapply(existing_failure_types, function(col) mean(mydata[[col]], na.rm = TRUE)),
  Total_Detected_Volume = sapply(existing_failure_types, function(col) sum(mydata[[col]], na.rm = TRUE))
)
prevalence_df <- prevalence_df[order(-prevalence_df$Mean_Frequency_Per_Review), ]
print(prevalence_df)

# By Model
prevalence_by_model <- mydata %>%
  group_by(Model) %>%
  summarise(across(all_of(existing_failure_types), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = -Model, names_to = "Failure_Type", values_to = "Mean_Frequency")

# By Condition
prevalence_by_condition <- mydata %>%
  group_by(Condition) %>%
  summarise(across(all_of(existing_failure_types), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = -Condition, names_to = "Failure_Type", values_to = "Mean_Frequency")

# By Judge
prevalence_by_judge <- mydata %>%
  group_by(Judge_ID) %>%
  summarise(across(all_of(existing_failure_types), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = -Judge_ID, names_to = "Failure_Type", values_to = "Mean_Frequency")

print(prevalence_by_judge)

# ============================================================================
# 4. SELF-CORRECTION EFFECTIVENESS
# Formula: R = (e_initial - e_corrected) / e_initial * 100
# ============================================================================
sc_data <- mydata %>% filter(Condition == "Self_Correction")

sc_data <- sc_data %>%
  mutate(
    Error_Reduction = Initial_Errors - Corrected_Errors,
    Error_Reduction_Rate = (Error_Reduction / (Initial_Errors + 0.001)) * 100
  )

overall_reduction <- mean(sc_data$Error_Reduction_Rate, na.rm = TRUE)
cat("\nAggregated Self-Correction Error Reduction Rate (Panel Mean):", round(overall_reduction, 1), "%\n")

reduction_by_model <- sc_data %>%
  group_by(Model) %>%
  summarise(
    Mean_Initial_Errors = mean(Initial_Errors, na.rm = TRUE),
    Mean_Corrected_Errors = mean(Corrected_Errors, na.rm = TRUE),
    Mean_Reduction_Rate = mean(Error_Reduction_Rate, na.rm = TRUE),
    N_Observations = n()
  ) %>%
  arrange(desc(Mean_Reduction_Rate))
print(reduction_by_model)

# ============================================================================
# 5. JUDGE PANEL CONSENSUS CONFIDENCE
# Formula: C_bar = (1/J) * sum(C_i) over J=3 independent judges
# ============================================================================
if (length(conf_cols) > 0) {
  conf_df <- data.frame(
    Failure_Type = existing_failure_types,
    Panel_Confidence_Mean = sapply(conf_cols, function(col) mean(mydata[[col]], na.rm = TRUE))
  )
  conf_df <- conf_df[order(-conf_df$Panel_Confidence_Mean), ]
  print(conf_df)
  
  conf_by_model <- mydata %>%
    group_by(Model) %>%
    summarise(across(all_of(conf_cols), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(cols = -Model, names_to = "Failure_Type", values_to = "Panel_Confidence_Mean")
}

# ============================================================================
# 6. UNCERTAINTY QUANTIFICATION (INTER-JUDGE DISAGREEMENT)
# ============================================================================

# Spread failure types by Judge_ID to compute disagreement
disagreement_wide <- mydata %>%
  select(Review_ID, Judge_ID, all_of(existing_failure_types)) %>%
  pivot_wider(
    id_cols = Review_ID,
    names_from = Judge_ID,
    values_from = all_of(existing_failure_types),
    names_sep = "_"
  )

# Compute disagreement per error type across judges
disagreement_df <- data.frame(Review_ID = unique(mydata$Review_ID))

for (ft in existing_failure_types) {
  j1_col <- paste0(ft, "_J1")
  j2_col <- paste0(ft, "_J2")
  j3_col <- paste0(ft, "_J3")
  
  if (all(c(j1_col, j2_col, j3_col) %in% names(disagreement_wide))) {
    disagreement_df[[paste0(ft, "_Disagreement")]] <- apply(
      disagreement_wide[, c(j1_col, j2_col, j3_col)], 
      1, 
      function(x) sd(x, na.rm = TRUE)
    )
  }
}

# Overall disagreement per review
disagreement_df$Mean_Disagreement <- rowMeans(
  disagreement_df[, grep("_Disagreement$", names(disagreement_df))], 
  na.rm = TRUE
)

# Merge back
mydata_with_disagreement <- mydata %>%
  left_join(disagreement_df[, c("Review_ID", "Mean_Disagreement")], by = "Review_ID")

mydata_with_disagreement$Mean_Disagreement[is.na(mydata_with_disagreement$Mean_Disagreement)] <- 0

# Summary by Model
disagreement_by_model <- mydata_with_disagreement %>%
  group_by(Model) %>%
  summarise(
    Mean_Panel_Disagreement = mean(Mean_Disagreement, na.rm = TRUE),
    SD_Panel_Disagreement = sd(Mean_Disagreement, na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Panel_Disagreement))

cat("\n--- Inter-Judge Disagreement by Model ---\n")
print(disagreement_by_model)

# Correlation between disagreement and errors
cor_val <- cor(mydata_with_disagreement$Mean_Disagreement, 
               mydata_with_disagreement$Initial_Errors, 
               use = "complete.obs")
cat("\nCorrelation between Inter-Judge Disagreement and Initial Errors:", round(cor_val, 3), "\n")

# ============================================================================
# 7. FLEISS' KAPPA: PROPER INTER-JUDGE AGREEMENT
# ============================================================================
cat("\n--- Fleiss' Kappa Inter-Judge Agreement ---\n")

# For each error type, compute Fleiss' Kappa across the 3 judges
kappa_results <- data.frame(Failure_Type = character(), Fleiss_Kappa = numeric(), P_value = numeric())

for (ft in existing_failure_types[1:min(5, length(existing_failure_types))]) {
  # Get judge-specific columns
  judge_data <- mydata %>%
    filter(Judge_ID %in% c("J1", "J2", "J3")) %>%
    select(Review_ID, Judge_ID, !!sym(ft)) %>%
    pivot_wider(names_from = Judge_ID, values_from = !!sym(ft))
  
  # Convert to matrix for Fleiss' Kappa (raters × subjects)
  rating_matrix <- as.matrix(judge_data[, c("J1", "J2", "J3")])
  
  # Handle NA values
  if (sum(is.na(rating_matrix)) > 0) {
    cat(sprintf("  Warning: NA values in %s, using complete cases only\n", ft))
    complete_cases <- complete.cases(rating_matrix)
    rating_matrix <- rating_matrix[complete_cases, ]
  }
  
  if (nrow(rating_matrix) > 0) {
    tryCatch({
      kappa_result <- kappam.fleiss(rating_matrix)
      kappa_results <- rbind(kappa_results, 
                             data.frame(Failure_Type = ft, 
                                        Fleiss_Kappa = kappa_result$value, 
                                        P_value = kappa_result$p.value))
    }, error = function(e) {
      cat(sprintf("  Error computing Fleiss' Kappa for %s: %s\n", ft, e$message))
    })
  }
}

print(kappa_results)

# ============================================================================
# 8. MULTIVARIATE PATTERN ANALYSIS: LOGISTIC REGRESSION
# ============================================================================

mydata$Condition <- as.factor(mydata$Condition)
mydata$Model <- as.factor(mydata$Model)
mydata$Domain <- as.factor(mydata$Domain)
mydata$Length <- as.factor(mydata$Length)

regression_summary <- data.frame()

for (failure in existing_failure_types[1:min(6, length(existing_failure_types))]) {
  cat("\n--- Logistic Regression for:", failure, "---\n")
  
  # Use majority vote across judges for binary outcome
  judge_wide <- mydata %>%
    select(Review_ID, Judge_ID, !!sym(failure)) %>%
    pivot_wider(names_from = Judge_ID, values_from = !!sym(failure))
  
  # Majority vote: if at least 2 judges say > 0.5
  judge_wide$Majority <- rowSums(judge_wide[, c("J1", "J2", "J3")] > 0.5, na.rm = TRUE) >= 2
  
  # Merge back
  review_data <- mydata %>%
    select(Review_ID, Model, Condition, Domain, Page_Count, Length) %>%
    distinct()
  
  model_data <- review_data %>%
    left_join(judge_wide[, c("Review_ID", "Majority")], by = "Review_ID")
  
  if (sum(model_data$Majority, na.rm = TRUE) > 0 && sum(!model_data$Majority, na.rm = TRUE) > 0) {
    fit_model <- glm(Majority ~ Model + Condition + Domain + Page_Count, 
                     data = model_data, family = binomial)
    
    coef_df <- tidy(fit_model)
    coef_df$Failure_Type <- failure
    regression_summary <- rbind(regression_summary, coef_df)
    
    # Print significant predictors
    significant <- coef_df %>% filter(p.value < 0.05)
    if (nrow(significant) > 0) {
      cat("Significant predictors (p < 0.05):\n")
      print(significant)
    } else {
      cat("No significant predictors found.\n")
    }
    
    # Print full model summary
    cat("\nModel Summary:\n")
    print(summary(fit_model))
    cat("\n")
  } else {
    cat("Insufficient data for logistic regression.\n")
  }
}

# ============================================================================
# 9. NOVICE DETECTION MISSED RATE
# ============================================================================

novice_by_model <- mydata %>%
  group_by(Model) %>%
  summarise(
    Mean_Missed_Rate = mean(Novice_Missed_Rate, na.rm = TRUE),
    SD_Missed_Rate = sd(Novice_Missed_Rate, na.rm = TRUE)
  ) %>% arrange(desc(Mean_Missed_Rate))
print(novice_by_model)

# ============================================================================
# 10. VISUALIZATION HEATMAPS
# ============================================================================

# HEATMAP 1: Prevalence by Model
prev_matrix <- prevalence_by_model %>%
  pivot_wider(names_from = Model, values_from = Mean_Frequency) %>%
  column_to_rownames("Failure_Type") %>% as.matrix()

pdf("heatmap_prevalence_model_failure.pdf", width = 10, height = 8)
corrplot(prev_matrix, method = "color", is.corr = FALSE, tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("white", "#E3F2FD", "#0D47A1"))(100),
         title = "Prevalence Heatmap: Failure Frequencies Across Models", mar = c(0, 0, 2, 0))
dev.off()
cat("Heatmap 1 saved: heatmap_prevalence_model_failure.pdf\n")

# HEATMAP 2: Disagreement by Model and Failure Type
disagreement_cols <- paste0(existing_failure_types, "_Disagreement")
disagreement_cols <- disagreement_cols[disagreement_cols %in% names(disagreement_df)]

if (length(disagreement_cols) > 0) {
  disagree_long <- disagreement_df %>%
    select(Review_ID, all_of(disagreement_cols)) %>%
    pivot_longer(cols = -Review_ID, names_to = "Failure_Type", values_to = "Disagreement") %>%
    mutate(Failure_Type = gsub("_Disagreement$", "", Failure_Type)) %>%
    left_join(mydata %>% select(Review_ID, Model) %>% distinct(), by = "Review_ID") %>%
    group_by(Model, Failure_Type) %>%
    summarise(Mean_Disagreement = mean(Disagreement, na.rm = TRUE), .groups = 'drop')
  
  disagree_matrix <- disagree_long %>%
    pivot_wider(names_from = Model, values_from = Mean_Disagreement) %>%
    column_to_rownames("Failure_Type") %>% as.matrix()
  disagree_matrix[is.na(disagree_matrix)] <- 0
  
  pdf("heatmap_disagreement_model_failure.pdf", width = 10, height = 8)
  corrplot(disagree_matrix, method = "color", is.corr = FALSE, tl.col = "black", tl.srt = 45,
           col = colorRampPalette(c("white", "#FFF3E0", "#E65100"))(100),
           title = "Inter-Judge Disagreement: Model vs Failure Type", mar = c(0, 0, 2, 0))
  dev.off()
  cat("Heatmap 2 saved: heatmap_disagreement_model_failure.pdf\n")
} else {
  cat("No disagreement columns found. Skipping Heatmap 2.\n")
}

# ============================================================================
# 11. GGPLOT CHARTS
# ============================================================================

p1 <- ggplot(prevalence_df, aes(x = reorder(Failure_Type, Mean_Frequency_Per_Review), y = Mean_Frequency_Per_Review, fill = Mean_Frequency_Per_Review)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_gradient(low = "#B3E5FC", high = "#0288D1") +
  labs(title = "Prevalence Frequency Across Taxonomies", x = "Failure Classification", y = "Mean Frequency") +
  theme_minimal() + theme(legend.position = "none")
print(p1)

p2 <- prevalence_by_model %>% group_by(Model) %>% summarise(Avg_Freq = mean(Mean_Frequency)) %>%
  ggplot(aes(x = reorder(Model, Avg_Freq), y = Avg_Freq, fill = Model)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette="Set2") +
  labs(title = "Average Failure Densities by Model", x = "Generative Architecture", y = "Averaged Failure Rate") +
  theme_minimal() + theme(legend.position = "none")
print(p2)

p3 <- reduction_by_model %>% ggplot(aes(x = reorder(Model, Mean_Reduction_Rate), y = Mean_Reduction_Rate, fill = Model)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette="Pastel1") +
  labs(title = "Self-Correction Effectiveness by Model", x = "Model", y = "Mean Error Reduction (%)") +
  theme_minimal() + theme(legend.position = "none")
print(p3)

p4 <- novice_by_model %>% ggplot(aes(x = reorder(Model, Mean_Missed_Rate), y = Mean_Missed_Rate, fill = Model)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette="Accent") +
  labs(title = "Novice Missed Rate by Model", x = "Model", y = "Mean Missed Rate (%)") +
  theme_minimal() + theme(legend.position = "none")
print(p4)

p5 <- disagreement_by_model %>% ggplot(aes(x = reorder(Model, Mean_Panel_Disagreement), y = Mean_Panel_Disagreement, fill = Model)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette="Dark2") +
  labs(title = "Inter-Judge Disagreement by Model", x = "Model", y = "Mean Disagreement") +
  theme_minimal() + theme(legend.position = "none")
print(p5)

p6 <- kappa_results %>% ggplot(aes(x = reorder(Failure_Type, Fleiss_Kappa), y = Fleiss_Kappa, fill = Fleiss_Kappa)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_gradient(low = "#FFCDD2", high = "#2E7D32") +
  labs(title = "Fleiss' Kappa: Inter-Judge Agreement by Failure Type", x = "Failure Type", y = "Fleiss' Kappa") +
  theme_minimal() + theme(legend.position = "none")
print(p6)

# ============================================================================
# 12. EXPORT RESULTS TO EXCEL
# ============================================================================

wb <- createWorkbook()

addWorksheet(wb, "Prevalence")
writeData(wb, "Prevalence", prevalence_df)

addWorksheet(wb, "Prevalence_by_Model")
writeData(wb, "Prevalence_by_Model", prevalence_by_model)

addWorksheet(wb, "Prevalence_by_Condition")
writeData(wb, "Prevalence_by_Condition", prevalence_by_condition)

addWorksheet(wb, "Prevalence_by_Judge")
writeData(wb, "Prevalence_by_Judge", prevalence_by_judge)

addWorksheet(wb, "Self_Correction_Efficacy")
writeData(wb, "Self_Correction_Efficacy", reduction_by_model)

if (length(conf_cols) > 0) {
  addWorksheet(wb, "Panel_Confidence_Indices")
  writeData(wb, "Panel_Confidence_Indices", conf_df)
}

addWorksheet(wb, "Disagreement_Profiles")
writeData(wb, "Disagreement_Profiles", disagreement_by_model)

addWorksheet(wb, "Fleiss_Kappa")
writeData(wb, "Fleiss_Kappa", kappa_results)

# ADD LOGISTIC REGRESSION RESULTS TO EXCEL
if (nrow(regression_summary) > 0) {
  addWorksheet(wb, "Logistic_Regression")
  writeData(wb, "Logistic_Regression", regression_summary)
}

addWorksheet(wb, "Novice_Human_Oversight")
writeData(wb, "Novice_Human_Oversight", novice_by_model)

addWorksheet(wb, "Master_Summary_Sheet")
summary_df <- data.frame(
  Core_Pipeline_Parameter = c("Total Independent Reviews Generated", "Averaged Anomaly Rate per Document", 
                              "Mean Inter-Judge Fleiss' Kappa", "Mean Panel Evaluation Variance", "Averaged Novice Missed Rate"),
  Empirical_Value = c(
    n_distinct(mydata$Review_ID),
    round(mean(mydata$Initial_Errors), 2),
    round(mean(kappa_results$Fleiss_Kappa, na.rm = TRUE), 3),
    round(mean(mydata_with_disagreement$Mean_Disagreement, na.rm = TRUE), 3),
    round(mean(mydata$Novice_Missed_Rate), 1)
  )
)
writeData(wb, "Master_Summary_Sheet", summary_df)

output_file <- "REAL_analysis_results_complete.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)
cat("\nResults exported to:", output_file, "\n")

# ============================================================================
# 13. SUMMARY FINDINGS
# ============================================================================

cat("1. Most Prevalent Error:", prevalence_df$Failure_Type[1], 
    "(", round(prevalence_df$Mean_Frequency_Per_Review[1], 2), ")\n")
cat("2. Model with Highest Error Rate:", 
    mydata %>% group_by(Model) %>% summarise(M = mean(Initial_Errors)) %>% slice_max(M) %>% pull(Model), "\n")
cat("3. Model with Best Self-Correction:", 
    reduction_by_model$Model[which.max(reduction_by_model$Mean_Reduction_Rate)], "\n")
cat("4. Mean Fleiss' Kappa:", round(mean(kappa_results$Fleiss_Kappa, na.rm = TRUE), 3), "\n")
cat("5. Mean Inter-Judge Disagreement:", round(mean(mydata_with_disagreement$Mean_Disagreement, na.rm = TRUE), 3), "\n")

# ============================================================================
# 14. LOGISTIC REGRESSION RESULTS SUMMARY
# ============================================================================

if (nrow(regression_summary) > 0) {
  print(regression_summary)
  
  # Extract significant predictors
  sig_predictors <- regression_summary %>% filter(p.value < 0.05)
  if (nrow(sig_predictors) > 0) {
    cat("\n--- Significant Predictors (p < 0.05) ---\n")
    print(sig_predictors)
  } else {
    cat("\n--- No significant predictors found ---\n")
  }
} else {
  cat("No logistic regression results available.\n")
}



#........................................... ADDITIONAL ANALYSIS

# Load required packages
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(corrplot)
library(irr)
library(patchwork)
library(broom)
library(viridis)
library(reshape2)
library(RColorBrewer)
library(ggcorrplot)

# ============================================================================
# 1. IMPORT DATA
# ============================================================================
mydata <- read.csv("Judges_Real_llm_review_failure_analysis_data_unaggregated.csv")
head(mydata)
dim(mydata)

# ============================================================================
# 2. DATA PREPARATION
# ============================================================================
failure_types <- c(
  "Hallucinated_Citations",
  "Fabricated_Comparisons",
  "Unsupported_Claims",
  "Verbatim_Hallucination",
  "Self_Correction_Blind_Spot",
  "Confirmation_Bias",
  "Overfitting_to_Prompt",
  "Defensive_Admission_Pattern",
  "Plausible_Reasoning_Gaps",
  "Omission_of_Key_Sources",
  "Suggestion_Fictional_Missing_Elements"
)

existing_failure_types <- failure_types[failure_types %in% names(mydata)]
conf_cols <- paste0(existing_failure_types, "_Conf")
conf_cols <- conf_cols[conf_cols %in% names(mydata)]

# Ensure Condition is a factor with proper order
mydata$Condition <- factor(mydata$Condition, levels = c("Structured", "Minimal", "Self_Correction"))

# ============================================================================
# 3. PERFORMANCE BY TASK (PER MODEL)
# ============================================================================
performance_by_task <- mydata %>%
  group_by(Model, Condition) %>%
  summarise(
    Mean_Initial_Errors = mean(Initial_Errors, na.rm = TRUE),
    SD_Initial_Errors = sd(Initial_Errors, na.rm = TRUE),
    Mean_Corrected_Errors = mean(Corrected_Errors, na.rm = TRUE),
    N_Reviews = n(),
    .groups = 'drop'
  ) %>%
  arrange(Model, Condition)

print(performance_by_task)

performance_wide <- performance_by_task %>%
  select(Model, Condition, Mean_Initial_Errors) %>%
  pivot_wider(names_from = Condition, values_from = Mean_Initial_Errors)

print(performance_wide)

# ============================================================================
# 4. MODEL COMPARISON: STRUCTURED VS MINIMAL (NO SELF-CORRECTION)
# ============================================================================

structured_minimal <- mydata %>%
  filter(Condition %in% c("Structured", "Minimal")) %>%
  group_by(Model, Condition) %>%
  summarise(
    Mean_Errors = mean(Initial_Errors, na.rm = TRUE),
    SD_Errors = sd(Initial_Errors, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = Condition, values_from = c(Mean_Errors, SD_Errors))

structured_minimal$Improvement <- structured_minimal$Mean_Errors_Minimal - structured_minimal$Mean_Errors_Structured
structured_minimal$Improvement_Pct <- (structured_minimal$Improvement / structured_minimal$Mean_Errors_Structured) * 100

best_structured <- structured_minimal %>% arrange(Mean_Errors_Structured) %>% head(1)
best_minimal <- structured_minimal %>% arrange(Mean_Errors_Minimal) %>% head(1)

# ============================================================================
# 5. SELF-CORRECTION BRILLIANCE ANALYSIS
# ============================================================================

sc_performance <- mydata %>%
  filter(Condition == "Self_Correction") %>%
  group_by(Model) %>%
  summarise(
    Mean_Initial = mean(Initial_Errors, na.rm = TRUE),
    Mean_Corrected = mean(Corrected_Errors, na.rm = TRUE),
    Mean_Reduction = Mean_Initial - Mean_Corrected,
    Reduction_Pct = (Mean_Reduction / Mean_Initial) * 100,
    N_Reviews = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(Reduction_Pct))

best_self_correction <- sc_performance %>% arrange(desc(Reduction_Pct)) %>% head(1)

sc_by_domain <- mydata %>%
  filter(Condition == "Self_Correction") %>%
  group_by(Model, Domain) %>%
  summarise(
    Mean_Initial = mean(Initial_Errors, na.rm = TRUE),
    Mean_Corrected = mean(Corrected_Errors, na.rm = TRUE),
    Reduction_Pct = ((Mean_Initial - Mean_Corrected) / Mean_Initial) * 100,
    .groups = 'drop'
  ) %>%
  arrange(Model, desc(Reduction_Pct))

# ============================================================================
# 6. ERROR RATE BY MODELS VS JUDGES ASSESSMENT
# ============================================================================

judge_stringency <- mydata %>%
  group_by(Judge_ID) %>%
  summarise(
    Mean_Errors_Flagged = mean(Initial_Errors, na.rm = TRUE),
    SD_Errors_Flagged = sd(Initial_Errors, na.rm = TRUE),
    Total_Errors_Flagged = sum(Initial_Errors, na.rm = TRUE),
    N_Reviews = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(Mean_Errors_Flagged))

model_by_judge <- mydata %>%
  group_by(Model, Judge_ID) %>%
  summarise(
    Mean_Errors = mean(Initial_Errors, na.rm = TRUE),
    SD_Errors = sd(Initial_Errors, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = Judge_ID, values_from = c(Mean_Errors, SD_Errors))

judge_ranking_cor <- model_by_judge %>%
  select(Model, starts_with("Mean_Errors_")) %>%
  column_to_rownames("Model") %>%
  cor(use = "complete.obs")

# ============================================================================
# 7. PROGRESSIVE LINE PLOTS WITH CORRECTED LABELS
# ============================================================================

# Create a directory for plots if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Custom theme for rotated x-axis labels
rotated_theme <- theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "right",
  plot.title = element_text(hjust = 0.5, face = "bold")
)

# 7a. Progressive plot: Models across tasks (Initial Errors)
p_task_models <- ggplot(performance_by_task, aes(x = Condition, y = Mean_Initial_Errors, 
                                                 color = Model, group = Model)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Model Performance Across Tasks (Initial Errors)",
       x = "Prompt Condition", y = "Mean Initial Errors") +
  theme_minimal() +
  rotated_theme +
  scale_color_brewer(palette = "Set1")

print(p_task_models)
ggsave("plots/plot_task_models.pdf", p_task_models, width = 10, height = 6)


# 7b. Progressive plot: Judges across tasks
judge_by_task <- mydata %>%
  group_by(Judge_ID, Condition) %>%
  summarise(
    Mean_Errors = mean(Initial_Errors, na.rm = TRUE),
    SD_Errors = sd(Initial_Errors, na.rm = TRUE),
    .groups = 'drop'
  )

p_task_judges <- ggplot(judge_by_task, aes(x = Condition, y = Mean_Errors, 
                                           color = Judge_ID, group = Judge_ID)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Judge Stringency Across Tasks",
       x = "Prompt Condition", y = "Mean Errors Flagged") +
  theme_minimal() +
  rotated_theme +
  scale_color_brewer(palette = "Set2")

print(p_task_judges)
ggsave("plots/plot_task_judges.pdf", p_task_judges, width = 10, height = 6)


# 7c. Self-correction bar plot (rotated labels)
p_self_correction <- ggplot(sc_performance, aes(x = reorder(Model, Reduction_Pct), y = Reduction_Pct, fill = Model)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Self-Correction Brilliance: Error Reduction by Model",
       x = "Model", y = "Error Reduction (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set3")

print(p_self_correction)
ggsave("plots/plot_self_correction.pdf", p_self_correction, width = 10, height = 6)


# 7d. Initial vs Corrected (rotated x-axis)
sc_initial_corrected <- mydata %>%
  filter(Condition == "Self_Correction") %>%
  group_by(Model) %>%
  summarise(
    Mean_Initial = mean(Initial_Errors, na.rm = TRUE),
    Mean_Corrected = mean(Corrected_Errors, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = c(Mean_Initial, Mean_Corrected), 
               names_to = "Stage", values_to = "Mean_Errors")

p_initial_vs_corrected <- ggplot(sc_initial_corrected, aes(x = Model, y = Mean_Errors, fill = Stage)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial vs Corrected Errors (Self-Correction Condition)",
       x = "Model", y = "Mean Errors") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

print(p_initial_vs_corrected)
ggsave("plots/plot_initial_vs_corrected.pdf", p_initial_vs_corrected, width = 10, height = 6)


# 7e. Judge agreement by error type (rotated labels)
judge_by_error <- mydata %>%
  pivot_longer(cols = all_of(existing_failure_types), 
               names_to = "Failure_Type", values_to = "Flagged") %>%
  group_by(Judge_ID, Failure_Type) %>%
  summarise(Mean_Flagged = mean(Flagged, na.rm = TRUE), .groups = 'drop')

p_judge_error <- ggplot(judge_by_error, aes(x = Failure_Type, y = Mean_Flagged, fill = Judge_ID)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Judge Agreement by Error Type",
       x = "Failure Type", y = "Mean Flagged") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")

print(p_judge_error)
ggsave("plots/plot_judge_error.pdf", p_judge_error, width = 12, height = 8)


# 7f. Heatmap: Model × Task Performance
task_heatmap_data <- performance_by_task %>%
  select(Model, Condition, Mean_Initial_Errors) %>%
  pivot_wider(names_from = Condition, values_from = Mean_Initial_Errors) %>%
  column_to_rownames("Model") %>%
  as.matrix()

pdf("plots/heatmap_model_task_performance.pdf", width = 8, height = 6)
corrplot(task_heatmap_data, method = "color", is.corr = FALSE, tl.col = "black", tl.srt = 0,
         col = colorRampPalette(c("#D32F2F", "white", "#1976D2"))(100),
         title = "Model × Task Performance Matrix", mar = c(0, 0, 2, 0))
dev.off()


# ============================================================================
# 7g. ADDITIONAL PLOT: Structured vs Minimal Comparison
# ============================================================================
p_structured_vs_minimal <- structured_minimal %>%
  select(Model, Mean_Errors_Structured, Mean_Errors_Minimal) %>%
  pivot_longer(cols = c(Mean_Errors_Structured, Mean_Errors_Minimal),
               names_to = "Condition", values_to = "Mean_Errors") %>%
  mutate(Condition = gsub("Mean_Errors_", "", Condition)) %>%
  ggplot(aes(x = Model, y = Mean_Errors, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Structured vs Minimal Prompting Performance",
       x = "Model", y = "Mean Errors") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

print(p_structured_vs_minimal)
ggsave("plots/plot_structured_vs_minimal.pdf", p_structured_vs_minimal, width = 10, height = 6)

# 7h. ADDITIONAL PLOT: Model Rankings Heatmap
model_rankings <- summary_table %>%
  select(Model, Structured_Errors, Minimal_Errors, SelfCorrection_Errors) %>%
  column_to_rownames("Model") %>%
  as.matrix()

pdf("plots/heatmap_model_rankings.pdf", width = 8, height = 6)
corrplot(model_rankings, method = "color", is.corr = FALSE, tl.col = "black", tl.srt = 0,
         col = colorRampPalette(c("#E8F5E9", "#FFF9C4", "#FFCDD2"))(100),
         title = "Model Performance Rankings by Condition", mar = c(0, 0, 2, 0))
dev.off()

# ============================================================================
# 7i. ADDITIONAL PLOT: Judge Stringency Bar Plot
# ============================================================================
p_judge_stringency <- ggplot(judge_stringency, aes(x = Judge_ID, y = Mean_Errors_Flagged, fill = Judge_ID)) +
  geom_bar(stat = "identity") +
  labs(title = "Judge Stringency: Mean Errors Flagged per Review",
       x = "Judge", y = "Mean Errors Flagged") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set2")

print(p_judge_stringency)
ggsave("plots/plot_judge_stringency.pdf", p_judge_stringency, width = 8, height = 6)

# ============================================================================
# 8. ADDITIONAL ANALYSIS
# ============================================================================

condition_effect <- mydata %>%
  group_by(Model, Condition) %>%
  summarise(Mean_Errors = mean(Initial_Errors, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Condition, values_from = Mean_Errors)

condition_effect$Effect_Structured_vs_Minimal <- condition_effect$Structured - condition_effect$Minimal
condition_effect$Effect_SelfCorrection_vs_Structured <- condition_effect$Self_Correction - condition_effect$Structured
condition_effect$Effect_SelfCorrection_vs_Minimal <- condition_effect$Self_Correction - condition_effect$Minimal
condition_effect$Structured_Benefit <- (condition_effect$Minimal - condition_effect$Structured) / condition_effect$Minimal * 100

# ============================================================================
# 9. COMPREHENSIVE SUMMARY TABLE
# ============================================================================

summary_table <- data.frame(
  Model = unique(mydata$Model),
  Structured_Errors = sapply(unique(mydata$Model), function(m) {
    mean(mydata$Initial_Errors[mydata$Model == m & mydata$Condition == "Structured"], na.rm = TRUE)
  }),
  Minimal_Errors = sapply(unique(mydata$Model), function(m) {
    mean(mydata$Initial_Errors[mydata$Model == m & mydata$Condition == "Minimal"], na.rm = TRUE)
  }),
  SelfCorrection_Errors = sapply(unique(mydata$Model), function(m) {
    mean(mydata$Initial_Errors[mydata$Model == m & mydata$Condition == "Self_Correction"], na.rm = TRUE)
  }),
  SelfCorrection_Reduction = sapply(unique(mydata$Model), function(m) {
    sc_data <- mydata[mydata$Model == m & mydata$Condition == "Self_Correction", ]
    mean(sc_data$Initial_Errors - sc_data$Corrected_Errors, na.rm = TRUE) / 
      (mean(sc_data$Initial_Errors, na.rm = TRUE) + 0.001) * 100
  }),
  Overall_Mean_Errors = sapply(unique(mydata$Model), function(m) {
    mean(mydata$Initial_Errors[mydata$Model == m], na.rm = TRUE)
  }),
  Best_Performance_Condition = sapply(unique(mydata$Model), function(m) {
    cond_means <- c(
      Structured = mean(mydata$Initial_Errors[mydata$Model == m & mydata$Condition == "Structured"], na.rm = TRUE),
      Minimal = mean(mydata$Initial_Errors[mydata$Model == m & mydata$Condition == "Minimal"], na.rm = TRUE),
      Self_Correction = mean(mydata$Initial_Errors[mydata$Model == m & mydata$Condition == "Self_Correction"], na.rm = TRUE)
    )
    names(cond_means)[which.min(cond_means)]
  })
)

summary_table <- summary_table[order(summary_table$Overall_Mean_Errors), ]

# ============================================================================
# 10. EXPORT ALL RESULTS TO EXCEL
# ============================================================================
wb <- createWorkbook()

addWorksheet(wb, "Performance_by_Task")
writeData(wb, "Performance_by_Task", performance_by_task)

addWorksheet(wb, "Performance_Matrix")
writeData(wb, "Performance_Matrix", performance_wide)

addWorksheet(wb, "Structured_vs_Minimal")
writeData(wb, "Structured_vs_Minimal", structured_minimal)

addWorksheet(wb, "Self_Correction_Brilliance")
writeData(wb, "Self_Correction_Brilliance", sc_performance)

addWorksheet(wb, "Judge_Stringency")
writeData(wb, "Judge_Stringency", judge_stringency)

addWorksheet(wb, "Model_by_Judge")
writeData(wb, "Model_by_Judge", model_by_judge)

addWorksheet(wb, "Judge_Agreement_Corr")
writeData(wb, "Judge_Agreement_Corr", judge_ranking_cor)

addWorksheet(wb, "Condition_Effect")
writeData(wb, "Condition_Effect", condition_effect)

addWorksheet(wb, "Summary_Table")
writeData(wb, "Summary_Table", summary_table)

addWorksheet(wb, "SC_by_Domain")
writeData(wb, "SC_by_Domain", sc_by_domain)

output_file <- "Additional_REAL_analysis_results_complete.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)
cat("\nAll results exported to:", output_file, "\n")

# ============================================================================
# 11. KEY FINDINGS SUMMARY
# ============================================================================

cat(best_structured$Model, "with", round(best_structured$Mean_Errors_Structured, 2), "errors\n")

cat(best_minimal$Model, "with", round(best_minimal$Mean_Errors_Minimal, 2), "errors\n")

cat(best_self_correction$Model, "reduced errors by", round(best_self_correction$Reduction_Pct, 1), "%\n")

cat(judge_stringency$Judge_ID[1], "flagged", round(judge_stringency$Mean_Errors_Flagged[1], 2), "errors on average\n")

cat(judge_stringency$Judge_ID[nrow(judge_stringency)], "flagged", 
    round(judge_stringency$Mean_Errors_Flagged[nrow(judge_stringency)], 2), "errors on average\n")

print(judge_ranking_cor)
