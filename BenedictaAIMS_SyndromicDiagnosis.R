# Load required packages
library(corrplot)
library(pROC)
library(caret)
library(ggplot2)



##LOADING DATASET
df <- read.csv("C:/Users/user/Downloads/HIVSymptomData.csv")
View(df)

## CHECKING DATA STRUCTURE
str(df)

## RESTRUCTURING DATA FRAME
df$HIV <- factor(df$HIV, levels = c(0, 1))
df$SSTD1 <- factor(df$SSTD1, levels = c(0, 1))
df$SSTD2 <- factor(df$SSTD2, levels =  c(0, 1))
df$SSTD3 <- factor(df$SSTD3, levels =  c(0, 1))
df$SSTD4 <- factor(df$SSTD4, levels =  c(0, 1))
df$SSTD5 <- factor(df$SSTD5, levels =  c(0, 1))
df$SSTD6 <- factor(df$SSTD6, levels =  c(0, 1))
df$SSTD7 <- factor(df$SSTD7, levels =  c(0, 1))
df$SSTD8 <- factor(df$SSTD8, levels =  c(0, 1))
df$SSTD9 <- factor(df$SSTD9, levels =  c(0, 1))
df$SSTD10 <- factor(df$SSTD10, levels =  c(0, 1))
actual_levels <- levels(df$HIV)


## CHECKING DATA STRUCTURE
str(df)

# Count number of HIV positive cases
hiv_positive <- sum(df$HIV == 1) 
hiv_negative <- sum(df$HIV == 0)   
total_cases <- nrow(df)

# CALCULATE PERCENTAGES
hiv_positive_percent <- (hiv_positive / total_cases) * 100
hiv_negative_percent <- (hiv_negative / total_cases) * 100

# PRINT RESULTS
cat("\nHIV Status Distribution:\n")
cat("=======================\n")
cat(sprintf("Total number of cases: %d\n", total_cases))
cat(sprintf("HIV Positive cases: %d (%.1f%%)\n", hiv_positive, hiv_positive_percent))
cat(sprintf("HIV Negative cases: %d (%.1f%%)\n", hiv_negative, hiv_negative_percent))

# CREATE A SUMMARY OF SYMPTOM COUNTS WITH ORDERED FACTORS
symptom_counts <- data.frame(
  Symptom = factor(paste0("SSTD", 1:10), levels = paste0("SSTD", 1:10)),  # Ensure correct ordering
  Count = sapply(1:10, function(i) sum(df[[paste0("SSTD", i)]] == 1)),
  Total = nrow(df)
)

# CALCULATE PERCENTAGES
symptom_counts$Percentage <- (symptom_counts$Count / symptom_counts$Total) * 100

# CREATE THE PLOT
symptom_plot <- ggplot(symptom_counts, aes(x = Symptom, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +  theme_minimal() +
  labs(
    x = "Symptom",
    y = "Number of Cases"
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10)
  ) +
  scale_x_discrete(limits = paste0("SSTD", 1:10))  # Explicitly set the order


print(symptom_plot)
ggsave("plots/symptom_distribution.pdf", symptom_plot, width = 12, height = 8, dpi = 300)


# PRINT SUMMARY STATISTICS
cat("\nSummary Statistics:\n")
cat("==================\n")
cat(sprintf("Total number of cases: %d\n", nrow(df)))
cat(sprintf("Most common symptom: %s (%.1f%%)\n", 
            symptom_counts$Symptom[which.max(symptom_counts$Count)],
            max(symptom_counts$Percentage)))
cat(sprintf("Least common symptom: %s (%.1f%%)\n", 
            symptom_counts$Symptom[which.min(symptom_counts$Count)],
            min(symptom_counts$Percentage)))


# Calculate number of symptoms per individual (using numeric comparisons since we have factors)
symptom_count_per_person <- rowSums(sapply(df[, paste0("SSTD", 1:10)], function(x) x == 1))

# Create a frequency table of symptom counts
symptom_frequency <- table(symptom_count_per_person)

# Calculate percentages
symptom_percentages <- prop.table(symptom_frequency) * 100

# Print the results
cat("\nDistribution of Number of Reported Symptoms:\n")
cat("=====================================\n")
cat("Number of Symptoms | Count | Percentage\n")
cat("-------------------------------------\n")
for(i in 0:10) {
  count <- as.integer(symptom_frequency[as.character(i)])
  if(is.na(count)) count <- 0
  percentage <- as.numeric(symptom_percentages[as.character(i)])
  if(is.na(percentage)) percentage <- 0
  cat(sprintf("%17d | %5d | %9.1f%%\n", i, count, percentage))
}

# Print summary statistics
cat("\nSummary Statistics:\n")
cat("=================\n")
cat(sprintf("Mean number of symptoms: %.2f\n", mean(symptom_count_per_person)))
cat(sprintf("Median number of symptoms: %.1f\n", median(symptom_count_per_person)))
cat(sprintf("Mode number of symptoms: %d\n", as.integer(names(which.max(symptom_frequency)))))
cat(sprintf("Range: %d - %d symptoms\n", min(symptom_count_per_person), max(symptom_count_per_person)))

# Create a data frame for plotting
symptom_dist_df <- data.frame(
  Symptoms = factor(0:10),
  Count = as.vector(symptom_frequency),
  Percentage = as.vector(symptom_percentages)
)

# Calculate symptom count per person
symptom_count_per_person <- rowSums(df[, grep("^SSTD", names(df))] == 1)

# Calculate frequency and percentages
symptom_frequency <- table(symptom_count_per_person)
symptom_percentages <- (symptom_frequency / sum(symptom_frequency)) * 100

# Create a data frame for plotting
symptom_dist_df <- data.frame(
  Symptoms = factor(0:10),
  Count = as.vector(symptom_frequency),
  Percentage = as.vector(symptom_percentages)
)

# Create an enhanced visualization
symptom_dist_plot <- ggplot(symptom_dist_df, aes(x = Symptoms, y = Count)) +
  # Add bars
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  
  # Add percentage labels on top of bars
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            vjust = -0.5, size = 4) +
  
  # Minimal clean theme
  theme_minimal() +
  
  # Axis labels
  labs(
    x = "Number of Symptoms",
    y = "Number of Cases"
  ) +
  
  # Customize visual appearance
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 10)
  )

# Save the plot
ggsave("plots/symptom_distribution_analysis.pdf", 
       symptom_dist_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)

# Display the plot
print(symptom_dist_plot)
# Display the plot
print(symptom_dist_plot)

# Print additional statistical insights
cat("\nDetailed Statistical Analysis:\n")
cat("===========================\n")
cat(sprintf("1. Bimodal Distribution Peaks:\n"))
cat(sprintf("   - No symptoms (0): %d cases (%.1f%%)\n", 
            symptom_frequency[1], symptom_percentages[1]))
cat(sprintf("   - All symptoms (10): %d cases (%.1f%%)\n", 
            symptom_frequency[11], symptom_percentages[11]))
cat(sprintf("\n2. Central Tendency:\n"))
cat(sprintf("   - Mean: %.2f symptoms\n", mean(symptom_count_per_person)))
cat(sprintf("   - Median: %.1f symptoms\n", median(symptom_count_per_person)))
cat(sprintf("   - Mode: %d symptoms\n", 
            as.numeric(names(which.max(symptom_frequency)))))
cat(sprintf("\n3. Distribution Characteristics:\n"))
cat(sprintf("   - Standard Deviation: %.2f\n", sd(symptom_count_per_person)))
cat(sprintf("   - Middle Range (1-9 symptoms): %.1f%% of cases\n", 
            sum(symptom_percentages[2:10])))


## FITTING MODEL(FULL)
logModelfull <- glm(HIV ~ ., data = df, family = binomial())
summary(logModelfull)

predfull <- predict(logModelfull,
                    newdata = df,
                    type = 'response')
pred_classfull <- ifelse(predfull > 0.5, 1, 0)



# Create the predicted factor with levels matching the actual levels
predicted_factor_full <- factor(pred_classfull, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_full)

## FITTING MODEL(SSTD1)
logModel1 <- glm(HIV ~ SSTD1, data = df, family = binomial())
summary(logModel1)

pred1 <- predict(logModel1,
                 newdata = df,
                 type = 'response')
pred_class1 <- ifelse(pred1 > 0.5, 1, 0)



# Create the predicted factor with levels matching the actual levels
predicted_factor_1 <- factor(pred_class1, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_1)

## FITTING MODEL(SSTD2)
logModel2 <- glm(HIV ~ SSTD2, data = df, family = binomial())
summary(logModel2)

pred2 <- predict(logModel2,
                 newdata = df,
                 type = 'response')
pred_class2 <- ifelse(pred2 > 0.5, 1, 0)



# Create the predicted factor with levels matching the actual levels
predicted_factor_2 <- factor(pred_class2, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_2)

## FITTING MODEL(SSTD3)
logModel3 <- glm(HIV ~ SSTD3, data = df, family = binomial())
summary(logModel3)

pred3 <- predict(logModel3,
                 newdata = df,
                 type = 'response')
pred_class3 <- ifelse(pred3 > 0.5, 1, 0)


# Create the predicted factor with levels matching the actual levels
predicted_factor_3 <- factor(pred_class3, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_3)

## FITTING MODEL(SSTD4)
logModel4 <- glm(HIV ~ SSTD4, data = df, family = binomial())
summary(logModel4)

pred4 <- predict(logModel4,
                 newdata = df,
                 type = 'response')
pred_class4 <- ifelse(pred4 > 0.5, 1, 0)



# Create the predicted factor with levels matching the actual levels
predicted_factor_4 <- factor(pred_class4, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_4)

## FITTING MODEL(SSTD5)
logModel5 <- glm(HIV ~ SSTD5, data = df, family = binomial())
summary(logModel5)

pred5 <- predict(logModel5,
                 newdata = df,
                 type = 'response')
pred_class5 <- ifelse(pred5 > 0.5, 1, 0)



# Create the predicted factor with levels matching the actual levels
predicted_factor_5 <- factor(pred_class5, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_5)

## FITTING MODEL(SSTD6)
logModel6 <- glm(HIV ~ SSTD6, data = df, family = binomial())
summary(logModel6)

pred6 <- predict(logModel6,
                 newdata = df,
                 type = 'response')
pred_class6 <- ifelse(pred6 > 0.5, 1, 0)



# Create the predicted factor with levels matching the actual levels
predicted_factor_6 <- factor(pred_class6, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_6)

## FITTING MODEL(SSTD7)
logModel7 <- glm(HIV ~ SSTD7, data = df, family = binomial())
summary(logModel7)

pred7 <- predict(logModel7,
                 newdata = df,
                 type = 'response')
pred_class7 <- ifelse(pred7 > 0.5, 1, 0)



# Create the predicted factor with levels matching the actual levels
predicted_factor_7 <- factor(pred_class7, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_7)

## FITTING MODEL(SSTD8)
logModel8 <- glm(HIV ~ SSTD8, data = df, family = binomial())
summary(logModel8)

pred8 <- predict(logModel8,
                 newdata = df,
                 type = 'response')
pred_class8 <- ifelse(pred8 > 0.5, 1, 0)



# Create the predicted factor with levels matching the actual levels
predicted_factor_8 <- factor(pred_class8, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_8)


## FITTING MODEL(SSTD9)
logModel9 <- glm(HIV ~ SSTD9, data = df, family = binomial())
summary(logModel9)

pred9 <- predict(logModel9,
                 newdata = df,
                 type = 'response')
pred_class9 <- ifelse(pred9 > 0.5, 1, 0)



# Create the predicted factor with levels matching the actual levels
predicted_factor_9 <- factor(pred_class9, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_9)

## FITTING MODEL(SSTD10)
logModel10 <- glm(HIV ~ SSTD10, data = df, family = binomial())
summary(logModel10)

pred10 <- predict(logModel10,
                  newdata = df,
                  type = 'response')
pred_class10 <- ifelse(pred10 > 0.514, 1, 0)



# Create the predicted factor with levels matching the actual levels
predicted_factor_10 <- factor(pred_class10, levels = actual_levels)

confusionMatrix(df$HIV, predicted_factor_10)




###SENSITIVITY FOREST

#Extract sensitivity and CI from confusion matrix
get_sensitivity_ci <- function(cm) {
  sens <- cm$byClass["Sensitivity"]
  tp <- cm$table[1,1]
  fn <- cm$table[2,1]
  ci <- prop.test(tp, tp + fn)$conf.int
  return(c(sens, ci[1], ci[2]))
}

# Store results in a list
models <- list(
  "Full Model" = confusionMatrix(df$HIV, predicted_factor_full),
  "SSTD 1" = confusionMatrix(df$HIV, predicted_factor_1),
  "SSTD 2" = confusionMatrix(df$HIV, predicted_factor_2),
  "SSTD 3" = confusionMatrix(df$HIV, predicted_factor_3),
  "SSTD 4" = confusionMatrix(df$HIV, predicted_factor_4),
  "SSTD 5" = confusionMatrix(df$HIV, predicted_factor_5),
  "SSTD 6" = confusionMatrix(df$HIV, predicted_factor_6),
  "SSTD 7" = confusionMatrix(df$HIV, predicted_factor_7),
  "SSTD 8" = confusionMatrix(df$HIV, predicted_factor_8),
  "SSTD 9" = confusionMatrix(df$HIV, predicted_factor_9),
  "SSTD 10" = confusionMatrix(df$HIV, predicted_factor_10)
)

# Build dataframe
sensitivity_data <- do.call(rbind,
                            lapply(names(models), function(name) {
                              vals <- get_sensitivity_ci(models[[name]])
                              data.frame(Model = name,
                                         Sensitivity = vals[1],
                                         CI_lower = vals[2],
                                         CI_upper = vals[3])
                            })
)

# Add formatted label
sensitivity_data$CI_label <- with(sensitivity_data,
                                  sprintf("[%.3f - %.3f]", CI_lower, CI_upper))

# Order from top to bottom
sensitivity_data$Model <- factor(sensitivity_data$Model, levels = rev(sensitivity_data$Model))


ggplot(sensitivity_data, aes(x = Sensitivity, y = Model)) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  
  # CI Interval Label (moved right)
  geom_text(aes(label = CI_label, x = 1.05), 
            hjust = 0, size = 4, fontface = "bold") +
  
  # Exact Sensitivity Value (moved further right)
  geom_text(aes(label = sprintf(" %.3f", Sensitivity), x = 0.96), 
            hjust = 0, size = 4, color = "black") +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(x = "Sensitivity", y = "Model") +
  theme_minimal(base_size = 13) +
  xlim(0, 1.35)  # Extend x-axis to make room

ggsave("plots/sensitivity_forest_plot.pdf", width = 10, height = 6, dpi = 300)


# Extract specificity and CI from confusion matrix
get_specificity_ci <- function(cm) {
  spec <- cm$byClass["Specificity"]
  if (is.na(spec)) {
    spec <- setNames(0, "Specificity")
  }
  tn <- cm$table[2,2]
  fp <- cm$table[1,2]
  if ((tn + fp) > 0) {
    ci <- prop.test(tn, tn + fp)$conf.int
  } else {
    ci <- c(NA, NA)
  }
  return(c(spec, ci[1], ci[2]))
}


# Build specificity data frame
specificity_data <- do.call(rbind,
                            lapply(names(models), function(name) {
                              vals <- get_specificity_ci(models[[name]])
                              data.frame(Model = name,
                                         Specificity = vals[1],
                                         CI_lower = vals[2],
                                         CI_upper = vals[3])
                            })
)

# Add formatted label
specificity_data$CI_label <- with(specificity_data,
                                  sprintf("[%.3f - %.3f]", CI_lower, CI_upper))

# Order models from top to bottom
specificity_data$Model <- factor(specificity_data$Model, levels = rev(specificity_data$Model))

# Plot
ggplot(specificity_data, aes(x = Specificity, y = Model)) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  
  # CI Interval Label (shifted right)
  geom_text(aes(label = CI_label, x = 1.05), 
            hjust = 0, size = 4, fontface = "bold") +
  
  # Exact Specificity Value (shifted slightly left)
  geom_text(aes(label = sprintf(" %.3f", Specificity), x = 0.96), 
            hjust = 0, size = 4, color = "black") +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(x = "Specificity", y = "Model") +
  theme_minimal(base_size = 13) +
  xlim(0, 1.35)  # Room for labels

# Save the plot
ggsave("plots/specificity_forest_plot.pdf", width = 10, height = 6, dpi = 300)



# Create ROC objects for each model with proper ordering

roc_list <- list(
  "SSTD1" = roc(df$HIV, pred1),
  "SSTD2" = roc(df$HIV, pred2),
  "SSTD3" = roc(df$HIV, pred3),
  "SSTD4" = roc(df$HIV, pred4),
  "SSTD5" = roc(df$HIV, pred5),
  "SSTD6" = roc(df$HIV, pred6),
  "SSTD7" = roc(df$HIV, pred7),
  "SSTD8" = roc(df$HIV, pred8),
  "SSTD9" = roc(df$HIV, pred9),
  "SSTD10" = roc(df$HIV, pred10),
  "Full Model" = roc(df$HIV, predfull)
)

# Create data frame for plotting with ordered factors
roc_data <- data.frame()
for(model_name in names(roc_list)) {
  roc_obj <- roc_list[[model_name]]
  temp_df <- data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities,
    Model = model_name,
    AUC = sprintf("%.3f", auc(roc_obj))
  )
  roc_data <- rbind(roc_data, temp_df)
}

# Create ordered factor for Model column to control legend order
roc_data$Model <- factor(roc_data$Model, 
                         levels = c(paste0("SSTD", 1:10), "Full Model"))
roc_data$Legend <- paste0(roc_data$Model, " (AUC = ", roc_data$AUC, ")")
roc_data$Legend <- factor(roc_data$Legend, 
                          levels = unique(roc_data$Legend)[order(match(gsub(" .*", "", unique(roc_data$Legend)), 
                                                                       c(paste0("SSTD", 1:10), "Full")))])

# Plot ROC curves

p <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Legend)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c(rainbow(10), "black")) +
  labs(
    #title = "ROC Curves for All Models",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Model (AUC)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
# Save the plot
ggsave("plots/roc_curve_plot.pdf", width = 10, height = 6, dpi = 300)

print(p)


### CORRELATION

# Ensure all variables are numeric
correlation_data <- data.frame(
  HIV = as.numeric(as.character(df$HIV)),
  SSTD1 = as.numeric(as.character(df$SSTD1)),
  SSTD2 = as.numeric(as.character(df$SSTD2)),
  SSTD3 = as.numeric(as.character(df$SSTD3)),
  SSTD4 = as.numeric(as.character(df$SSTD4)),
  SSTD5 = as.numeric(as.character(df$SSTD5)),
  SSTD6 = as.numeric(as.character(df$SSTD6)),
  SSTD7 = as.numeric(as.character(df$SSTD7)),
  SSTD8 = as.numeric(as.character(df$SSTD8)),
  SSTD9 = as.numeric(as.character(df$SSTD9)),
  SSTD10 = as.numeric(as.character(df$SSTD10))
)

# Function to calculate Kendall's correlation and p-value
kendall_cor_test <- function(x, y) {
  # Remove any NA values
  complete_cases <- complete.cases(x, y)
  x <- x[complete_cases]
  y <- y[complete_cases]
  
  test_result <- cor.test(x, y, method="kendall")
  return(c(test_result$estimate, test_result$p.value))
}

# Initialize matrices for correlations and p-values
n <- ncol(correlation_data)
tau_b <- matrix(NA, n, n)
p_values <- matrix(NA, n, n)
colnames(tau_b) <- colnames(p_values) <- names(correlation_data)
rownames(tau_b) <- rownames(p_values) <- names(correlation_data)

# Calculate correlations and p-values
for(i in 1:n) {
  for(j in 1:n) {
    result <- kendall_cor_test(correlation_data[[i]], correlation_data[[j]])
    tau_b[i,j] <- result[1]
    p_values[i,j] <- result[2]
  }
}

# Create significance markers
sig_markers <- matrix("", nrow=nrow(p_values), ncol=ncol(p_values))
sig_markers[p_values < 0.001] <- "***"
sig_markers[p_values >= 0.001 & p_values < 0.01] <- "**"
sig_markers[p_values >= 0.01 & p_values < 0.05] <- "*"

# Set up the plotting device with specific dimensions and resolution
pdf("plots/correlations.pdf", width=8, height=8)

# Set margins
par(mar=c(1,1,3,1))

# Create correlation plot
corr <- corrplot(tau_b,
         method="color",
         type="full",  # Show full matrix
         addCoef.col="black",
         number.cex=0.9,  # Size of correlation coefficients
         tl.col="black",  # Text label color
         tl.srt=45,      # Rotate text labels
         tl.cex=1.2,     # Text label size
         cl.cex=1,       # Color legend text size
         col=colorRampPalette(c("#4A6FE3", "white", "#E74C3C"))(200),  # Color palette
         #title="Kendall's tau-b Correlations: HIV and STI Symptoms",
         mar=c(0,0,2,0),
         addgrid.col="white",  # Add white grid lines
         diag=TRUE,           # Show diagonal
         outline=TRUE)        # Show cell borders

dev.off()

# Print correlation matrix with significance levels
cat("\nKendall's tau-b Correlation Matrix with Significance Levels:\n")
cat("Significance codes: 0 '***' 0.001 '**' 0.01 '*' 0.05\n\n")

# Format and print the correlation matrix with significance markers
print_matrix <- matrix(paste0(sprintf("%.2f", tau_b), sig_markers), 
                       nrow=nrow(tau_b))
colnames(print_matrix) <- colnames(tau_b)
rownames(print_matrix) <- rownames(tau_b)
print(print_matrix)
cat("\nCorrelation matrix has been saved as: plots/correlation/kendall_tau_b_correlations.png\n")


