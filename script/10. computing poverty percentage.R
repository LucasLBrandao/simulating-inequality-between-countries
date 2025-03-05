### SETUP: Load Required Libraries
library(survey)
library(dplyr)
library(microbenchmark)
brazil_income_data  <- readRDS("./intermediarios/renda_PNADc_brasil2023visita_1_ajustada_uruguai.rds")

### FUNCTION: Compute Poverty Metrics with Logging
compute_poverty_metrics <- function(data, income_var, threshold, group_var = NULL) {
  cat("Computing poverty metrics for", income_var, "with threshold", threshold, "and group", ifelse(is.null(group_var), "Overall", group_var), "\n")

  # Define Poverty Indicator
  data <- update(data, poverty = as.numeric(get(income_var) < threshold))

  # Compute Survey Mean for Poverty Rate
  if (is.null(group_var)) {
    result <- svymean(~poverty, data, na.rm = TRUE)
    summary_df <- tibble(
      Category = "Overall",
      Category_Value = "Overall",
      Poverty_Rate = coef(result)[1],
      SE = SE(result)[1],
      CI_Lower = coef(result)[1] - 1.96 * SE(result)[1],
      CI_Upper = coef(result)[1] + 1.96 * SE(result)[1]
    )
  } else {
    result <- svyby(~poverty, by = as.formula(paste("~", group_var)), design = data, FUN = svymean, na.rm = TRUE)
    summary_df <- tibble(
      Category = group_var,
      Category_Value = result[[group_var]],
      Poverty_Rate = result$poverty,
      SE = result$se,
      CI_Lower = result$poverty - 1.96 * result$se,
      CI_Upper = result$poverty + 1.96 * result$se
    )
  }
  return(summary_df)
}

### ANALYSIS: Compute Poverty and Extreme Poverty Rates Efficiently
compute_all_metrics <- function(data) {
  cat("Starting computation of all poverty metrics...\n")

  categories <- list(NULL, "sexo", "raça", "sexo_raca")
  category_names <- c("Overall", "Gender", "Race", "Gender & Race")
  thresholds <- c(660, 330)
  threshold_names <- c("Poverty", "Extreme_Poverty")

  results_df <- tibble()

  for (t in seq_along(thresholds)) {
    for (c in seq_along(categories)) {
      cat("Processing", threshold_names[t], "for category", category_names[c], "...\n")

      original <- compute_poverty_metrics(data, "VD5008_DEF", thresholds[t], categories[[c]])
      adjusted <- compute_poverty_metrics(data, "VD5008_DEF_adjusted", thresholds[t], categories[[c]])

      merged_results <- original %>%
        rename(Original_Rate = Poverty_Rate, Original_SE = SE, Original_CI_Lower = CI_Lower, Original_CI_Upper = CI_Upper) %>%
        left_join(
          adjusted %>%
            rename(Adjusted_Rate = Poverty_Rate, Adjusted_SE = SE, Adjusted_CI_Lower = CI_Lower, Adjusted_CI_Upper = CI_Upper),
          by = c("Category", "Category_Value")
        ) %>%
        mutate(
          category_name = category_names[c],
          threshold_name = threshold_names[t],
          Rate_Difference = Adjusted_Rate - Original_Rate,
          SE_Difference = sqrt(Original_SE^2 + Adjusted_SE^2),
          CI_Lower_Diff = Rate_Difference - 1.96 * SE_Difference,
          CI_Upper_Diff = Rate_Difference + 1.96 * SE_Difference
        ) %>%
        select(Category, Category_Value, category_name, threshold_name, Original_Rate, Adjusted_Rate, Rate_Difference, SE_Difference, CI_Lower_Diff, CI_Upper_Diff)

      results_df <- bind_rows(results_df, merged_results)
    }
  }
  cat("Finished computing all poverty metrics.\n")
  return(results_df)
}

### EXECUTION: Measure Time and Run Analysis
execution_time <- microbenchmark(
  poverty_results <- compute_all_metrics(brazil_income_data),
  times = 1
)
print(execution_time)

### OUTPUT: Print and Store Results with Logging
cat("Final Computed Poverty Metrics:\n")
print(poverty_results)

poverty_results %>% View()
poverty_results %>% saveRDS("./intermediarios/poverty_results.rds")

### VERIFICATION: Ensure No Missing or Unexpected Values
check_results <- function(results_df) {
  cat("Verifying computed results...\n")
  all_finite <- all(is.finite(as.matrix(results_df)))
  if (all_finite) {
    cat("All computed values are finite and valid.\n")
  } else {
    cat("Warning: Some values contain NA or Infinite values. Please check input data.\n")
  }
}
check_results(poverty_results)
