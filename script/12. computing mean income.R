### SETUP: Load Required Libraries
library(survey)
library(dplyr)
library(tidyverse)
library(microbenchmark)
brazil_income_data <- readRDS("./intermediarios/renda_PNADc_brasil2023visita_1_ajustada_uruguai.rds")

data = brazil_income_data
income_var = "VD5008_DEF_adjusted"
group_var = "sexo"
### FUNCTION: Compute Income Metrics with Logging
compute_income_metrics <- function(data, income_var, group_var = NULL) {
  cat("Computing income metrics for", income_var, "and group",
      ifelse(is.null(group_var), "Overall", group_var), "\n")

  if (is.null(group_var)) {
    result_median <- svyquantile(~get(income_var), data, quantiles = 0.5, na.rm = TRUE)
    result_mean <- svymean(~get(income_var), data, na.rm = TRUE)
    result_sd <- svyvar(~get(income_var), data, na.rm = TRUE) %>% sqrt()

    summary_df <- tibble(
      Category = "Overall",
      Category_Value = "Overall",
      Median = as.double(result_median[[1]])[1], # Extract as double
      Mean = coef(result_mean)[1],
      SE_Mean = SE(result_mean)[1],
      Mean_CI_Lower = coef(result_mean)[1] - 1.96 * SE(result_mean)[1],
      Mean_CI_Upper = coef(result_mean)[1] + 1.96 * SE(result_mean)[1],
      SD = coef(result_sd)[1],
      SE_SD = SE(result_sd)[1],
      SD_CI_Lower = coef(result_sd)[1] - 1.96 * SE(result_sd)[1],
      SD_CI_Upper = coef(result_sd)[1] + 1.96 * SE(result_sd)[1]
    )
  } else {
    result_median <- svyby(~get(income_var), by = as.formula(paste("~", group_var)),
                          design = data, FUN = svyquantile, quantiles = 0.5, na.rm = TRUE)
    result_mean <- svyby(~get(income_var), by = as.formula(paste("~", group_var)),
                        design = data, FUN = svymean, na.rm = TRUE)
    result_sd <- svyby(~get(income_var), by = as.formula(paste("~", group_var)),
                      design = data, FUN = svyvar, na.rm = TRUE)

    summary_df <- tibble(
      Category = group_var,
      Category_Value = result_mean[[group_var]],
      Median = as.double(result_median[, 2]), # Extract as double
      Mean = result_mean[, 2],
      SE_Mean = result_mean$se,
      Mean_CI_Lower = result_mean[, 2] - 1.96 * result_mean$se,
      Mean_CI_Upper = result_mean[, 2] + 1.96 * result_mean$se,
      SD = sqrt(result_sd[, 2]),
      SE_SD = sqrt(result_sd$se),
      SD_CI_Lower = sqrt(result_sd[, 2]) - 1.96 * sqrt(result_sd$se),
      SD_CI_Upper = sqrt(result_sd[, 2]) + 1.96 * sqrt(result_sd$se)
    )
  }

  return(summary_df)
}
### ANALYSIS: Compute Income Metrics Efficiently
compute_all_metrics <- function(data) {
  cat("Starting computation of all income metrics...\n")

  # Define grouping details
  categories <- list(NULL, "sexo", "raça", "sexo_raca")
  category_names <- c("Geral", "Sexo", "Raça", "Sexo e Raça")

  results_df <- tibble()

  for (c in seq_along(categories)) {
    cat("Processing category", category_names[c], "...\n")

    # Compute metrics for original income measure
    original <- compute_income_metrics(data, "VD5008_DEF", categories[[c]])
    # Compute metrics for adjusted income measure
    adjusted <- compute_income_metrics(data, "VD5008_DEF_adjusted", categories[[c]])

    # Merge original and adjusted results by Category and Category_Value
    merged_results <- original %>%
      rename(Original_Median = Median,
             Original_Mean = Mean,
             Original_SE_Mean = SE_Mean,
             Original_Mean_CI_Lower = Mean_CI_Lower,
             Original_Mean_CI_Upper = Mean_CI_Upper,
             Original_SD = SD,
             Original_SE_SD = SE_SD,
             Original_SD_CI_Lower = SD_CI_Lower,
             Original_SD_CI_Upper = SD_CI_Upper) %>%
      left_join(
        adjusted %>%
          rename(Adjusted_Median = Median,
                 Adjusted_Mean = Mean,
                 Adjusted_SE_Mean = SE_Mean,
                 Adjusted_Mean_CI_Lower = Mean_CI_Lower,
                 Adjusted_Mean_CI_Upper = Mean_CI_Upper,
                 Adjusted_SD = SD,
                 Adjusted_SE_SD = SE_SD,
                 Adjusted_SD_CI_Lower = SD_CI_Lower,
                 Adjusted_SD_CI_Upper = SD_CI_Upper),
        by = c("Category", "Category_Value")
      ) %>%
      mutate(
        category_name = category_names[c],
        # Mean differences
        Mean_Difference = Adjusted_Mean - Original_Mean,
        SE_Difference_Mean = sqrt(Original_SE_Mean^2 + Adjusted_SE_Mean^2),
        CI_Lower_Diff_Mean = Mean_Difference - 1.96 * SE_Difference_Mean,
        CI_Upper_Diff_Mean = Mean_Difference + 1.96 * SE_Difference_Mean,
        # SD differences
        SD_Difference = Adjusted_SD - Original_SD,
        SE_Difference_SD = sqrt(Original_SE_SD^2 + Adjusted_SE_SD^2),
        CI_Lower_Diff_SD = SD_Difference - 1.96 * SE_Difference_SD,
        CI_Upper_Diff_SD = SD_Difference + 1.96 * SE_Difference_SD
      ) %>%
      select(Category, Category_Value, category_name,
             Original_Median, Adjusted_Median,
             Original_Mean, Adjusted_Mean, Mean_Difference, SE_Difference_Mean, CI_Lower_Diff_Mean, CI_Upper_Diff_Mean,
             Original_SD, Adjusted_SD, SD_Difference, SE_Difference_SD, CI_Lower_Diff_SD, CI_Upper_Diff_SD)

    results_df <- bind_rows(results_df, merged_results)
  }
  cat("Finished computing all income metrics.\n")
  return(results_df)
}

### EXECUTION: Measure Time and Run Analysis
execution_time <- microbenchmark(
  income_results <- compute_all_metrics(brazil_income_data),
  times = 1
)
print(execution_time)

### OUTPUT: Print and Store Results with Logging
income_results %>% View()

# Fix 1: Start a new pipe chain with poverty_results
income_results %>%
  filter(!Category_Value %in% c("","Homem ","Mulher ")) %>%
  mutate(Category_Value = if_else(Category_Value == "Overall", "Geral", Category_Value)) %>%
  mutate(Category_Value = fct_relevel(Category_Value, "Geral") %>% fct_rev())  %>%
  saveRDS("./intermediarios/poverty_results.rds")

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
