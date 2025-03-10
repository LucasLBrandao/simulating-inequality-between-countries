### SETUP: Load Required Libraries
library(survey)
library(dplyr)
library(tidyverse)
library(microbenchmark)
brazil_income_data  <- readRDS("./intermediarios/renda_PNADc_brasil2023visita_1_ajustada_paises.rds")

brazil_income_data$variables %>% colnames()
### FUNCTION: Compute Poverty Metrics with Logging and Count Calculation
compute_poverty_metrics <- function(data, income_var, threshold, group_var = NULL) {
  cat("Computing poverty metrics for", income_var, "with threshold", threshold, "and group",
      ifelse(is.null(group_var), "Overall", group_var), "\n")

  # Define Poverty Indicator (1 = poor, 0 = non-poor)
  data <- update(data, poverty = as.numeric(get(income_var) < threshold))

  # Compute Rate (using svymean) and Count (using svytotal)
  if (is.null(group_var)) {
    result_mean  <- svymean(~poverty, data, na.rm = TRUE)
    result_total <- svytotal(~poverty, data, na.rm = TRUE)
    summary_df <- tibble(
      Category = "Overall",
      Category_Value = "Overall",
      Rate = coef(result_mean)[1],
      SE_Rate = SE(result_mean)[1],
      Rate_CI_Lower = coef(result_mean)[1] - 1.96 * SE(result_mean)[1],
      Rate_CI_Upper = coef(result_mean)[1] + 1.96 * SE(result_mean)[1],
      Count = coef(result_total)[1],
      SE_Count = SE(result_total)[1],
      Count_CI_Lower = coef(result_total)[1] - 1.96 * SE(result_total)[1],
      Count_CI_Upper = coef(result_total)[1] + 1.96 * SE(result_total)[1]
    )
  } else {
    # For group-level calculations, use svyby for both rates and totals
    result_mean  <- svyby(~poverty, by = as.formula(paste("~", group_var)), design = data, FUN = svymean, na.rm = TRUE)
    result_total <- svyby(~poverty, by = as.formula(paste("~", group_var)), design = data, FUN = svytotal, na.rm = TRUE)

    summary_df <- tibble(
      Category = group_var,
      Category_Value = result_mean[[group_var]],
      Rate = result_mean$poverty,
      SE_Rate = result_mean$se,
      Rate_CI_Lower = result_mean$poverty - 1.96 * result_mean$se,
      Rate_CI_Upper = result_mean$poverty + 1.96 * result_mean$se,
      Count = result_total$poverty,
      SE_Count = result_total$se,
      Count_CI_Lower = result_total$poverty - 1.96 * result_total$se,
      Count_CI_Upper = result_total$poverty + 1.96 * result_total$se
    )
  }
  return(summary_df)
}

### ANALYSIS: Compute Poverty and Extreme Poverty Rates and Counts Efficiently
compute_all_metrics <- function(data) {
  cat("Starting computation of all poverty metrics...\n")

  # Define grouping and threshold details
  categories <- list(NULL, "sexo_raca")
  category_names <- c("Geral",  "Sexo e Raça")
  thresholds <- c(660, 330)
  threshold_names <- c("Pobreza", "Extrema Pobreza")
  paises  <- data.frame(nome_pais = c("Finlândia",
                                   "Uruguai",
                                   "México",
                                   "Espanha",
                                   "Estados Unidos"),
                        renda_ajustada = c("VD5008_DEF_adjusted_FIN",
                                           "VD5008_DEF_adjusted_URU",
                                           "VD5008_DEF_adjusted_MEX",
                                           "VD5008_DEF_adjusted_ESP",
                                           "VD5008_DEF_adjusted_EUA"))

  results_df <- tibble()
  for (pais in 1:5) {
    for (t in seq_along(thresholds)) {
      for (c in seq_along(categories)) {
        cat("Processing",paises$nome_pais[pais], threshold_names[t], "for category", category_names[c], "...\n")

        # Compute metrics for original income measure
        original <- compute_poverty_metrics(data, "VD5008_DEF", thresholds[t], categories[[c]])
        # Compute metrics for adjusted income measure
        adjusted <- compute_poverty_metrics(data, paises$renda_ajustada[pais], thresholds[t], categories[[c]])

        # Merge original and adjusted results by Category and Category_Value
        merged_results <- original %>%
          rename(Original_Rate = Rate,
                Original_SE_Rate = SE_Rate,
                Original_Rate_CI_Lower = Rate_CI_Lower,
                Original_Rate_CI_Upper = Rate_CI_Upper,
                Original_Count = Count,
                Original_SE_Count = SE_Count,
                Original_Count_CI_Lower = Count_CI_Lower,
                Original_Count_CI_Upper = Count_CI_Upper) %>%
          left_join(
            adjusted %>%
              rename(Adjusted_Rate = Rate,
                    Adjusted_SE_Rate = SE_Rate,
                    Adjusted_Rate_CI_Lower = Rate_CI_Lower,
                    Adjusted_Rate_CI_Upper = Rate_CI_Upper,
                    Adjusted_Count = Count,
                    Adjusted_SE_Count = SE_Count,
                    Adjusted_Count_CI_Lower = Count_CI_Lower,
                    Adjusted_Count_CI_Upper = Count_CI_Upper),
            by = c("Category", "Category_Value")
          ) %>%
          mutate(
            country_name = paises$nome_pais[pais],
            category_name = category_names[c],
            threshold_name = threshold_names[t],
            # Rate differences
            Rate_Difference = Adjusted_Rate - Original_Rate,
            SE_Difference_Rate = sqrt(Original_SE_Rate^2 + Adjusted_SE_Rate^2),
            CI_Lower_Diff_Rate = Rate_Difference - 1.96 * SE_Difference_Rate,
            CI_Upper_Diff_Rate = Rate_Difference + 1.96 * SE_Difference_Rate,
            # Count differences
            Count_Difference = Adjusted_Count - Original_Count,
            SE_Difference_Count = sqrt(Original_SE_Count^2 + Adjusted_SE_Count^2),
            CI_Lower_Diff_Count = Count_Difference - 1.96 * SE_Difference_Count,
            CI_Upper_Diff_Count = Count_Difference + 1.96 * SE_Difference_Count
          ) %>%
          select(country_name,Category, Category_Value, category_name, threshold_name,
                Original_Rate, Adjusted_Rate, Rate_Difference, SE_Difference_Rate, CI_Lower_Diff_Rate, CI_Upper_Diff_Rate,
                Original_Count, Adjusted_Count, Count_Difference, SE_Difference_Count, CI_Lower_Diff_Count, CI_Upper_Diff_Count)

        results_df <- bind_rows(results_df, merged_results)
      }
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
poverty_results %>%
  filter(! Category_Value %in% c("","Homem ","Mulher ")) %>%
  mutate(Category_Value = if_else(Category_Value == "Overall", "Geral",Category_Value)) %>%
  mutate(Category_Value = fct_relevel(Category_Value, "Geral") %>% fct_rev()) %>%
  mutate(Original_Count = round(Original_Count),
      Adjusted_Count = round(Adjusted_Count,0),
      Count_Difference = Adjusted_Count - Original_Count) %>%
  saveRDS("./intermediarios/poverty_results_countries_sample.rds")

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
