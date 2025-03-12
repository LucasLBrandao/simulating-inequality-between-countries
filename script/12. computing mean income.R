### SETUP: Load Required Libraries
library(survey)
library(dplyr)
library(tidyverse)
library(microbenchmark)
brazil_income_data  <- readRDS("./intermediarios/renda_PNADc_brasil2023visita_1_ajustada_paises.rds")


brazil_income_data <- update(brazil_income_data,
  fl_criancas = ifelse(V2009 <= 14, "Até 14 anos", "Acima de 14 anos")
)

### FUNCTION: Compute Income Metrics with Logging
compute_income_metrics <- function(data, income_var, group_var = NULL) {
  cat("Computing income metrics for", income_var, "and group",
      ifelse(is.null(group_var), "Overall", group_var), "\n")

  if (is.null(group_var)) {
    result_median <- svyquantile(~get(income_var), data, quantiles = 0.5, na.rm = TRUE)
    result_mean <- svymean(~get(income_var), data, na.rm = TRUE)
    result_sd <- svyvar(~get(income_var), data, na.rm = TRUE) %>% sqrt()

    result_decis <- svyquantile(~get(income_var), data, quantiles = c(0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.9,1), na.rm = TRUE)
    result_decis_wider <- result_decis$`get(income_var)` %>%
            as.tibble() %>%
            mutate(decil = paste0(c(1,2,3,4,6,7,8,9,10),"º decil")) %>%
            select(decil, quantile) %>%
            pivot_wider(names_from = decil, values_from = quantile)

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
    ) %>% cbind(result_decis_wider)
  } else {
    result_median <- svyby(~get(income_var), by = as.formula(paste("~", group_var)),
                          design = data, FUN = svyquantile, quantiles = 0.5, na.rm = TRUE)
    result_mean <- svyby(~get(income_var), by = as.formula(paste("~", group_var)),
                        design = data, FUN = svymean, na.rm = TRUE)
    result_sd <- svyby(~get(income_var), by = as.formula(paste("~", group_var)),
                      design = data, FUN = svyvar, na.rm = TRUE)

    result_decis <- svyby(~get(income_var), by = as.formula(paste("~", group_var)),
                          design = data, FUN = svyquantile,
                          quantiles = c(0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.9,1), na.rm = TRUE)
     colnames(result_decis)[c(seq(2,10,1))] <-  paste0(c(1,2,3,4,6,7,8,9,10),"º decil")
    result_decis_final  <- result_decis[,c(2:10)]
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
    ) %>% cbind(result_decis_final)
  }

  return(summary_df)
}
### ANALYSIS: Compute Income Metrics Efficiently
compute_all_metrics <- function(data) {
  cat("Starting computation of all income metrics...\n")

  # Define grouping details
  categories <- list(NULL, "sexo_raca", "fl_criancas")
  category_names <- c("Geral", "Sexo e Raça", "Faixa Idade")
  paises <- data.frame(
    nome_pais = c("Finlândia",
                  "Uruguai",
                  "México",
                  "Espanha",
                  "Estados Unidos"),
    renda_ajustada = c("VD5008_DEF_adjusted_FIN",
                       "VD5008_DEF_adjusted_URU",
                       "VD5008_DEF_adjusted_MEX",
                       "VD5008_DEF_adjusted_ESP",
                       "VD5008_DEF_adjusted_EUA"),
    stringsAsFactors = FALSE
  )

  # Compute original metrics once for each category.
  # Rename decile columns (e.g. "1º decil", "2º decil", ...) with prefix "Original_"
  original_results_list <- list()
  for (i in seq_along(categories)) {
    cat("Computing original metrics for category", category_names[i], "...\n")
    original_cat <- compute_income_metrics(data, "VD5008_DEF", categories[[i]]) %>%
      mutate(category_name = category_names[i]) %>%
      rename_at(vars(contains("decil")), ~ paste0("Original_", .))
    original_results_list[[i]] <- original_cat
  }
  original_results <- bind_rows(original_results_list)

  results_df <- tibble()

  # For each country and category, compute adjusted metrics and join with original.
  # Rename decile columns with prefix "Adjusted_" for the computed metrics.
  for (pais in 1:nrow(paises)) {
    for (i in seq_along(categories)) {
      cat("Processing", paises$nome_pais[pais], "for category", category_names[i], "...\n")
      adjusted_cat <- compute_income_metrics(data, paises$renda_ajustada[pais], categories[[i]]) %>%
        mutate(category_name = category_names[i]) %>%
        rename_at(vars(contains("decil")), ~ paste0("Adjusted_", .))

      merged_results <- original_results %>%
        filter(category_name == category_names[i]) %>%
        rename(
          Original_Median = Median,
          Original_Mean = Mean,
          Original_SE_Mean = SE_Mean,
          Original_Mean_CI_Lower = Mean_CI_Lower,
          Original_Mean_CI_Upper = Mean_CI_Upper,
          Original_SD = SD,
          Original_SE_SD = SE_SD,
          Original_SD_CI_Lower = SD_CI_Lower,
          Original_SD_CI_Upper = SD_CI_Upper
        ) %>%
        left_join(
          adjusted_cat %>% rename(
            Adjusted_Median = Median,
            Adjusted_Mean = Mean,
            Adjusted_SE_Mean = SE_Mean,
            Adjusted_Mean_CI_Lower = Mean_CI_Lower,
            Adjusted_Mean_CI_Upper = Mean_CI_Upper,
            Adjusted_SD = SD,
            Adjusted_SE_SD = SE_SD,
            Adjusted_SD_CI_Lower = SD_CI_Lower,
            Adjusted_SD_CI_Upper = SD_CI_Upper
          ),
          by = c("Category", "Category_Value", "category_name")
        ) %>%
        mutate(
          country_name = paises$nome_pais[pais],
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
        select(
          country_name,
          Category, Category_Value, category_name,
          Original_Median, Adjusted_Median,
          Original_Mean, Adjusted_Mean, Mean_Difference, SE_Difference_Mean, CI_Lower_Diff_Mean, CI_Upper_Diff_Mean,
          Original_SD, Adjusted_SD, SD_Difference, SE_Difference_SD, CI_Lower_Diff_SD, CI_Upper_Diff_SD,
          # Include all decile columns for original and adjusted incomes.
          starts_with("Original_1º decil"), starts_with("Original_2º decil"),
          starts_with("Original_3º decil"), starts_with("Original_4º decil"),
          starts_with("Original_5º decil"), starts_with("Original_6º decil"),
          starts_with("Original_7º decil"), starts_with("Original_8º decil"),
          starts_with("Original_9º decil"), starts_with("Original_10º decil"),
          starts_with("Adjusted_1º decil"), starts_with("Adjusted_2º decil"),
          starts_with("Adjusted_3º decil"), starts_with("Adjusted_4º decil"),
          starts_with("Adjusted_5º decil"), starts_with("Adjusted_6º decil"),
          starts_with("Adjusted_7º decil"), starts_with("Adjusted_8º decil"),
          starts_with("Adjusted_9º decil"), starts_with("Adjusted_10º decil")
        )

      results_df <- bind_rows(results_df, merged_results)
    }
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
  saveRDS("./intermediarios/income_results_countries_sample_with_age_and_deciles.rds")

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
