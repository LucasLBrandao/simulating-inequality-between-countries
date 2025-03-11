# Load required libraries
library(survey)
library(dplyr)
library(Hmisc)  # for weighted quantiles if needed
library(ineq)
library(acid)
set.seed(123)  # For reproducibility

# ========= 1. Extract Data and Compute Original Mean =========
# Convert the survey design object to a data.frame and extract weights
brazil_data  <- readRDS("./intermediarios/renda_PNADc_brasil2023visita_1.rds")
brazil_income_data <- subset(brazil_data, !is.na(VD5008_DEF))
brazil_income_data <- update(brazil_income_data,
                              orig_order = seq_len(nrow(brazil_income_data$variables)))

brazil_income_data_df <- brazil_income_data$variables

ordered_brazil_weights  <-  brazil_income_data_df  %>%
                                arrange(VD5008_DEF) %>%
                                .$V1032


# Compute the original (weighted) mean income of Brazil
mean_income <- as.numeric(svymean(~VD5008_DEF, brazil_income_data, na.rm = TRUE))

# ========= 2. Calibrate a Lognormal Distribution Using Uruguay's Gini =========
# For a lognormal distribution, the Gini is given by:
#   G = 2 * pnorm(sigma/sqrt(2)) - 1
# Inverting, we obtain:
#   sigma_target = sqrt(2) * qnorm((G + 1)/2)

gini_indexes  <- fread("./entradas/world-bank/gini_paises_selecionados.csv")
siglas_paises  <- gini_indexes  %>% distinct(pais, sigla_pais)

countries_income_share <- readRDS("./intermediarios/income_share_accumulated_countries_sample.rds") %>%
                          left_join(siglas_paises, by = c("Country" = "pais"))

countries_income_share$IncomeShareAccumulated  <- countries_income_share$IncomeShareAccumulated/100

# Função que ajusta a distribuição lognormal para os dados de um país
fit_log_normal <- function(country_decile_shares, country_gini_index,country_sigla) {
  objective_function <- function(sigma) {
  # A função abaixo é derivada da MGF (Moment generating function) do t=1 de uma lognormal
  # ela foi derivada igualando a renda média do Brasil (mean_income) ao valor esperado da distribuição lognormal
  # e isolando o parametro mu.
  # essa função buscará o valor de sigma que a partir dessa primeira equação...
  # minimizará a diferença entre a distribuição obtida e a distribuição do uruguai
  mu <- log(mean_income) - (sigma^2) / 2


  # Após obter o mu, é criada uma amostra de 383.049 valores aleatórios que sigam a distribuição lognormal
  # com os parâmetros obtidos, e que replique os pesos da amostra do brasil, na ordem por renda.
  sim_sample <- rlnorm(382937, meanlog = mu, sdlog = sigma)  %>% sort()
  weight_sample  <- ordered_brazil_weights
  sample <- data.frame(sim_sample, weight_sample)


  # com os valores aleatórios obtidos, é calculado então o percentil de cada decil e o total da renda, para assim calcular
  # a participação acumulada de cada decil na renda total.
  p <- seq(0, 1, by = 0.1)
  decile_breaks <- wtd.quantile(sample$sim_sample, weights = sample$weight_sample, probs = p, type = "quantile")
  total_income <- sum(sample$sim_sample * sample$weight_sample)

  cum_income_shares <- sapply(2:length(decile_breaks), function(i) {
  sum(sample$sim_sample[sample$sim_sample <= decile_breaks[i]] *
  sample$weight_sample[sample$sim_sample <= decile_breaks[i]]) / total_income
  })

  # Theoretical cumulative decile shares (excluding 0%)
  theoretical_cum_shares <- cum_income_shares

  # depois de calcular a participação acumulada na renda teórica,
  # o valor obtido é comparado com a distribuicao do paraguai
  error_shares <- sum((theoretical_cum_shares - country_decile_shares)^2)

  # Theoretical Gini
  # a partir dos valores teóricos obtidos calcula-se o gini teórico tambem
  theoretical_gini <- weighted.gini(x = sample$sim_sample, w = sample$weight_sample)$Gini

  # e é computado o erro, comparando com o erro do uruguay
  error_gini <- (theoretical_gini - country_gini_index)^2

  # Total loss function (weights can be adjusted if needed)
  # calcula-se entao o erro compartilhado
  return(error_shares + error_gini)
    }
    # Optimize sigma

    # calcula-se um sigma inicial com base no gini do uruguai
    initial_sigma <- sqrt(2) * qnorm((country_gini_index + 1) / 2)

    # utiliza-se a funcao optim para encontrar o sigma que minimiza o erro da função acima
    opt_result <- optim(par = initial_sigma, fn = objective_function, method = "BFGS")

    # extrai o sigma do resultado
    sigma_target <- opt_result$par

    # calcula o mu a partir do sigma obtido
    mu_target <- log(mean_income) - (sigma_target^2) / 2

    # Define the target quantile function based on these parameters.
    target_quantile <- function(q) {
      exp(mu_target + sigma_target * qnorm(q))
    }

  func_name <- paste0("target_quantile_", gsub("[^a-zA-Z0-9]", "_", tolower(country_sigla)))
  assign(func_name, target_quantile, envir = .GlobalEnv)

    return(data.frame(
    country = country_sigla,
    mu = mu_target,
    sigma = sigma_target
  ))

}

# Obtendo as target functions for each countries

results_final  <- data.frame(
    country = NULL,
    mu = NULL,
    sigma = NULL
  )
for (country_name in unique(countries_income_share$Country)) {
    cat("Processing country:", country_name, "\n")

    gini_country  <- gini_indexes %>% filter(pais == country_name) %>% pull(gini)/100

    deciles_shares_country  <- countries_income_share %>%
                                  filter(Country == country_name) %>%
                                  pull(IncomeShareAccumulated)
    deciles_shares_country[10] <- 1

    sigla  <- gini_indexes %>% filter(pais == country_name) %>% pull(sigla_pais)

    # Create target quantile function for this country
    result  <- fit_log_normal(
      country_decile_shares = deciles_shares_country,
      country_gini_index = gini_country,
      country_sigla = sigla
      )

    # Store result
    results_final <- bind_rows(results_final,result)

    cat("Completed processing for", country_name, "\n")
  }


# ========= 3. Quantile Mapping =========
brazil_income_data_df_c_renda_ajustada <- brazil_income_data_df %>%
  arrange(VD5008_DEF) %>%
  mutate(cum_weight = cumsum(V1032),
         total_weight = sum(V1032, na.rm = TRUE),
         emp_quantile = cum_weight / total_weight,
         emp_quantile_adjusted = pmin(pmax(cum_weight / total_weight, 1e-7), 1 - 1e-7),
         VD5008_DEF_adjusted_URU = target_quantile_uru(emp_quantile_adjusted),
         VD5008_DEF_adjusted_EUA = target_quantile_eua(emp_quantile_adjusted),
         VD5008_DEF_adjusted_ESP = target_quantile_esp(emp_quantile_adjusted),
          VD5008_DEF_adjusted_FIN = target_quantile_fin(emp_quantile_adjusted),
          VD5008_DEF_adjusted_MEX = target_quantile_mex(emp_quantile_adjusted)
         )

tail(brazil_income_data_df_c_renda_ajustada %>%
        select(orig_order,cum_weight,
              total_weight,
              emp_quantile,
              emp_quantile_adjusted,
              VD5008_DEF,
              VD5008_DEF_adjusted_URU,
              VD5008_DEF_adjusted_EUA,
              VD5008_DEF_adjusted_ESP,
              VD5008_DEF_adjusted_FIN,
              VD5008_DEF_adjusted_MEX))

# Compute weighted means of the specified income variables using V1032 as weights
brazil_income_data_df_c_renda_ajustada <- brazil_income_data_df_c_renda_ajustada %>%
  mutate(
    mean_VD5008_DEF = weighted.mean(VD5008_DEF, V1032, na.rm = TRUE),
    mean_VD5008_DEF_adjusted_URU = weighted.mean(VD5008_DEF_adjusted_URU, V1032, na.rm = TRUE),
    mean_VD5008_DEF_adjusted_EUA = weighted.mean(VD5008_DEF_adjusted_EUA, V1032, na.rm = TRUE),
    mean_VD5008_DEF_adjusted_ESP = weighted.mean(VD5008_DEF_adjusted_ESP, V1032, na.rm = TRUE),
    mean_VD5008_DEF_adjusted_FIN = weighted.mean(VD5008_DEF_adjusted_FIN, V1032, na.rm = TRUE),
    mean_VD5008_DEF_adjusted_MEX = weighted.mean(VD5008_DEF_adjusted_MEX, V1032, na.rm = TRUE),

    scaling_factor_uru = mean_VD5008_DEF_adjusted_URU/mean_VD5008_DEF,
    scaling_factor_eua = mean_VD5008_DEF_adjusted_EUA/mean_VD5008_DEF,
    scaling_factor_esp = mean_VD5008_DEF_adjusted_ESP/mean_VD5008_DEF,
    scaling_factor_fin =mean_VD5008_DEF_adjusted_FIN/mean_VD5008_DEF,
    scaling_factor_mex = mean_VD5008_DEF_adjusted_MEX/mean_VD5008_DEF,

    VD5008_DEF_adjusted_URU = VD5008_DEF_adjusted_URU * scaling_factor_uru,
    VD5008_DEF_adjusted_EUA = VD5008_DEF_adjusted_EUA * scaling_factor_eua,
    VD5008_DEF_adjusted_ESP = VD5008_DEF_adjusted_ESP * scaling_factor_esp,
    VD5008_DEF_adjusted_FIN = VD5008_DEF_adjusted_FIN * scaling_factor_fin,
    VD5008_DEF_adjusted_MEX = VD5008_DEF_adjusted_MEX * scaling_factor_mex
  )

# ========= 4. Update Survey Design =========
# 4.1 atualizando a base com a ordem original
# verifica-se abaixo que as bases estão em ordens diferentes:
head(brazil_income_data_df %>% select(orig_order, VD5008_DEF))
head(brazil_income_data_df_c_renda_ajustada %>% select(orig_order,
                                                      VD5008_DEF,
                                                      VD5008_DEF_adjusted_URU,
                                                      VD5008_DEF_adjusted_EUA,
                                                      VD5008_DEF_adjusted_ESP,
                                                      VD5008_DEF_adjusted_FIN,
                                                      VD5008_DEF_adjusted_MEX))

# Portanto, é necessário garantir que os novos valores de renda estejam atribuídos a observações corretas
brazil_income_data_df_final <- brazil_income_data_df %>%
  left_join(brazil_income_data_df_c_renda_ajustada %>%
                select(orig_order, VD5008_DEF_adjusted_URU,
                                                      VD5008_DEF_adjusted_EUA,
                                                      VD5008_DEF_adjusted_ESP,
                                                      VD5008_DEF_adjusted_FIN,
                                                      VD5008_DEF_adjusted_MEX),
            by = "orig_order") %>%
  arrange(orig_order)

head(brazil_income_data_df_final%>% select(orig_order,
                                            VD5008_DEF,
                                            VD5008_DEF_adjusted_URU,
                                            VD5008_DEF_adjusted_EUA,
                                            VD5008_DEF_adjusted_ESP,
                                            VD5008_DEF_adjusted_FIN,
                                            VD5008_DEF_adjusted_MEX))

brazil_income_data_adjusted <- update(brazil_income_data,
                                      VD5008_DEF_adjusted_URU = brazil_income_data_df_final$VD5008_DEF_adjusted_URU,
                                      VD5008_DEF_adjusted_EUA = brazil_income_data_df_final$VD5008_DEF_adjusted_EUA,
                                      VD5008_DEF_adjusted_ESP = brazil_income_data_df_final$VD5008_DEF_adjusted_ESP,
                                      VD5008_DEF_adjusted_FIN = brazil_income_data_df_final$VD5008_DEF_adjusted_FIN,
                                      VD5008_DEF_adjusted_MEX = brazil_income_data_df_final$VD5008_DEF_adjusted_MEX)

# ========= 5. Validação dos resultados ----

# 5.1 Renda média idêntica --------
svymean(~VD5008_DEF+
        VD5008_DEF_adjusted_URU+
        VD5008_DEF_adjusted_EUA+
        VD5008_DEF_adjusted_ESP+
        VD5008_DEF_adjusted_FIN+
        VD5008_DEF_adjusted_MEX,
        brazil_income_data_adjusted,
        na.rm = TRUE)

# 5.2 índice de Gini -------
library(convey)

brazil_income_data_adjusted <- convey_prep(brazil_income_data_adjusted)

original_gini_brazil  <- svygini(~VD5008_DEF,
        design = brazil_income_data_adjusted,
        na.rm = TRUE)
adjusted_gini_spain  <- svygini(~VD5008_DEF_adjusted_ESP,
        design = brazil_income_data_adjusted,
        na.rm = TRUE)
adjusted_gini_finland <- svygini(~VD5008_DEF_adjusted_FIN,
        design = brazil_income_data_adjusted,
        na.rm = TRUE)
adjusted_gini_mexico <- svygini(~VD5008_DEF_adjusted_MEX,
        design = brazil_income_data_adjusted,
        na.rm = TRUE)
adjusted_gini_eua <- svygini(~VD5008_DEF_adjusted_EUA,
        design = brazil_income_data_adjusted,
        na.rm = TRUE)
adjusted_gini_uru <- svygini(~VD5008_DEF_adjusted_URU,
        design = brazil_income_data_adjusted,
        na.rm = TRUE)

gini_indexes


# 5.3 distribuição entre os decis ------

# Helper function to compute Lorenz curve for a given income variable
source("./script/_dataviz settings.R")
compute_lorenz <- function(var, label) {
  # Create a formula for the survey functions
  form <- as.formula(paste0("~", var))

  # Compute deciles (quantiles)
  quantiles <- svyquantile(form, brazil_income_data_adjusted, seq(0, 1, 0.1), na.rm = TRUE)
  quantile_vec <- quantiles[[var]][, 1]

  # Compute income share by decile groups
  share <- svyby(form,
                 as.formula(paste0("~cut(", var, ", breaks = unique(quantile_vec), include.lowest = TRUE)")),
                 brazil_income_data_adjusted,
                 svytotal)

  # Calculate cumulative income and population
  n <- nrow(share)
  share %>%
    mutate(cumulative_income = cumsum(.data[[var]]) / sum(.data[[var]]),
           cumulative_population = seq(1/n, 1, length.out = n),
           renda = label) %>%
    select(cumulative_income,cumulative_population,renda)
}

var_income = "VD5008_DEF_adjusted_FIN"
country_desc = "Finlândia"
plot_lorenz_curves  <- function(var_income, country_desc){
  # Compute Lorenz curves for the two income variables
  lorenz_curve          <- compute_lorenz("VD5008_DEF", "Brasil")
  adjusted_lorenz_curve <- compute_lorenz(var_income, paste("Ajuste",country_desc) )

  # Load and adjust Uruguay's Lorenz curve data
  country_lorenz_curve <- countries_income_share %>%
    filter(Country == country_desc ) %>%
    mutate(cumulative_population = Decile / 10,
          cumulative_income     = IncomeShareAccumulated,
          renda = country_desc) %>%
    select(cumulative_population, cumulative_income,renda)

  # Combine all curves and plot them
  both_lorenz <- bind_rows(lorenz_curve, adjusted_lorenz_curve, country_lorenz_curve)



  # Create the Lorenz curve plot
  ggplot(both_lorenz, aes(x = cumulative_population,
                          y = cumulative_income,
                          color = renda)) +
    geom_line(size = 1, alpha = 0.5) +
    # Direct labeling using the last point of each curve
    labs(title = "Comparação da Curva de Lorenz da renda",
        subtitle = paste0("Renda do Brasil, do ",country_desc," e ajustada pela distribuição do ",country_desc),
        col = "",
        x = "População acumulada",
        y = "Participação acumulada na renda",
        caption = "Fontes: PNADc(2023) & Banco Mundial") +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal(base_size = 12) +
    theme_swd()+
    theme(
          legend.position = "top",  # legend removed in favor of direct labeling
          legend.justification='left',
          #plot.title = element_text(face = "bold", hjust = 0.5),
          #plot.subtitle = element_text(hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))



}

plot_lorenz_curves("VD5008_DEF_adjusted_URU", "Uruguai")

# ======= 6. saving result ======

brazil_income_data_adjusted %>% saveRDS("./intermediarios/renda_PNADc_brasil2023visita_1_ajustada_paises.rds")
