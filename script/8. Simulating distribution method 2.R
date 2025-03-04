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
brazil_income_data  <- update(brazil_income_data,
                              orig_order = 1:nrow(brazil_income_data$variables))

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

uruguai_gini_index <- 0.406
uruguay_income_share <- readRDS("./intermediarios/income_share_accumulated_uruguay.rds")
uruguay_decile_shares  <- uruguay_income_share$IncomeShareAccumulated/100
uruguay_decile_shares[10] <- 1

# Define the optimization function
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
  error_shares <- sum((theoretical_cum_shares - uruguay_decile_shares)^2)

  # Theoretical Gini
  # a partir dos valores teóricos obtidos calcula-se o gini teórico tambem
  theoretical_gini <- weighted.gini(x = sample$sim_sample, w = sample$weight_sample)$Gini

  # e é computado o erro, comparando com o erro do uruguay
  error_gini <- (theoretical_gini - uruguai_gini_index)^2

  # Total loss function (weights can be adjusted if needed)
  # calcula-se entao o erro compartilhado
  return(error_shares + error_gini)
}

# Optimize sigma

# calcula-se um sigma inicial com base no gini do uruguai
initial_sigma <- sqrt(2) * qnorm((uruguai_gini_index + 1) / 2)

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

# ========= 3. Quantile Mapping =========
brazil_income_data_df_c_renda_ajustada <- brazil_income_data_df %>%
  arrange(VD5008_DEF) %>%
  mutate(cum_weight = cumsum(V1032),
         total_weight = sum(V1032, na.rm = TRUE),
         emp_quantile = cum_weight / total_weight,
         emp_quantile_adjusted = pmin(pmax(cum_weight / total_weight, 1e-7), 1 - 1e-7),
         VD5008_DEF_adjusted = target_quantile(emp_quantile_adjusted))

tail(brazil_income_data_df_c_renda_ajustada %>%
        select(orig_order,cum_weight,
              total_weight,
              emp_quantile,
              emp_quantile_adjusted,
              VD5008_DEF,
              VD5008_DEF_adjusted))

renda_media  <- weighted.mean(brazil_income_data_df_c_renda_ajustada$VD5008_DEF,
             brazil_income_data_df_c_renda_ajustada$V1032)

renda_media_ajustada  <- weighted.mean(brazil_income_data_df_c_renda_ajustada$VD5008_DEF_adjusted,
             brazil_income_data_df_c_renda_ajustada$V1032)

scaling_factor <- renda_media / renda_media_ajustada
brazil_income_data_df_c_renda_ajustada <- brazil_income_data_df_c_renda_ajustada %>%
  mutate(VD5008_DEF_adjusted = VD5008_DEF_adjusted * scaling_factor)

# ========= 4. Update Survey Design =========
# 4.1 atualizando a base com a ordem original
# verifica-se abaixo que as bases estão em ordens diferentes:
head(brazil_income_data_df %>% select(orig_order, VD5008_DEF))
head(brazil_income_data_df_c_renda_ajustada %>% select(orig_order, VD5008_DEF,VD5008_DEF_adjusted))

# Portanto, é necessário garantir que os novos valores de renda estejam atribuídos a observações corretas
brazil_income_data_df_final <- brazil_income_data_df %>%
  left_join(brazil_income_data_df_c_renda_ajustada %>%
                select(orig_order, VD5008_DEF_adjusted),
            by = "orig_order") %>%
  arrange(orig_order)

head(brazil_income_data_df_final%>% select(orig_order, VD5008_DEF,VD5008_DEF_adjusted))

brazil_income_data_adjusted <- update(brazil_income_data,
                                      VD5008_DEF_adjusted = brazil_income_data_df_final$VD5008_DEF_adjusted)
# ========= 5. Validação do resultado

# ======= 7. saving result ======

brazil_income_data_adjusted %>% saveRDS("./intermediarios/renda_PNADc_brasil2023visita_1_ajustada_uruguai.rds")
