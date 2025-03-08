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

countries_income_share <- readRDS("./intermediarios/income_share_accumulated_countries_sample.rds")

countries_income_share$IncomeShareAccumulated  <- countries_income_share$IncomeShareAccumulated/100

# Função que ajusta a distribuição lognormal para os dados de um país
fit_log_normal <- function(country_decile_shares, country_gini_index,country_name) {
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

  func_name <- paste0("target_quantile_", gsub("[^a-zA-Z0-9]", "_", tolower(country_name)))
  assign(func_name, target_quantile, envir = .GlobalEnv)

    return(list(
    country = country_name,
    mu = mu_target,
    sigma = sigma_target
  ))

}

# Obtendo as target functions for each countries
country_name  <- "Estados Unidos"
for (country_name in unique(countries_income_share$Country)) {
    cat("Processing country:", country_name, "\n")

    gini_country  <- gini_indexes %>% filter(pais == country_name) %>% pull(gini)
    deciles_shares_country  <- countries_income_share %>%
                                  filter(Country == country_name) %>%
                                  pull(IncomeShareAccumulated)
    deciles_shares_country[10] <- 1

    # Create target quantile function for this country
    result <- create_target_quantile_function(
      country_name = country_name,
      country_decile_shares = deciles_shares_country,
      country_gini_index = gini_country,
      )

    # Store result
    results[[country_name]] <- result

    cat("Completed processing for", country_name, "\n")
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

cat("Optimal sigma:", sigma_target, "\n")
cat("Optimal mu:", mu_target, "\n")
# ========= 5. Validação dos resultados ----

# 5.1 Renda média idêntica --------
renda_media_brasil <- svymean(~VD5008_DEF,
                             brazil_income_data_adjusted,
                             na.rm = TRUE)

renda_media_ajustada_brasil <- svymean(~VD5008_DEF_adjusted,
                             brazil_income_data_adjusted,
                             na.rm = TRUE)
# 5.2 índice de Gini -------
library(convey)

brazil_income_data_adjusted <- convey_prep(brazil_income_data_adjusted)
gini_index_renda <- svygini(~VD5008_DEF,
                            design = brazil_income_data_adjusted,
                            na.rm = TRUE)

gini_index_renda_ajustada <- svygini(~VD5008_DEF_adjusted,
                                     design = brazil_income_data_adjusted,
                                     na.rm = TRUE)


# 5.3 distribuição entre os decis ------

# Helper function to compute Lorenz curve for a given income variable
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

# Compute Lorenz curves for the two income variables
lorenz_curve          <- compute_lorenz("VD5008_DEF", "Brasil")
adjusted_lorenz_curve <- compute_lorenz("VD5008_DEF_adjusted", "Brasil ajustado")

# Load and adjust Uruguay's Lorenz curve data
uruguai_lorenz_curve <- readRDS("./intermediarios/income_share_accumulated_uruguay.rds") %>%
  mutate(cumulative_population = Decile / 10,
         cumulative_income     = IncomeShareAccumulated / 100,
         renda = "Uruguai") %>%
  select(cumulative_population, cumulative_income,renda)

# Combine all curves and plot them
both_lorenz <- bind_rows(lorenz_curve, adjusted_lorenz_curve, uruguai_lorenz_curve)


# Plotting the lorenz
last_points <- both_lorenz %>%
  group_by(renda) %>%
  filter(cumulative_population == max(cumulative_population))

library(ggrepel)
source("./script/0. tema ggplot.R")
# Create the Lorenz curve plot
options(vsc.dev.args = list(width=1500, height=1500, pointsize=12, res=300))
png(filename = "./saidas/Comparação renda compartilhada Brasil, Brasil ajustado e Uruguai.png",width=1500, height=1500, pointsize=12, res=300)

ggplot(both_lorenz, aes(x = cumulative_population, y = cumulative_income, color = renda)) +
  geom_line(size = 1, alpha = 0.5) +
  # Direct labeling using the last point of each curve
  labs(title = "Comparação da Curva de Lorenz da renda",
       subtitle = "Renda do Brasil, do Uruguai e Renda do Brasil com distribuição do uruguai",
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
dev.off()
# Display the plot

# ======= 6. saving result ======

brazil_income_data_adjusted %>% saveRDS("./intermediarios/renda_PNADc_brasil2023visita_1_ajustada_uruguai.rds")
