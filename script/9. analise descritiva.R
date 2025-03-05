library(tidyverse)
library(survey)

dados_renda_brasil  <- readRDS("./intermediarios/renda_PNADc_brasil2023visita_1_ajustada_uruguai.rds")

# Validação dos resultados - Comparação do resultado com o uruguai
# Renda média idêntica

renda_media_brasil <- svymean(~VD5008_DEF,
                             dados_renda_brasil,
                             na.rm = TRUE)

renda_media_ajustada_brasil <- svymean(~VD5008_DEF_adjusted,
                             dados_renda_brasil,
                             na.rm = TRUE)

# Distribuição da renda entre os decis
income_deciles <- svyquantile(~VD5008_DEF,
                             dados_renda_brasil,
                             seq(0, 1, 0.1),
                             na.rm = TRUE)

adjusted_income_deciles <- svyquantile(~VD5008_DEF_adjusted,
                             dados_renda_brasil,
                             seq(0, 1, 0.1),
                             na.rm = TRUE)

income_deciles_vector  <- income_deciles$VD5008_DEF[,1]

adjusted_income_deciles_vector  <- adjusted_income_deciles$VD5008_DEF_adjusted[,1]

income_share <- svyby(~VD5008_DEF,
                      ~cut(VD5008_DEF,
                           breaks = unique(income_deciles_vector),
                           include.lowest = TRUE),
                      dados_renda_brasil,
                      svytotal)

adjusted_income_share <- svyby(~VD5008_DEF_adjusted,
                      ~cut(VD5008_DEF_adjusted,
                           breaks = unique(adjusted_income_deciles_vector),
                           include.lowest = TRUE),
                      dados_renda_brasil,
                      svytotal)

lorenz_curve <- income_share %>%
  mutate(cumulative_income = cumsum(VD5008_DEF) / sum(VD5008_DEF),
         cumulative_population = cumsum(rep(1/nrow(income_share), nrow(income_share))),
         renda = "Renda")

adjusted_lorenz_curve <- adjusted_income_share %>%
  mutate(cumulative_income = cumsum(VD5008_DEF_adjusted) / sum(VD5008_DEF_adjusted),
         cumulative_population = cumsum(rep(1/nrow(income_share), nrow(income_share))),
         renda = "Renda ajustada")

uruguai_lorenz_curve  <-  readRDS("./intermediarios/income_share_accumulated_uruguay.rds") %>%
mutate(cumulative_population = Decile/10,
          cumulative_income = IncomeShareAccumulated/100) %>%
select(cumulative_population,cumulative_income)


both_lorenz = bind_rows(lorenz_curve,adjusted_lorenz_curve,uruguai_lorenz_curve)


# Gini

# Comparação das curvas de Lorenz entre os anos

both_lorenz %>% ggplot(aes(x = cumulative_population, y = cumulative_income, col = renda))+
geom_line()



# Comparação das taxas de pobreza e extrema pobreza =======

# Geral

# ========= 5. Poverty Calculation =========
poverty_line <- 1320 / 2
brazil_income_data <- update(brazil_income_data,
                             poor = as.numeric(VD5008_DEF < poverty_line))
brazil_income_data_adjusted <- update(brazil_income_data_adjusted,
                                      poor_adjusted = as.numeric(VD5008_DEF_adjusted < poverty_line))

# Compute poverty rates
original_poverty_rate <- as.numeric(svymean(~poor, brazil_income_data, na.rm = TRUE))
adjusted_poverty_rate <- as.numeric(svymean(~poor_adjusted, brazil_income_data_adjusted, na.rm = TRUE))

# ========= 6. Output =========
cat("Optimal sigma:", sigma_target, "\n")
cat("Optimal mu:", mu_target, "\n")
cat("Poverty line:", poverty_line, "\n")
cat("Original poverty rate:", original_poverty_rate, "\n")
cat("Adjusted poverty rate:", adjusted_poverty_rate, "\n")


# Por Gênero e raça
