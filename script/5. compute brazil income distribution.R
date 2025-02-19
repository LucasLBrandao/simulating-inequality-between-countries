library(tidyverse)
library(data.table)
library(survey)

brazil_income_data  <- readRDS("./intermediarios/renda_PNADc_brasil2023visita_1.rds")

# Calculate income share by each decile
income_deciles <- svyquantile(~VD5008_DEF,
                             brazil_income_data,
                             seq(0, 1, 0.1),
                             na.rm = TRUE)

income_deciles_vector  <- income_deciles$VD5008_DEF[,1]

income_share <- svyby(~VD5008_DEF,
                      ~cut(VD5008_DEF,
                           breaks = income_deciles_vector,
                           include.lowest = TRUE),
                      brazil_income_data,
                      svytotal)

# Create Lorenz curve
lorenz_curve <- income_share %>%
  mutate(cumulative_income = cumsum(VD5008_DEF) / sum(VD5008_DEF),
         cumulative_population = cumsum(rep(1/nrow(income_share), nrow(income_share))))

# Calculate Gini index
gini_index <- 1 - 2 * sum(lorenz_curve$cumulative_income[-1] * diff(lorenz_curve$cumulative_population))

# Plot Lorenz curve using ggplot2
ggplot(lorenz_curve, aes(x = cumulative_population, y = cumulative_income)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Lorenz Curve for Brazil Income Distribution",
       x = "Cumulative Population",
       y = "Cumulative Income") +
  theme_minimal()

colnames(lorenz_curve)  <- c("interval","income","se",
                             "IncomeShareAccumulated","Decile")
income_share_accumu_brazil <- lorenz_curve %>%
  mutate(Decile = Decile * 10,
         IncomeShare = income * 100 / sum(income),
         Country = "Brasil",
         Year = 2023) %>%
  select(Country, Year, Decile, IncomeShare, IncomeShareAccumulated)

saveRDS(income_share_accumu_brazil,
        "./intermediarios/income_share_accumulated_brazil.rds")
