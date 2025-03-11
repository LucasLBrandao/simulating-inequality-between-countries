library(tidyverse)

# Load data
brazil_income_share <- readRDS("./intermediarios/income_share_accumulated_brazil.rds")
other_countries_income_share <- readRDS("./intermediarios/income_share_accumulated_countries_sample.rds")

income_share_brazil_countries  <- bind_rows(brazil_income_share,
                                          other_countries_income_share) %>%
                                          mutate(Decile = as.integer(Decile))

row.names(income_share_brazil_countries)  <- NULL
income_share_brazil_countries <- income_share_brazil_countries %>%
                                   mutate(Country = case_when(
                                             Country == "Finland" ~ "Finlândia",
                                             Country == "Uruguay" ~ "Uruguai",
                                             Country == "Mexico" ~ "México",
                                             Country == "Spain" ~ "Espanha",
                                             Country == "United States" ~ "Estados Unidos",
                                             TRUE ~ Country))


# Create a line chart
source("./script/_dataviz settings.R")
png(filename = "./saidas/curva de lorenz Brasil e outros paises.png", width = 1200, height = 800, res = 150)
ggplot(income_share_brazil_countries,
       aes(x = Decile, y = IncomeShareAccumulated,
           color = Country)) +
  geom_line(size = 1) +
  theme_swd() +
  theme(legend.position = "right") +
  labs(title = "Curva de Lorenz - Brasil (2023) e Uruguai (2022)",
       x = "Decil",
       y = "% da Renda compartilhada acumulada",
       color = "")
dev.off()

