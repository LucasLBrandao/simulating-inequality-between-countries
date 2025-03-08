library(data.table)
library(tidyverse)
income_share_for_each_decile  <- fread("./entradas/world-bank/income-share-for-each-decile.csv")

income_share_long <- income_share_for_each_decile %>%
  pivot_longer(cols = -c(Country, Year),
               names_to = "Decile", values_to = "IncomeShare")

income_share_long_acumulated <- income_share_long %>%
  mutate(Decile = case_when( Decile == "Poorest decile" ~ 1,
    Decile == "Richest decile" ~ 10,
    TRUE ~ as.numeric(substr(Decile, 1, 1))
  )) %>%
  arrange(Country, Year, Decile) %>%
  group_by(Country, Year) %>%
  mutate(IncomeShareAccumulated = cumsum(IncomeShare)) %>%
  ungroup()

# Filter the data frame to year equal to 2022
income_share_max_year <- income_share_long_acumulated %>%
  group_by(Country) %>%
  filter(Year == max(Year)) %>%
  ungroup()

# List of South American countries
sample_countries <- c("United States", "Spain", "Mexico", "Finland","Uruguay")

# Filter the data frame to include only South American countries
income_share_max_year_sa <- income_share_max_year %>%
  filter(Country %in% sample_countries)

# Filtering by uruguay and saving the data

income_share_max_year_sa %>%
  filter(Country %in% sample_countries) %>%
  saveRDS("./intermediarios/income_share_accumulated_countries_sample.rds")
