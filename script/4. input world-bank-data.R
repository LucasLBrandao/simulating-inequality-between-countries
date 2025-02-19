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
south_american_countries <- c("Argentina", "Bolivia", "Brazil", "Chile",
                              "Colombia", "Ecuador", "Guyana", "Paraguay",
                              "Peru", "Suriname", "Uruguay", "Venezuela")

# Filter the data frame to include only South American countries
income_share_max_year_sa <- income_share_max_year %>%
  filter(Country %in% south_american_countries)

# Adjust plot view options for better readability in VSCode plot viewer
options(vsc.dev.args = list(width=1500, height=1500, pointsize=12, res=300))
# Plot the data
ggplot(income_share_max_year_sa, aes(x = Decile,
                                y = IncomeShareAccumulated,
                                color = Country)) +
  geom_line(size = 1) +
  labs(title = "Income Share Accumulated by Decile in 2022",
       x = "Decile",
       y = "Accumulated Income Share",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Filtering by uruguay and saving the data

income_share_max_year_sa %>%
  filter(Country == "Uruguay") %>%
  saveRDS("./intermediarios/income_share_accumulated_uruguay.rds")
