library(tidyverse)
library(data.table)

# Read the .dta file
gini_data <- fread("./entradas/swiid9_8/swiid9_8_summary.csv")

gini_data %>% glimpse()
# Filter and select the data for Uruguay
uruguay_gini <- gini_data %>% 
        filter(country == "Uruguay")

south_america_gini  <- gini_data %>% 
        filter(country %in% c("Argentina",
                              "Bolivia",
                              "Brazil",
                              "Chile",
                              "Colombia",
                              "Ecuador",
                              "Paraguay",
                              "Peru",
                              "Uruguay",
                              "Venezuela"))

last_south_america_gini <-  south_america_gini %>% 
    group_by(country) %>% 
    filter(year == max(year)) %>% 
    ungroup()


# Save the plot to a PNG file
png(filename = "./saidas/latest_gini_dispersion_south_america.png", width = 1200, height = 800, res = 150)
last_south_america_gini  %>% 
    ggplot(aes(x = reorder(country, gini_disp), y = gini_disp)) +
    geom_col(fill = "skyblue") +
    geom_errorbar(aes(ymin = gini_disp - gini_disp_se, ymax = gini_disp + gini_disp_se), width = 0.2, col = "blue") +
    geom_text(aes(label = paste0(year, ": ", round(gini_disp, 2))), 
              vjust = 1.5, size = 4, hjust = 1.1) +
    coord_flip() +
    labs(title = "Último gini da renda disponível - América do Sul", 
         x = "País", y = "Gini da renda disponível",
         subtitle = "Dados extraídos do Standardized World Inequality Income Database, 2025") +
    theme_minimal() +
    theme(text = element_text(size = 12))

# Turn off the device
dev.off()


# Save the line plot to a PNG file
png(filename = "./saidas/gini_evolution_south_america.png", width = 1200, height = 800, res = 150)
# Create a line plot with direct labels for country names and gini_disp labels for each ten years
ggplot(south_america_gini, aes(x = year, y = gini_disp, col = country)) +
    geom_line() +
    geom_text(data = south_america_gini %>% group_by(country) %>% filter(year == max(year)), 
              aes(label = country), 
              hjust = -0.1, 
              vjust = 0.5, 
              size = 4) +
    #geom_text(data = south_america_gini %>% filter(year %% 10 == 0 | year == min(year) | year == max(year)), 
    #          aes(label = round(gini_disp, 2)), 
    #          vjust = -0.5, 
    #          size = 3) +
    labs(title = "Evolução do Gini da renda disponível - América do Sul", x = "ano", y = "Gini da renda disponível") +
    theme_minimal() +
    theme(text = element_text(size = 12), legend.position = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15)))  # Increase the limits of the x-axis to ensure labels are readable

# Turn off the device
dev.off()
# Display the first few rows of the data
head(data)

# será utilizado então o Gini da renda disponível do uruguai, que é igual a 37.9