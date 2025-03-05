
poverty_df  <-  readRDS("./intermediarios/poverty_results.rds")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
# Optionally, use patchwork or gridExtra if you want to combine multiple plots later
# library(patchwork)

source("./script/_dataviz settings.R")

poverty_df <- readRDS("./intermediarios/poverty_results.rds")
poverty_df %>% openxlsx::write.xlsx("./saidas/analise_pobreza.xlsx")
poverty_df %>% colnames()
# ---------------------------
# Overall Comparison Plot
# ---------------------------
ggplot(poverty_df, aes(x = Category_Value)) +
  # Connect the two points for each category with a dashed line
  geom_segment(aes(x = Category_Value, xend = Category_Value,
                   y = Original_Rate, yend = Adjusted_Rate),
               linetype = "solid", size = 0.5, color = "gray50") +
  # Plot original and adjusted rates as points
  geom_point(aes(y = Original_Rate, color = "Renda"), size = 3) +
  geom_point(aes(y = Adjusted_Rate, color = "Renda Ajustada"), size = 3) +
  # Add data labels for original and adjusted rates
  geom_text(aes(y = Original_Rate, label = round(Original_Rate * 100, 1)),
            vjust = -1, color = "#b34739", size = 3) +
  geom_text(aes(y = Adjusted_Rate, label = round(Adjusted_Rate * 100, 1)),
            vjust = -1, color = "#5575ba", size = 3) +
  # Add data label for rate difference
  geom_text(aes(y = (Original_Rate + Adjusted_Rate) / 2,
                label = round(Rate_Difference * 100, 1)),
            vjust = 1.5, color = "gray50", size = 2) +
  # Facet by poverty threshold type
  facet_wrap(~ threshold_name, scales = "fixed") +
  # Define a consistent, accessible color palette
  scale_color_manual(values = c("Renda" = "#b34739",
                                "Renda Ajustada" = "#5575ba")) +
  labs(
       x = "Categoria demográfica",
       y = "Taxa de pobreza (%)",
       color = "") +
  theme_minimal() +
  theme(axis.line.y = element_line(size = .4, color = GRAY7),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "gray50"),
        axis.text.y = element_text(color = "gray50"),
        axis.title.x = element_text(hjust = 0, color = "gray50"),
        axis.title.y = element_text(vjust = 1, color = "gray50"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        legend.position = "top",
        legend.justification =  "left",
        axis.title.x.top = element_text(hjust = 0, color = "gray50")) +
  scale_y_continuous(labels = scales::percent,expand=c(0,0.05)) +
  #expand_limits(y = c(0,0.45))+
  coord_flip()
