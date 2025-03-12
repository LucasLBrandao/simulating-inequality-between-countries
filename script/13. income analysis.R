# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
# Optionally, use patchwork or gridExtra if you want to combine multiple plots later
# library(patchwork)

source("./script/_dataviz settings.R")

income_df <- readRDS("./intermediarios/income_results_countries_sample_with_age_and_deciles.rds")
income_df %>% openxlsx::write.xlsx("./saidas/analise_pobreza_paises.xlsx")
income_df %>% data.table::fwrite("./saidas/analise_pobreza_paises.csv")
income_df %>% colnames()
income_df %>% View()


create_income_plot  <- function(data,original_variable_name,adjusted_variable_name){
  income_df_tratado   <- data %>%
      filter(Category_Value != "Acima de 14 anos") %>%
      arrange(-(get(original_variable_name))) %>%
      mutate(Category_Value = factor(Category_Value, levels = unique(Category_Value))) %>%
      mutate(Category_Value = relevel(Category_Value, ref = "Geral")) %>%
      mutate(Category_Value = factor(Category_Value, levels = rev(levels(Category_Value))))
min_value <- min(income_df_tratado[[original_variable_name]]) * 0.95
max_value <- max(income_df_tratado[[adjusted_variable_name]]) * 1.05
  ggplot(income_df_tratado,
    aes(y = Category_Value, color = reorder(country_name, Adjusted_Median)))+
    geom_point(aes(x = get(original_variable_name), color = "Brasil"),
      shape = "|",
    size = 4)+
    geom_point(aes(x = get(adjusted_variable_name)), size = 2)+
    theme_swd()+
    labs(col = "")+
    ggrepel::geom_text_repel(aes(x = get(adjusted_variable_name),
                  label = scales::comma(round(get(adjusted_variable_name) , 1),big.mark = ".",decimal.mark = ",")),
                  vjust = -1, size = 3)+
    geom_text(aes(x = get(original_variable_name),
    label = scales::comma(round(get(original_variable_name) , 1),big.mark = ".",decimal.mark = ","),color = "Brasil"),
    size = 3, hjust = 1.1)+
    scale_color_manual(values = c("Brasil" = "black",
                                    "Finlândia" = "#1a8abe" ,
                                    "Espanha" = "#58508d",
                                    "Estados Unidos" = "#bc5090",
                                    "Uruguai" = "#ff6361",
                                    "México" = "#ffa600"))+
    labs(x = "Mediana (R$)",
          y = "",
          col = "") +
    scale_x_continuous(n.breaks = 6,
                      labels = scales::label_comma(big.mark = ".", decimal.mark = ","),
                      limits = c(min_value,max_value))+
      # Theme customization
      theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.justification = "right",
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          legend.spacing.y = unit(-0.1, "cm"),
          panel.grid.major.y = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10)) +
      guides(color = guide_legend(override.aes = list(label = "",
            size = 2), nrow = 1))

}
options(vsc.dev.args = list(width=2000, height=1500, pointsize=12, res=300))

create_income_plot(income_df, "Original_Median","Adjusted_Median")
create_income_plot(income_df, "Extrema Pobreza")

create_difference_plot <- function(data,original_variable_name,adjusted_variable_name) {
  # Filter data for the specified threshold
  plot_data <- data %>%
    filter(Category_Value != "Acima de 14 anos") %>%
    arrange(-(get(original_variable_name))) %>%
    mutate(Rate_Difference = (get(adjusted_variable_name)*100/get(original_variable_name)) - 100 ) %>%
    mutate(Category_Value = factor(Category_Value, levels = unique(Category_Value))) %>%
    mutate(Category_Value = relevel(Category_Value, ref = "Geral")) %>%
    mutate(Category_Value = factor(Category_Value, levels = rev(levels(Category_Value)))) %>%
    arrange((Rate_Difference)) %>%
    mutate(country_name = factor(country_name, levels = unique(country_name)))

  # Find min and max difference for proper scaling
  #min_diff <- min(plot_data$Rate_Difference) * 1.1
  #max_diff <- 0  # Since all differences are negative

  # Create plot
  ggplot(plot_data, aes(y = Category_Value,
     x = Rate_Difference, color = country_name)) +
    # Add reference line at 0
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    # Points for rate differences
    geom_point(size = 2.5) +
    # Add labels for differences
    ggrepel::geom_text_repel(aes(label = paste0("+",round(Rate_Difference, 1))),
              vjust = -1, size = 3) +
    # Format x-axis as percentage
    #scale_x_continuous(
    #                   limits = c(min_diff, max_diff)) +
    # Color scheme for countries
    scale_color_manual(values = c("Brasil" = "black",
                                    "Finlândia" = "#1a8abe" ,
                                    "Espanha" = "#58508d",
                                    "Estados Unidos" = "#bc5090",
                                    "Uruguai" = "#ff6361",
                                    "México" = "#ffa600")) +
    # Facet by category
   # facet_wrap(~Category, scales = "free_y") +
    # Labels and title
    labs(x = "Diferença na Mediana (%)",
         y = "",
         col = "") +
    # Theme customization
    theme_swd() +
    theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "right",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.spacing.y = unit(-0.2, "cm"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10)) +
    guides(color = guide_legend(override.aes = list(label = "", size = 2)))
}
# Create difference plots
create_difference_plot(income_df, "Original_Median","Adjusted_Median")
base_decis_geral  <- income_df %>% select(country_name, Category_Value, contains("decil")) %>%
            filter(Category_Value == "Geral") %>%
            pivot_longer(cols = contains("decil"), names_to = "decil", values_to = "value") %>%
            separate(decil, into = c("decil", "quantile"), sep = "_") %>%
            pivot_wider(names_from = decil, values_from = value) %>%
            mutate(var = (Adjusted*100/Original) - 100)  %>%
            mutate(quantile = as.integer(gsub("º", "", substr(quantile, 1, 2))))

base_decis_geral %>%
  ggplot(aes(x = quantile, y = var, color = country_name)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line( )+#se = FALSE, method = "loess") +
  # Add points for the first and last decil for each country
  geom_point(data = base_decis_geral %>%
               group_by(country_name) %>%
               filter(quantile == min(quantile) | quantile == max(quantile)) %>%
               ungroup(),
             aes(x = quantile, y = var, color = country_name)) +
  # Add text labels for the first and last decil for each country
  geom_text(data = base_decis_geral %>%
              group_by(country_name) %>%
              filter(quantile == min(quantile) | quantile == max(quantile)) %>%
              ungroup(),
            aes(x = quantile, y = var, label = round(var, 1), color = country_name,
                hjust = ifelse(var < 0, -0.5, 1.1)),
            vjust = 0) +
  labs(x = "Decil",
       y = "Diferença no decil (%)",
       col = "") +
  scale_color_manual(values = c("Brasil" = "black",
                                "Finlândia" = "#1a8abe",
                                "Espanha" = "#58508d",
                                "Estados Unidos" = "#bc5090",
                                "Uruguai" = "#ff6361",
                                "México" = "#ffa600")) +
  scale_x_continuous(breaks = c(seq(1, 10, by = 1)), limits = c(0.55, 10.5)) +
  scale_y_continuous(n.breaks = 6, limits = c(-87.5,160))+
  theme_swd() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "right",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.spacing.y = unit(-0.2, "cm"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_line(color = "gray90"),  # vertical grid lines
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10)) +
  guides(color = guide_legend(override.aes = list(label = "", size = 2)))

# ---------------------------
# Overall Comparison Plot
# ---------------------------

create_specific_country_income_plot  <- function(data,selected_country){
  ggplot(data %>% filter(country_name == selected_country), aes(x = Category_Value)) +
    # Connect the two points for each category with a dashed line
    geom_segment(aes(x = Category_Value, xend = Category_Value,
                    y = get(original_variable_name), yend = get(adjusted_variable_name)),
                linetype = "solid", size = 0.5, color = "gray50") +
    # Plot original and adjusted rates as points
    geom_point(aes(y = get(original_variable_name), color = "Renda"), size = 3) +
    geom_point(aes(y = get(adjusted_variable_name), color = "Renda Ajustada"), size = 3) +
    # Add data labels for original and adjusted rates
    geom_text(aes(y = get(original_variable_name), label = round(get(original_variable_name) * 100, 1)),
              vjust = -1, color = "#b34739", size = 3) +
    geom_text(aes(y = get(adjusted_variable_name), label = round(get(adjusted_variable_name) * 100, 1)),
              vjust = -1, color = "#5575ba", size = 3) +
    # Add data label for rate difference
    geom_text(aes(y = (get(original_variable_name) + get(adjusted_variable_name)) / 2,
                  label = round(Rate_Difference * 100, 1)),
              vjust = 1.5, color = "gray50", size = 2) +
    # Facet by income threshold type
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


}

create_specific_country_income_plot(income_df, "Finlândia")

