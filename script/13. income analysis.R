# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
# Optionally, use patchwork or gridExtra if you want to combine multiple plots later
# library(patchwork)

source("./script/_dataviz settings.R")

income_df <- readRDS("./intermediarios/income_results_countries_sample.rds")
income_df %>% openxlsx::write.xlsx("./saidas/analise_pobreza_paises.xlsx")
income_df %>% data.table::fwrite("./saidas/analise_pobreza_paises.csv")
income_df %>% colnames()
income_df$threshold_name %>% unique()
income_df %>% View()

create_income_plot  <- function(data, threshold_value){
  income_df_tratado   <- data %>%
      filter(threshold_name == threshold_value) %>%
      arrange(-(Original_Rate)) %>%
      mutate(Adjusted_Rate = Adjusted_Rate*100,
          Original_Rate = Original_Rate*100) %>%
      mutate(Category_Value = factor(Category_Value, levels = unique(Category_Value))) %>%
      mutate(Category_Value = relevel(Category_Value, ref = "Geral")) %>%
      mutate(Category_Value = factor(Category_Value, levels = rev(levels(Category_Value)))) %>%
      arrange((Rate_Difference)) %>%
      mutate(country_name = factor(country_name, levels = unique(country_name)))


  ggplot(income_df_tratado,
    aes(y = Category_Value,color = country_name))+
    geom_point(aes(x = Adjusted_Rate), size = 2)+
    geom_point(aes(x = Original_Rate, color = "Brasil"),
      shape = "|",
    size = 4)+
    theme_swd()+
    labs(col = "")+
    ggrepel::geom_text_repel(aes(x = Adjusted_Rate,
                  label = round(Adjusted_Rate , 1)),
                  vjust = -1, size = 3)+
    geom_text(aes(x = Original_Rate,
    label = round(Original_Rate , 1),color = "Brasil"),
    size = 3, hjust = -0.1)+
    scale_color_manual(values = c("Brasil" = "#4c4c4c",
                                    "Finlândia" = "#51989a",
                                    "Espanha" = "#6189b9",
                                    "Estados Unidos" = "#d0c314",
                                    "Uruguai" = "#ed9375",
                                    "México" = "#cd4242"))+
    labs(x = "Taxa de pobreza (%)",
          y = "",
          col = "") +
    scale_x_continuous(n.breaks = 10)+
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

create_income_plot(income_df, "Pobreza")
create_income_plot(income_df, "Extrema Pobreza")

create_difference_plot <- function(data, threshold_value) {
  # Filter data for the specified threshold
  plot_data <- data %>%
    filter(threshold_name == threshold_value) %>%
    arrange(-(Original_Rate)) %>%
    mutate(Rate_Difference = Rate_Difference*100) %>%
    mutate(Category_Value = factor(Category_Value, levels = unique(Category_Value))) %>%
    mutate(Category_Value = relevel(Category_Value, ref = "Geral")) %>%
    mutate(Category_Value = factor(Category_Value, levels = rev(levels(Category_Value)))) %>%
    arrange((Rate_Difference)) %>%
    mutate(country_name = factor(country_name, levels = unique(country_name)))

  # Find min and max difference for proper scaling
  min_diff <- min(plot_data$Rate_Difference) * 1.1
  max_diff <- 0  # Since all differences are negative

  # Create plot
  ggplot(plot_data, aes(y = Category_Value,
     x = Rate_Difference, color = country_name)) +
    # Add reference line at 0
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    # Points for rate differences
    geom_point(size = 2) +
    # Add labels for differences
    ggrepel::geom_text_repel(aes(label = round(Rate_Difference, 1)),
              vjust = -1, size = 3) +
    # Format x-axis as percentage
    scale_x_continuous(
                       limits = c(min_diff, max_diff)) +
    # Color scheme for countries
    scale_color_manual(values = c("Brasil" = "#4c4c4c",
                                  "Finlândia" = "#51989a",
                                  "Espanha" = "#6189b9",
                                  "Estados Unidos" = "#d0c314",
                                  "Uruguai" = "#ed9375",
                                  "México" = "#cd4242")) +
    # Facet by category
   # facet_wrap(~Category, scales = "free_y") +
    # Labels and title
    labs(x = "Diferença na Taxa de pobreza (p.p.)",
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
create_difference_plot(income_data, "Pobreza")
create_difference_plot(income_data, "Extrema Pobreza")

# ---------------------------
# Overall Comparison Plot
# ---------------------------

create_specific_country_income_plot  <- function(data,selected_country){
  ggplot(data %>% filter(country_name == selected_country), aes(x = Category_Value)) +
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

