# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(scales)  # For percentage formatting

# Read the CSV file
poverty_data <- read_csv("./saidas/analise_pobreza_paises.csv")

# Function to create dumbbell plots for each threshold
create_dumbbell_plot <- function(data, threshold_value) {
  # Filter data for the specified threshold
  plot_data <- data %>%
    filter(threshold_name == threshold_value) %>%
    # Sort categories by original rate for better visualization
    arrange(desc(Original_Rate)) %>%
    mutate(Category_Value = factor(Category_Value, levels = unique(Category_Value)))

  # Create reference data for original rates (only need one per category)
  reference_data <- plot_data %>%
    group_by(Category_Value, Original_Rate) %>%
    slice(1) %>%
    ungroup()

  # Create the dumbbell plot
  ggplot(plot_data, aes(y = Category_Value)) +
    # Segments connecting original and adjusted rates
    geom_segment(aes(x = Original_Rate, xend = Adjusted_Rate,
                     yend = Category_Value, color = country_name),
                 size = 0.75) +
    # Points for original rates (gray)
    geom_point(aes(x = Original_Rate), color = "gray30", size = 3) +
    # Points for adjusted rates (colored by country)
    geom_point(aes(x = Adjusted_Rate, color = country_name), size = 4) +
    # Add labels for original rates
    geom_text(data = reference_data,
              aes(x = Original_Rate,
                  label = scales::percent(Original_Rate, accuracy = 0.1)),
              hjust = 1.3, color = "gray30", size = 3) +
    # Format x-axis as percentage
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    # Color scheme for countries
    scale_color_brewer(palette = "Set1", name = "Country") +
    # Facet by category to separate demographic groups
    #facet_wrap(~Category, scales = "free_y") +
    # Labels and title
    labs(title = paste("Comparison of Original vs Adjusted", threshold_value, "Rates"),
         subtitle = "Gray dots: Original rates (same across countries) | Colored dots: Country-specific adjusted rates",
         x = "Rate",
         y = "") +
    # Theme customization
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10))
}
source("./script/_dataviz settings.R")
# Create plots for both thresholds
pobreza_plot <- create_dumbbell_plot(poverty_data, "Pobreza")
extrema_pobreza_plot <- create_dumbbell_plot(poverty_data, "Extrema Pobreza")

# Display plots
print(pobreza_plot)
print(extrema_pobreza_plot)

# Save plots
ggsave("pobreza_comparison.png", pobreza_plot, width = 12, height = 8)
ggsave("extrema_pobreza_comparison.png", extrema_pobreza_plot, width = 12, height = 8)

# Function to create a diverging difference plot
create_difference_plot <- function(data, threshold_value) {
  # Filter data for the specified threshold
  plot_data <- data %>%
    filter(threshold_name == threshold_value) %>%
    arrange(-(Original_Rate)) %>%
    mutate(Rate_Difference = Rate_Difference*100) %>%
    mutate(Category_Value = factor(Category_Value, levels = unique(Category_Value))) %>%
    mutate(Category_Value = relevel(Category_Value, ref = "Geral")) %>%
    mutate(Category_Value = factor(Category_Value, levels = rev(levels(Category_Value))))

  # Find min and max difference for proper scaling
  min_diff <- min(plot_data$Rate_Difference) * 1.1
  max_diff <- 0  # Since all differences are negative

  # Create plot
  ggplot(plot_data, aes(y = Category_Value, x = Rate_Difference, color = country_name)) +
    # Add reference line at 0
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    # Points for rate differences
    geom_point(size = 4) +
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
    theme(legend.position = c(0.05, 0.95),
        #legend.position  = "top",
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
    guides(color = guide_legend(override.aes = list(label = "", size = 4)))
}

# Create difference plots
pobreza_diff_plot <- create_difference_plot(poverty_data, "Pobreza")
extrema_pobreza_diff_plot <- create_difference_plot(poverty_data, "Extrema Pobreza")

# Display difference plots
print(pobreza_diff_plot)
print(extrema_pobreza_diff_plot)

# Save difference plots
ggsave("pobreza_difference.png", pobreza_diff_plot, width = 12, height = 8)
ggsave("extrema_pobreza_difference.png", extrema_pobreza_diff_plot, width = 12, height = 8)
