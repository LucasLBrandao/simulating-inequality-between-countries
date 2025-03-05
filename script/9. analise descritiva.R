library(tidyverse)
library(survey)
library(convey)

brazil_income_data  <- readRDS("./intermediarios/renda_PNADc_brasil2023visita_1_ajustada_uruguai.rds")


# Análise descritiva das duas rendas
# Load the convey package

# Prepare the survey design object
brazil_income_data <- convey_prep(brazil_income_data)

# Function to calculate metrics

calculate_metrics <- function(var) {
     mean_val <- svymean(~get(var), brazil_income_data)[1]
     median_val <- svyquantile(~get(var), brazil_income_data, 0.5)[[1]][1]
     sd_val <- sqrt(svyvar(~get(var), brazil_income_data))[1]
     gini_val <- svygini(~get(var), brazil_income_data)[1]
     quantiles <- svyquantile(~get(var), brazil_income_data, c(0.1, 0.25, 0.75, 0.9, 0.99, 1))
     q10  <- quantiles[[1]][1]
     q25 <- quantiles[[1]][2]
     q75 <- quantiles[[1]][3]
     q90 <- quantiles[[1]][4]
     q99 <- quantiles[[1]][5]
     q100 <- quantiles[[1]][6]

     return(c(mean = mean_val, median = median_val, sd = sd_val, gini = gini_val,
          q10 = q10,q25 = q25, q75 = q75, q90 = q90, q99 = q99, q100 = q100))
}

# Calculate metrics for both variables
metrics_VD5008_DEF <- calculate_metrics("VD5008_DEF")
metrics_VD5008_DEF_adjusted <- calculate_metrics("VD5008_DEF_adjusted")

# Create data frame
metrics_df <- data.frame(
     Metric = names(metrics_VD5008_DEF),
     VD5008_DEF = metrics_VD5008_DEF,
     VD5008_DEF_adjusted = metrics_VD5008_DEF_adjusted
)

# Calculate absolute difference and percent change
metrics_df$Absolute_Difference <- abs(metrics_df$VD5008_DEF - metrics_df$VD5008_DEF_adjusted)
metrics_df$Percent_Change <- (metrics_df$Absolute_Difference / metrics_df$VD5008_DEF) * 100

metrics_df %>% openxlsx::write.xlsx("./saidas/analise_descritiva_rendas.xlsx")

# Calculate difference between all the percentiles
calculate_percentiles  <- function(var){
quantiles <- svyquantile(~get(var), brazil_income_data, seq(0.01, 1, 0.01))
df_quantile  <- quantiles[[1]]
as.data.frame(df_quantile)$quantile
lista  <- as.data.frame(df_quantile)$quantile
return(lista)
}

percentil_renda  <- calculate_percentiles("VD5008_DEF")
percentil_renda_ajustada <- calculate_percentiles("VD5008_DEF_adjusted")

percentis_df <- data.frame(
     Percentile = as.integer(seq(1, 100, 1)),
     VD5008_DEF = percentil_renda,
     VD5008_DEF_adjusted = percentil_renda_ajustada
)

percentis_df$Absolute_Difference <- -(percentis_df$VD5008_DEF - percentis_df$VD5008_DEF_adjusted)
percentis_df$Percent_Change <- (percentis_df$Absolute_Difference / percentis_df$VD5008_DEF) * 100

source("./script/_dataviz settings.R")

last_point <- percentis_df %>% filter(Percent_Change > 0) %>% tail(1)
last_x <- last_point$Percentile
last_y <- last_point$Percent_Change

p1 <- percentis_df %>%
     filter(Percent_Change != Inf) %>%
     ggplot(aes(x = Percentile, y = Percent_Change)) +
     geom_point(data = percentis_df %>% filter(Percentile %in% c(5,seq(0, 100, 10),95)),
                aes(color = ifelse(Percent_Change > 0, BLUE1, RED1))) +
     geom_hline(yintercept = 0, color = "gray") +
     geom_line(aes(color = ifelse(Percent_Change > 0, BLUE1, RED1))) +
     geom_vline(xintercept = last_x, linetype = "dashed", color = "gray") +
     geom_text(aes(x = last_x, y = last_y,
                   label = paste0("percentil = ", round(last_x, 2))),
               vjust = 4, hjust = 1.2, color = "gray", size = 3.2) +
     geom_text(data = percentis_df %>% filter(Percentile %in% c(5,seq(0, 100, 10),95)),
               aes(label = round(Percent_Change, 2),
                   color = ifelse(Percent_Change > 0, BLUE1, RED1)),
               vjust = -1,
               size = 3) +
     theme_swd() +
     labs(y = "Variação(%)", x = "Percentil") +
     scale_x_continuous(n.breaks = 20, minor_breaks = c(5,seq(0, 100, 10),95)) +
     scale_y_continuous(n.breaks = 10) +
     theme(panel.grid.major.x = element_line(size = 0.3,color = "#e8e7e7", linetype = "solid")) +
     scale_color_identity()
save_plot(p1, "Variação por percentil - Renda e Renda Ajustada.png")

# Por Gênero e raça
