library(tidyverse)

# Load data
brazil_income_share <- readRDS("./intermediarios/income_share_accumulated_brazil.rds")
uruguay_income_share <- readRDS("./intermediarios/income_share_accumulated_uruguay.rds")

income_share_brazil_uruguay  <- bind_rows(brazil_income_share,
                                          uruguay_income_share)

# Create a line chart

png(filename = "./saidas/curva de lorenz Brasil e Uruguai.png", width = 1200, height = 800, res = 150)
ggplot(income_share_brazil_uruguay,
       aes(x = Decile, y = IncomeShareAccumulated,
           color = Country)) +
  geom_line(size = 1) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = "Curva de Lorenz - Brasil (2023) e Uruguai (2022)",
       x = "Decil",
       y = "% da Renda compartilhada acumulada",
       color = "")+
  scale_color_manual(values = c("Brasil" = "#e15832", "Uruguay" = "#727272"))
dev.off()

# Create a bar chart
png(filename = "./saidas/Comparação renda compartilhada Brasil e Uruguai.png", width = 1200, height = 800, res = 150)
ggplot(income_share_brazil_uruguay,
  aes(x = factor(Decile, levels = 10:1), y = IncomeShare, fill = Country)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(IncomeShare, 1), color = Country),
       position = position_dodge(width = 0.9),
       vjust = 0.5,
       hjust = -0.4,
       show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top",
   legend.justification = "left") +
  scale_fill_manual(values = c("Brasil" = "#e15832", "Uruguay" = "gray")) +
  scale_color_manual(values = c("Brasil" = "#e15832", "Uruguay" = "#727272")) +
  labs(title = "% da renda compartilhada por decil",
  x = "Decil",
  y = "% da Renda compartilhada",
  fill = "") +
  ylim(0, 45)

dev.off()
