# Criando tema para o gráfico ----
# Cores dos gráficos
GRAY1 <- "#231F20"
GRAY2 <- "#414040"
GRAY3 <- "#555655"
GRAY4 <- "#646369"
GRAY5 <- "#76787B"
GRAY6 <- "#828282"
GRAY7 <- "#929497"
GRAY8 <- "#A6A6A5"
GRAY9 <- "#BFBEBE"
BLUE1 <- "#174A7E"
BLUE2 <- "#4A81BF"
BLUE3 <- "#94B2D7"
BLUE4 <- "#94AFC5"
BLUE5 <- "#22435e"
BLUE6 <- "#95B3D7"
RED1 <- "#C3514E"
RED2 <- "#E6BAB7"
RED3 <- "#800000"
GREEN1 <- "#0C8040"
GREEN2 <- "#9ABB59"
GREEN3 <- "#31859C"
GREEN4 <- "#4BACC6"
GREEN5 <- "#93CDDD"
ORANGE1 <- "#F79747"
ORANGE2 <- "#FAC090"

# Tema dos gráficos

theme_swd <- function() {
theme_minimal(base_size = 11) +
  theme(
   legend.position = "top",  # legend removed in favor of direct labeling
   legend.justification='left',
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(),
   axis.line = element_line(size = .4, color = GRAY7),
   axis.text = element_text(color = GRAY7),
   axis.ticks.x = element_line(size = 0.5, color = GRAY7),
   axis.ticks.y = element_line(size = 0.5, color = GRAY7),
   axis.title = element_text(color = GRAY3),
   axis.title.y = element_text(hjust = 1, margin = margin(0, 6, 0, 15, "pt")),
   axis.title.x = element_text(hjust = 0, margin = margin(6, 0, 15, 0, "pt")),
   plot.subtitle = element_text(color = GRAY4, size= 8, margin = margin(0,0,5,0,"pt")),
   plot.title = element_text(color = GRAY4, size= 12, margin = margin(0,0,2,0,"pt")),
   plot.title.position = "plot", # This aligns the plot title to the very left edge
   plot.caption = element_text(hjust = 0, color = GRAY6),
   plot.caption.position = "plot",
   plot.margin = margin(.3,.3,.2,.2,"cm"),
   strip.text = element_text(color = GRAY7),
   #text = element_text(family = "Arial")
   )
}


save_plot <- function(plot,title ,Width = 1700,Height = 1500,Pointsize = 12, Res = 300, Units = "px"){

  png(file= paste0("./saidas/", title),
      width = Width,
      height = Height,
      units = Units,
      res = Res)
  print(plot)
  dev.off()
}

options(vsc.dev.args = list(width=1500, height=1500, pointsize=12, res=300))
