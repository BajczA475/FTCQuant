library("extrafont", lib.loc="~/R/win-library/3.2")

Alex.theme = theme(
  axis.text = element_text(size = 24, color = "black"),
  axis.title = element_text(size = 30, color = "black"),
  plot.title = element_text(size = 38, margin = margin(0, 0, 20, 0)),
  panel.grid.major.x = element_blank(),
  plot.margin = unit(c(0.1, 0.25, 0.5, 0.85), "cm"),
  axis.title.y = element_text(margin = margin(0, 15, 0, 0)),
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  legend.title = element_text(size = 28),
  legend.text = element_text(size = 20),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  text = element_text(family = "Arial"),
  axis.line.x = element_line(color = "black", size = 1),
  axis.line.y = element_line(color = "black", size = 1)
)
