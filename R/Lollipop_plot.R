# Lollipop plot

library(tidyverse)
library(ggplot2)
library(ggh4x)

# Load data
df <- read.csv("Fig3b.csv")

# Plot
p <- ggplot(
  df,
  aes(
    x = NES,
    y = fct_reorder(NAME, NES)
  )
) +
  geom_segment(
    aes(
      x = 0,
      xend = NES,
      y = fct_reorder(NAME, NES),
      yend = NAME
    )
  ) +
  geom_point(
    aes(
      color = FDR_qval,
      size = SIZE
    )
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dotted"
  ) +
  scale_color_distiller(
    palette = "PuRd",
    direction = -1
  ) +
  scale_size_area(max_size = 5) +
  theme_bw(base_family = "Arial") +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    panel.grid = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position = "none"
  ) +
  xlim(c(-2.2, 4.8)) +
  force_panelsizes(
    rows = unit(20, "line"),
    cols = unit(6, "line")
  )

print(p)

