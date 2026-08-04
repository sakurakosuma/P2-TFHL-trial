# Dot plot

library(tidyverse)
library(ggplot2)

# Load data
df <- read.csv("Fig4e.csv")

# Set factor levels
df$Annotation <- factor(
  df$Annotation,
  levels = c(
    "CD4 Tn", "CD4 Tcm", "CD4 Tex", "CD4 Tem", "CD4 Teff",
    "CD8 Tn", "CD8 Tcm", "CD8 Tex", "CD8 Tem1", "CD8 Tem2", 
    "CD8 Teff", "CD8 Temra"
  )
)

df$Response <- factor(
  df$Response,
  levels = c("Rs", "NRs")
)

df$Time <- factor(
  df$Time,
  levels = c("SCR", "D30/FIN")
)

# Plot
p <- ggplot(
  df,
  aes(
    x = Time,
    y = VAF_TET2,
    group = Patient_ID
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(
    aes(color = Time),
    size = 3
  ) +
  scale_color_manual(
    values = c("#9bd38d", "#BEBADA")
  ) +
  theme_classic2() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_text(
      color = "transparent",
      size = 15
    )
  ) +
  facet_rep_grid(
    reorder(Response, dplyr::desc(Response)) ~ Annotation,
    repeat.tick.labels = TRUE,
    scales = "free_x"
  ) +
  ylim(0, NA)

print(p)

