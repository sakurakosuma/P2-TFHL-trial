# Heatmap of median logFC

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggh4x)

# Load data
df <- read.csv("Fig4f.csv")

# Set factor levels
df$Annotation <- factor(
  df$Annotation,
  levels = c(
    "CD4 Tn", "CD4 Tcm", "CD4 Tex", "CD4 Tem", "CD4 Teff",
    "CD8 Tn", "CD8 Tcm", "CD8 Tex", "IL7R+ CD8 Tem",
    "NKG7+ CD8 Tem", "CD8 Teff", "CD8 Temra",
    "T cells", "NK cells", "B cells", "Myeloid cells"
  )
)

df$Response <- factor(
  df$Response,
  levels = c("NRs", "Rs")
)

df$Cell_type <- factor(
  df$Cell_type,
  levels = c("CD4", "CD8", "bulk")
)

# Colors
cols <- brewer.pal(9, "RdBu")

# Plot
p <- ggplot(
  df,
  aes(
    x = Annotation,
    y = Response,
    fill = LogFC
  )
) +
  geom_tile() +
  facet_rep_grid(
    Response ~ Cell_type,
    scales = "free"
  ) +
  scale_fill_gradient2(
    low = cols[9],
    mid = cols[5],
    high = cols[1],
    midpoint = 0
  ) +
  theme_classic2() +
  force_panelsizes(cols = c(5, 7, 4)) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(
      color = "transparent",
      size = 15
    ),
    legend.position = "none",
    axis.line = element_blank(),
    axis.ticks.y = element_blank()
  )

print(p)

