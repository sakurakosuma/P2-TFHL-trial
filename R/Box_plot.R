# Box plot
# Example code used to generate Fig. 2b

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lemon)
library(cowplot)

# Load data
df <- read.csv("Fig2b.csv")

# Set factor levels
df$Cluster <- factor(df$Cluster, levels = c("T", "NK", "B", "Myeloid"))
df$Response <- factor(df$Response, levels = c("Rs", "NRs"))

# Create combined response and time variable
df$res_time <- paste(df$Response, df$Time, sep = "_")
df$res_time <- factor(
  df$res_time,
  levels = c(
    "Rs_SCR",
    "Rs_D30",
    "Rs_FIN",
    "NRs_SCR",
    "NRs_D30"
  )
)

# Colors
cols <- c("#FF0000", "#0070C0")
names(cols) <- levels(df$Response)

# Plot
p <- ggplot(df, aes(x = res_time, y = Proportion)) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.5
  ) +
  geom_jitter(
    aes(color = Response),
    size = 1.5,
    alpha = 0.6
  ) +
  theme_cowplot() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "transparent"),
    strip.text = element_text(color = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(color = "transparent"),
    legend.position = "none"
  ) +
  facet_rep_wrap(
    ~Cluster,
    repeat.tick.labels = TRUE,
    scales = "free",
    nrow = 1
  ) +
  scale_color_manual(values = cols) +
  scale_y_continuous(
    expand = expansion(mult = c(0.08, 0.4)),
    limits = c(-0.001, NA)
  )

print(p)

