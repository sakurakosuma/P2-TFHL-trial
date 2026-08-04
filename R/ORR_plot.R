# ORR plot
# Example code used to generate Fig. 1f

library(tidyverse)
library(ggplot2)
library(scales)

# Load data
df <- read.csv("Fig1f.csv")

# Summarize data
df <- df %>%
  group_by(RHOA_G17V, ORR_CT) %>%
  summarise(n = n(), .groups = "drop")

# Set factor levels
df$RHOA_G17V <- factor(
  df$RHOA_G17V,
  levels = c("positive", "negative")
)

df$ORR_CT <- factor(
  df$ORR_CT,
  levels = c("CR", "PR", "SD", "PD", "NE")
)

# Colors
cols <- c(
  "#E64B35FF",
  "#F39B7FFF",
  "#00A087FF",
  "#3C5488FF",
  "#BFBFBF"
)

names(cols) <- levels(df$ORR_CT)

# Plot
p <- ggplot(df, aes(x = RHOA_G17V, y = n, fill = ORR_CT)) +
  geom_bar(
    stat = "identity",
    position = "fill",
    width = 0.2
  ) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

print(p)

