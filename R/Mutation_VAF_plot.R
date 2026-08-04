# Mutation plot

library(tidyverse)
library(ggplot2)

### Fig. 1e

# Load data
df <- read.csv("Fig1e.csv")

# Colors
cols <- c("#000000", "#299e35", "#3068e3", "#e33030")
names(cols) <- c("others", "CH-R", "TCR-R", "TFHL-R")

cols2 <- c("transparent", "#299e35", "#3068e3", "#e33030")
names(cols2) <- c("others", "CH-R", "TCR-R", "TFHL-R")

# Plot
p <- ggplot(df, aes(x = Patient_ID, y = VAF, fill = Group)) +
  geom_jitter(height = 0, width = 0.3, size = 2.5, stroke = 0.6, shape = 21) +
  stat_summary(
    fun = median,
    geom = "crossbar",
    size = 0.25,
    linetype = 1,
    aes(group = Patient_ID)
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols2) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  )

print(p)

### Fig. S4a

# Load data
df <- read.csv("FigS4a.csv")

# Colors
cols <- c("#000000", "#299e35", "#3068e3", "#e33030")
names(cols) <- c("others", "CH-R", "TCR-R", "TFHL-R")

cols2 <- c("transparent", "#299e35", "#3068e3", "#e33030")
names(cols2) <- c("others", "CH-R", "TCR-R", "TFHL-R")

# Plot
p <- ggplot(df, aes(x = Patient_ID, y = VAF, fill = Group)) +
  geom_jitter(height = 0, width = 0.3, size = 2.5, stroke = 0.6, shape = 21) +
  stat_summary(
    fun = median,
    geom = "crossbar",
    size = 0.25,
    linetype = 1,
    aes(group = Patient_ID)
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols2) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  )

print(p)

### Fig. S18

# Load data
df <- read.csv("FigS18.csv")

# Colors
cols <- c("#000000", "#299e35", "#3068e3", "#e33030", "#FF7E79")
names(cols) <- c("others", "CH-R", "TCR-R", "TFHL-R", "ddPCR_RHOA")

cols2 <- c("transparent", "#299e35", "#3068e3", "#e33030", "#FF7E79", "#7030A0")
names(cols2) <- c("others", "CH-R", "TCR-R", "TFHL-R", "ddPCR_RHOA", "Tumor")

# Plot
p <- ggplot(df, aes(x = Patient_ID, y = VAF, fill = Group)) +
  geom_jitter(
    aes(shape = Method),
    height = 0,
    width = 0.2,
    size = 2.5,
    stroke = 0.6
  ) +
  stat_summary(
    fun = median,
    geom = "crossbar",
    size = 0.25,
    linetype = 1,
    width = 0.6,
    aes(group = Patient_ID)
  ) +
  scale_shape_manual(values = c(24, 21)) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols2) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  )

print(p)

