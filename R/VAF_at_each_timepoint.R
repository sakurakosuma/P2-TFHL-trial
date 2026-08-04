# VAFs at each time point for Targeted-seq
# Example code used to generate Fig. S5a

library(tidyverse)
library(ggplot2)

# Load data
df <- read.csv("FigS5.csv")

# Set time points
df <- df %>%
  mutate(Time = factor(Time, levels = c("SCR", "D30", "D60", "D90", "FIN")))

df <- df %>%
  mutate(Time2 = ifelse(Time == "SCR", 1, Time))

# Replace zero values for log-scale visualization
df <- df %>%
  mutate(VAF2 = ifelse(VAF == 0, 0.005, VAF))

# Define positive and negative mutations
df <- df %>%
  mutate(shape = ifelse(VAF == 0, "Neg", "Pos"))

# Colors
cols <- c(
  "#e33030",
  "#ffc0cb",
  "#3068e3",
  "#4DAF4A",
  "#F5BF42",
  "#e0218a",
  "grey70"
)

names(cols) <- c(
  "TFHL-R",
  "TFHL-R2",
  "TCR-R",
  "epigenetic",
  "Transcriptional regulation",
  "Tumor suppressor",
  "Others"
)

# Plot
p <- ggplot(
  df %>% filter(Response == "Rs"),
  aes(
    x = Time2,
    y = VAF2,
    group = Gene_label,
    color = Group
  )
) +
  geom_hline(
    yintercept = 0.005,
    size = 0.2
  ) +
  geom_vline(
    xintercept = 1:5,
    linetype = "dotted"
  ) +
  geom_line(size = 0.8) +
  geom_point(
    aes(shape = shape),
    size = 2
  ) +
  scale_color_manual(values = cols) +
  scale_shape_manual(values = c(1, 19)) +
  facet_rep_wrap(
    ~Patient_ID,
    repeat.tick.labels = TRUE,
    nrow = 16
  ) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    strip.background = element_rect(
      fill = "grey95",
      color = "transparent"
    ),
    strip.text = element_text(
      colour = "transparent",
      size = 15
    )
  ) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("SCR", "D30/FIN", "D60", "D90", "FIN"),
    limits = c(1, 5)
  ) +
  guides(color = guide_legend(ncol = 3)) +
  scale_y_continuous(
    trans = "log10",
    limits = c(0.005, 0.6),
    breaks = c(0.005, 0.01, 0.05, 0.1, 0.6),
    labels = c("ND", 0.01, 0.05, 0.1, 0.6)
  )

print(p)

