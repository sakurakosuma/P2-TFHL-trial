# Plot of VAF ratio

library(tidyverse)
library(ggplot2)

# Load data
df <- read.csv("Fig3d.csv")

# Plot
p <- ggplot(
  df,
  aes(
    x = Treatment_duration,
    y = VAF_ratio,
    colour = Response
  )
) +
  geom_abline(
    intercept = 1,
    slope = 0,
    linewidth = 0.1
  ) +
  geom_point(
    aes(shape = RHOA_ddPCR),
    size = 6
  ) +
  geom_smooth(
    method = "lm",
    linetype = "dashed",
    se = FALSE,
    colour = "orange"
  ) +
  scale_color_manual(
    values = c(
      NRs = "#4472C4",
      Rs = "#FF0000"
    )
  ) +
  scale_shape_manual(
    values = c(
      No = 16,
      Yes = 17,
      others = 15
    )
  ) +
  theme_classic() +
  ylim(0, 1.6) +
  theme(
    text = element_blank(),
    legend.position = "none"
  )

print(p)

# Pearson's correlation coefficient test
cor.test(
  df$VAF_ratio,
  df$Treatment_duration
)

