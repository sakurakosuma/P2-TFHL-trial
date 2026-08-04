# Volcano plot
# Example code used to generate Fig. 2d

library(tidyverse)
library(ggplot2)

# Load data
df <- read.csv("Fig2d.csv")

# Set factor levels
df$Cluster <- factor(df$Cluster, levels = paste0("T", 0:16))
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

# Calculate median cell counts and log2 fold change
df1 <- df %>%
  filter(Time == "SCR") %>%
  group_by(Cluster, res_time) %>%
  summarise(
    median = median(Absolute_cell_count),
    .groups = "drop"
  )

df1 <- df1 %>%
  pivot_wider(
    names_from = res_time,
    values_from = median,
    id_cols = Cluster
  )

df1 <- df1 %>%
  mutate(logFC = log2(Rs_SCR) - log2(NRs_SCR))

# Load adjusted P values
df2 <- read.csv("Fig2d_pval.csv")

df2 <- df2 %>%
  filter(Group.1 == "Rs_SCR") %>%
  mutate(Cluster = paste0("T", 0:16))

df1 <- left_join(
  df1,
  df2 %>% select(Cluster, Adjusted.p.value),
  by = "Cluster"
)

# Define significance groups
df1 <- df1 %>%
  mutate(
    threshold = case_when(
      Adjusted.p.value < 0.05 & logFC > 0 ~ "Rs",
      Adjusted.p.value < 0.05 & logFC < 0 ~ "NRs",
      TRUE ~ "NS"
    )
  )

df1$threshold <- factor(
  df1$threshold,
  levels = c("Rs", "NRs", "NS")
)

# Colors
cols <- c(
  "Rs" = "#BC3C29CC",
  "NRs" = "#0072B5CC",
  "NS" = "#84919e"
)

# Plot
p <- ggplot(
  df1,
  aes(
    x = logFC,
    y = -log10(Adjusted.p.value),
    color = threshold,
    label = Cluster
  )
) +
  geom_point(size = 2) +
  theme_linedraw() +
  theme(
    text = element_text(size = 0),
    panel.grid = element_blank(),
    panel.border = element_rect(
      fill = NA,
      size= 1,
      colour = "black"
    ),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  scale_color_manual(values = cols) +
  geom_vline(
    xintercept = 0,
    colour = "black",
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = -log10(0.05),
    colour = "black",
    linetype = "dashed"
  ) +
  lims(x = c(-4, 4), y = c(0, NA))

print(p)

