# Heatmap of Metascape results
# Example code used to generate Fig. 3a

library(tidyverse)
library(ComplexHeatmap)
library(circlize)

# Load data
df <- read.csv("Fig3a.csv", row.names = 1)

# Color scale
col_fun <- colorRamp2(
  c(-20, 0, 20),
  c("#6DBCC3", "black", "deeppink3")
)

# Plot
Heatmap(
  df,
  col = col_fun,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  show_column_names = FALSE,
  show_row_names = FALSE,
  show_heatmap_legend = FALSE
)

