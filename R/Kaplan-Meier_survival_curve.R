# Kaplan-Meier survival curve and Cox regression analysis
# Example code used to generate Fig. 1j

library(tidyverse)
library(ggplot2)
library(survival)
library(survminer)
library(ggsci)

# Load data
df <- read.csv("Fig1j.csv")

# Kaplan-Meier analysis
fit <- survfit(
  Surv(PFS_time, PFS_status) ~ RHOA_G17V,
  data = df
)

# Plot
p <- ggsurvplot(
  fit,
  conf.int = FALSE,
  surv.median.line = "hv",
  risk.table = FALSE,
  break.x.by = 50,
  pval = FALSE,
  tables.theme = theme_cleantable(),
  palette = "lancet"
)

p$plot <- p$plot +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

print(p)

# Univariate Cox proportional-hazards model
cox_model <- coxph(
  Surv(PFS_time, PFS_status) ~ RHOA_G17V,
  data = df
)

summary(cox_model)

# Multivariate Cox proportional-hazards model
cox_model <- coxph(
  Surv(PFS_time, PFS_status) ~
    RHOA_G17V +
    LDH_status +
    sIL2R_status +
    Stage +
    Number_of_prior_therapy_status,
  data = df
)

summary(cox_model)

