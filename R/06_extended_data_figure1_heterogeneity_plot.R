################################################################################
# Project: China's Sponge City Programme and rain-induced road congestion
# File:    06_extended_data_figure1_heterogeneity_plot.R
# Purpose: Draw Extended Data Fig. 1 from Stata-exported heterogeneity estimates
# Author:  Yanghan Lin et al.
################################################################################

# Run submission_code/Stata/06_extended_data_figure1_heterogeneity_estimates.do
# before this script.

project_root <- "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
submission_dir <- file.path(project_root, "submission_code")
output_dir <- file.path(submission_dir, "outputs")

required_packages <- c("ggplot2", "dplyr", "patchwork", "grid")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following R packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)

blue_main <- "#0072B2"

heterogeneity_df <- read.csv(
  file.path(output_dir, "extended_data_fig1_heterogeneity.csv"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    panel = as.character(panel),
    group = as.integer(group),
    beta = as.numeric(beta),
    se = as.numeric(se),
    ci_lo = as.numeric(ci_lo),
    ci_hi = as.numeric(ci_hi)
  )

panel_specs <- list(
  a = list(
    title = "Nighttime light",
    order = c("High", "Mid", "Low")
  ),
  b = list(
    title = "Population",
    order = c("High", "Mid", "Low")
  ),
  c = list(
    title = "Elevation",
    order = c(">300m", "100-300m", "0-100m")
  ),
  d = list(
    title = "Slope",
    order = c(">10 deg", "5-10 deg", "0-5 deg")
  )
)

theme_extended_fig1 <- function() {
  theme_bw(base_size = 12, base_family = "Helvetica") +
    theme(
      text = element_text(family = "Helvetica", size = 12, color = "black"),
      axis.text = element_text(family = "Helvetica", size = 12, color = "black"),
      axis.title.x = element_text(
        family = "Helvetica", size = 12, color = "black",
        margin = margin(t = 9)
      ),
      axis.title.y = element_blank(),
      plot.title = element_text(
        family = "Helvetica", size = 13, color = "black",
        hjust = 0.5, face = "bold",
        margin = margin(t = 2, b = 5)
      ),
      plot.tag = element_text(
        family = "Helvetica", size = 18, color = "black", face = "bold"
      ),
      plot.tag.position = c(0.01, 0.99),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(5, 10, 10, 10),
      legend.position = "none"
    )
}

draw_heterogeneity_panel <- function(panel_id, show_x_title = FALSE) {
  spec <- panel_specs[[panel_id]]
  df <- heterogeneity_df %>%
    filter(panel == panel_id) %>%
    mutate(group_label = factor(group_label, levels = spec$order))

  ggplot(df, aes(x = beta, y = group_label)) +
    geom_segment(
      aes(x = ci_lo, xend = ci_hi, y = group_label, yend = group_label),
      color = blue_main,
      linewidth = 0.85
    ) +
    geom_point(color = blue_main, size = 3.0) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.45
    ) +
    scale_x_continuous(
      limits = c(-0.10, 0.06),
      breaks = c(-0.10, -0.05, 0, 0.05),
      labels = c("-0.10", "-0.05", "0", "0.05")
    ) +
    labs(
      tag = panel_id,
      title = spec$title,
      x = if (show_x_title) "SCP effects on TCDI" else NULL,
      y = NULL
    ) +
    theme_extended_fig1()
}

panel_a <- draw_heterogeneity_panel("a")
panel_b <- draw_heterogeneity_panel("b")
panel_c <- draw_heterogeneity_panel("c", show_x_title = TRUE)
panel_d <- draw_heterogeneity_panel("d", show_x_title = TRUE)

panel_list <- list(a = panel_a, b = panel_b, c = panel_c, d = panel_d)

for (panel_name in names(panel_list)) {
  ggsave(
    file.path(output_dir, paste0("extended_data_fig1_panel_", panel_name, ".png")),
    panel_list[[panel_name]],
    width = 12,
    height = 10,
    units = "cm",
    dpi = 600
  )
}

extended_data_fig1 <- (panel_a | panel_b) / (panel_c | panel_d)

ggsave(
  file.path(output_dir, "extended_data_fig1_heterogeneity.png"),
  extended_data_fig1,
  width = 24,
  height = 20,
  units = "cm",
  dpi = 600
)

ggsave(
  file.path(output_dir, "extended_data_fig1_heterogeneity.pdf"),
  extended_data_fig1,
  width = 24,
  height = 20,
  units = "cm"
)
