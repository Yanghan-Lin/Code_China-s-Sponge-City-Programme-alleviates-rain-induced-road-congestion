################################################################################
# Project: China's Sponge City Programme and rain-induced road congestion
# File:    07_extended_data_figure2_gelbach_plot.R
# Purpose: Draw Extended Data Fig. 2 from Stata-exported Gelbach contributions
# Author:  Yanghan Lin et al.
################################################################################

# Run submission_code/Stata/07_extended_data_figure2_gelbach_estimates.do first.

project_root <- "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
submission_dir <- file.path(project_root, "submission_code")
output_dir <- file.path(submission_dir, "outputs")

required_packages <- c("ggplot2", "dplyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following R packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(ggplot2)
library(dplyr)

################################################################################
# 1. Read the decomposition and model summary
################################################################################

gelbach_df <- read.csv(
  file.path(output_dir, "extended_data_fig2_gelbach_components.csv"),
  stringsAsFactors = FALSE
) %>%
  arrange(mechanism_order) %>%
  mutate(
    share_pct = as.numeric(share_pct),
    mechanism_label = factor(mechanism_label, levels = rev(mechanism_label)),
    share_label = sprintf("%.1f%%", share_pct),
    label_hjust = ifelse(share_pct >= 0, 0, 1)
  )

summary_df <- read.csv(
  file.path(output_dir, "extended_data_fig2_gelbach_summary.csv"),
  stringsAsFactors = FALSE
)

stopifnot(
  nrow(gelbach_df) == 6,
  nrow(summary_df) == 1,
  all(is.finite(gelbach_df$share_pct)),
  abs(sum(gelbach_df$share_pct) - 100) < 0.001
)

summary_label <- sprintf(
  "Baseline ATT = %.4f\nControlled ATT = %.4f\nExplained share = %.1f%%",
  summary_df$baseline_att,
  summary_df$controlled_att,
  summary_df$explained_pct
)

################################################################################
# 2. Draw the horizontal contribution bars
################################################################################

blue_main <- "#1F77B4"
x_upper <- max(68, max(gelbach_df$share_pct) * 1.85)
x_lower <- if (min(gelbach_df$share_pct) < 0) min(gelbach_df$share_pct) - 8 else 0
label_gap <- (x_upper - x_lower) * 0.012

gelbach_df <- gelbach_df %>%
  mutate(label_x = share_pct + ifelse(share_pct >= 0, label_gap, -label_gap))

extended_data_fig2 <- ggplot(gelbach_df, aes(x = share_pct, y = mechanism_label)) +
  geom_col(width = 0.62, fill = blue_main, colour = "black", linewidth = 0.35) +
  geom_text(
    aes(x = label_x, label = share_label, hjust = label_hjust),
    family = "Helvetica", size = 4.2, colour = "black"
  ) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.5) +
  annotate(
    "label", x = x_upper * 0.64, y = 3.35,
    label = summary_label, hjust = 0, vjust = 0.5,
    family = "Helvetica", size = 4, lineheight = 1.25,
    fill = "white", colour = "black", linewidth = 0.35,
    label.padding = grid::unit(0.18, "lines"),
    label.r = grid::unit(0.12, "lines")
  ) +
  scale_x_continuous(
    limits = c(x_lower, x_upper),
    breaks = scales::breaks_width(10),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(expand = expansion(add = 0.6)) +
  labs(x = "Contribution to coefficient attenuation (%)", y = NULL) +
  theme_classic(base_size = 12, base_family = "Helvetica") +
  theme(
    text = element_text(family = "Helvetica", colour = "black"),
    axis.text = element_text(size = 12, colour = "black"),
    axis.title.x = element_text(size = 12, margin = margin(t = 8)),
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    plot.margin = margin(10, 12, 10, 10),
    legend.position = "none"
  )

################################################################################
# 3. Export the figure
################################################################################

ggsave(
  file.path(output_dir, "extended_data_fig2_gelbach.png"),
  extended_data_fig2,
  width = 32, height = 19, units = "cm", dpi = 600
)

ggsave(
  file.path(output_dir, "extended_data_fig2_gelbach.pdf"),
  extended_data_fig2,
  width = 32, height = 19, units = "cm", device = cairo_pdf
)
