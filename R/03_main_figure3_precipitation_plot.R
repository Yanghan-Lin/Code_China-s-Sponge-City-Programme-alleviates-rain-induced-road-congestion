################################################################################
# Project: China's Sponge City Programme and rain-induced road congestion
# File:    03_main_figure3_precipitation_plot.R
# Purpose: Draw Fig. 3a-d from Stata-exported precipitation margins
# Author:  Yanghan Lin et al.
################################################################################

# Run submission_code/Stata/03_main_figure3_precipitation_estimates.do first.

project_root <- "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
submission_dir <- file.path(project_root, "submission_code")
output_dir <- file.path(submission_dir, "outputs")

required_packages <- c("ggplot2", "dplyr", "patchwork", "scales")
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
library(scales)

blue_main <- "#377EB8"
red_main <- "#F28E8C"

fig3_data <- read.csv(
  file.path(output_dir, "fig3_precipitation_margins.csv"),
  stringsAsFactors = FALSE
)

fig3_data <- fig3_data |>
  mutate(
    policy_label = factor(policy_label, levels = c("Without SCP", "With SCP")),
    panel = factor(panel, levels = c("a_full", "b_semi", "c_humid", "d_hyper")),
    panel_title = factor(
      panel_title,
      levels = c("Full sample", "Semi-humid regions", "Humid regions", "Hyper-humid regions")
    )
  )

theme_fig3 <- function() {
  theme_bw(base_size = 10, base_family = "Helvetica") +
    theme(
      plot.title = element_text(
        family = "Helvetica", size = 10, face = "bold",
        hjust = 0.5, color = "black", margin = margin(t = 2, b = 5)
      ),
      text = element_text(family = "Helvetica", size = 10, color = "black"),
      axis.text = element_text(family = "Helvetica", size = 10, color = "black"),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1),
      axis.title.x = element_text(
        family = "Helvetica", size = 10, color = "black",
        margin = margin(t = 9)
      ),
      axis.title.y = element_text(
        family = "Helvetica", size = 10, color = "black",
        margin = margin(r = 9)
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(5, 10, 10, 10),
      aspect.ratio = 4.5 / 6,
      legend.position = "none",
      legend.justification = c(0, 1),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(family = "Helvetica", size = 10, color = "black"),
      axis.ticks = element_line(color = "black", linewidth = 0.4)
    )
}

plot_precip_panel <- function(panel_id, title_text, show_legend = FALSE) {
  df <- fig3_data |>
    filter(panel == panel_id) |>
    arrange(precp_bin, policy)

  pd <- position_dodge(width = 2.5)

  p <- ggplot(df, aes(x = prec_mid, y = margin, color = policy_label, group = policy_label)) +
    geom_line(position = pd, linewidth = 0.3) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0, linewidth = 0.6, position = pd) +
    geom_point(size = 1.5, position = pd) +
    scale_color_manual(
      values = c("Without SCP" = red_main, "With SCP" = blue_main),
      labels = c("Without SCP", "With SCP")
    ) +
    scale_x_continuous(
      breaks = sort(unique(fig3_data$prec_mid)),
      labels = fig3_data |>
        distinct(precp_bin, prec_mid, precipitation_bin) |>
        arrange(precp_bin) |>
        pull(precipitation_bin),
      expand = c(0.05, 0)
    ) +
    scale_y_continuous(
      limits = c(1.5, 2.1),
      breaks = seq(1.5, 2.1, by = 0.2),
      labels = number_format(accuracy = 0.1),
      oob = scales::oob_keep
    ) +
    labs(
      title = title_text,
      x = "Daily precipitation (mm)",
      y = "Predicted TCDI"
    ) +
    theme_fig3()

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }

  p
}

p_a <- plot_precip_panel("a_full", "All samples")
p_b <- plot_precip_panel(
  "b_semi", expression("Semi-humid regions (400\u2013800 mm " * yr^{-1} * ")")
)
p_c <- plot_precip_panel(
  "c_humid", expression("Humid regions (800\u20131500 mm " * yr^{-1} * ")")
)
p_d <- plot_precip_panel(
  "d_hyper", expression("Hyper-humid regions (>1500 mm " * yr^{-1} * ")")
)

# Use horizontal, capless point-and-interval keys in the shared footer.
make_interval_legend <- function(labels, colours, key_width = 14,
                                 text_gap = 3, item_gap = 20) {
  text_gp <- grid::gpar(fontfamily = "Helvetica", fontsize = 9, col = "black")
  label_grobs <- lapply(labels, grid::textGrob, gp = text_gp)
  item_widths <- lapply(label_grobs, function(label) {
    grid::unit(key_width + text_gap, "pt") + grid::grobWidth(label)
  })
  total_width <- Reduce(`+`, item_widths) +
    grid::unit(item_gap * (length(labels) - 1L), "pt")
  start <- (grid::unit(1, "npc") - total_width) / 2
  grobs <- list()
  for (i in seq_along(labels)) {
    centre_x <- start + grid::unit(key_width / 2, "pt")
    grobs <- c(grobs, list(
      grid::segmentsGrob(
        x0 = centre_x - grid::unit(5.5, "pt"),
        x1 = centre_x + grid::unit(5.5, "pt"), y0 = 0.55, y1 = 0.55,
        gp = grid::gpar(col = colours[i], lwd = 0.75, lineend = "butt")
      ),
      grid::circleGrob(
        x = centre_x, y = 0.55, r = grid::unit(1.25, "pt"),
        gp = grid::gpar(col = colours[i], fill = colours[i])
      ),
      grid::textGrob(
        labels[i], x = start + grid::unit(key_width + text_gap, "pt"),
        y = 0.55, just = c("left", "centre"), gp = text_gp
      )
    ))
    start <- start + item_widths[[i]] + grid::unit(item_gap, "pt")
  }
  do.call(grid::grobTree, grobs)
}

fig3_key <- wrap_elements(
  full = make_interval_legend(
    c("Without SCP", "With SCP"), c(red_main, blue_main),
    key_width = 18, text_gap = 5, item_gap = 26
  ),
  ignore_tag = TRUE
)

fig3 <- ((p_a | p_b) / (p_c | p_d) / fig3_key) +
  plot_layout(heights = c(1, 1, 0.10)) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(
      family = "Helvetica", face = "bold",
      size = 18, color = "black"
    )
  )

ggsave(file.path(output_dir, "fig3_panel_a_precipitation.png"), p_a, width = 12, height = 10, units = "cm", dpi = 600)
ggsave(file.path(output_dir, "fig3_panel_b_precipitation.png"), p_b, width = 12, height = 10, units = "cm", dpi = 600)
ggsave(file.path(output_dir, "fig3_panel_c_precipitation.png"), p_c, width = 12, height = 10, units = "cm", dpi = 600)
ggsave(file.path(output_dir, "fig3_panel_d_precipitation.png"), p_d, width = 12, height = 10, units = "cm", dpi = 600)
ggsave(file.path(output_dir, "fig3_precipitation_main.png"), fig3, width = 22, height = 18, units = "cm", dpi = 600)
ggsave(file.path(output_dir, "fig3_precipitation_main.pdf"), fig3, width = 22, height = 18, units = "cm")
