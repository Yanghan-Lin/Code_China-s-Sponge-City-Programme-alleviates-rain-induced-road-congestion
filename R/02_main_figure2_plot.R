################################################################################
# Project: China's Sponge City Programme and rain-induced road congestion
# File:    02_main_figure2_plot.R
# Purpose: Draw Fig. 2a-d from Stata-exported coefficient files
# Author:  Yanghan Lin et al.
################################################################################

# Run submission_code/Stata/02_main_figure2_estimates.do before this script.

project_root <- "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
submission_dir <- file.path(project_root, "submission_code")
output_dir <- file.path(submission_dir, "outputs")

required_packages <- c("ggplot2", "dplyr", "lubridate", "patchwork", "scales")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following R packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(scales)

blue_main <- "#0072B2"
rainy_col <- "#F28E8C"

read_output <- function(file_name) {
  read.csv(file.path(output_dir, file_name), stringsAsFactors = FALSE)
}

att_summary <- read_output("fig2_att_summary.csv")

format_att_label <- function(panel_name) {
  row <- att_summary[att_summary$panel == panel_name, , drop = FALSE]
  if (nrow(row) != 1) return("")
  paste0(
    "ATT = ", sprintf("%.4f", row$beta),
    " (", sprintf("%.4f", row$se), ")", row$stars,
    "\nContribution: ", row$contribution
  )
}

theme_fig2 <- function() {
  theme_bw(base_size = 10, base_family = "Helvetica") +
    theme(
      plot.title = element_text(
        family = "Helvetica", size = 10, color = "black",
        hjust = 0.5, face = "bold", margin = margin(t = 2, b = 5)
      ),
      text = element_text(family = "Helvetica", size = 10, color = "black"),
      axis.text = element_text(family = "Helvetica", size = 10, color = "black"),
      axis.text.x = element_text(size = 8),
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
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key = element_blank(),
      legend.text = element_text(size = 10, family = "Helvetica", color = "black"),
      legend.title = element_blank()
    )
}

format_x_labels <- function(breaks, first_label = NULL, last_label = NULL) {
  labels <- as.character(breaks)
  if (!is.null(first_label)) labels[1] <- first_label
  if (!is.null(last_label)) labels[length(labels)] <- last_label
  labels
}

add_rainy_season <- function(df, base_date) {
  months_df <- data.frame(
    k = min(df$k, na.rm = TRUE):max(df$k, na.rm = TRUE)
  ) |>
    mutate(
      date = as.Date(base_date) %m+% months(k),
      month_number = month(date),
      is_rainy = k >= 0 & month_number %in% 4:10
    )

  df |>
    left_join(months_df[, c("k", "is_rainy")], by = "k") |>
    mutate(
      is_rainy = ifelse(is.na(is_rainy), FALSE, is_rainy),
      season = factor(
        ifelse(is_rainy, "Rainy season (post)", "Non-rainy"),
        levels = c("Non-rainy", "Rainy season (post)")
      )
    )
}

plot_event_panel <- function(
  df, title, x_breaks, x_labels, y_limits, y_breaks,
  att_label = "", show_rainy = FALSE
) {
  if (!show_rainy) {
    p <- ggplot(df, aes(x = k, y = beta)) +
      geom_linerange(aes(ymin = ci_lo, ymax = ci_hi), linewidth = 0.25, color = blue_main) +
      geom_point(size = 0.8, color = blue_main)
  } else {
    p <- ggplot(df, aes(x = k, y = beta, color = season)) +
      geom_linerange(aes(ymin = ci_lo, ymax = ci_hi), linewidth = 0.25) +
      geom_point(size = 0.8) +
      scale_color_manual(
        values = c("Non-rainy" = blue_main, "Rainy season (post)" = rainy_col),
        breaks = "Rainy season (post)",
        labels = "Rainy season (post)"
      )
  }

  p +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -1, linetype = "dashed", color = "gray40", linewidth = 0.4) +
    annotate(
      "text", x = Inf, y = Inf, label = att_label,
      hjust = 1.08, vjust = 1.18, size = 10 / .pt,
      family = "Helvetica", color = "black"
    ) +
    labs(title = title, x = "Relative year-month", y = "SCP effects on TCDI") +
    scale_x_continuous(
      limits = range(x_breaks),
      breaks = x_breaks,
      labels = x_labels,
      expand = c(0.02, 0)
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      labels = number_format(accuracy = 0.01, trim = TRUE)
    ) +
    theme_fig2() +
    theme(legend.position = "none")
}

panel_a <- read_output("fig2_panel_a_event_study.csv") |>
  filter(k >= -36, k <= 36)

panel_b <- read_output("fig2_panel_b_event_study.csv") |>
  filter(k >= -15, k <= 57) |>
  add_rainy_season("2015-04-01")

panel_c <- read_output("fig2_panel_c_event_study.csv") |>
  filter(k >= -36, k <= 36) |>
  add_rainy_season("2015-06-01")

panel_d <- read_output("fig2_panel_d_month_effects.csv") |>
  mutate(month = as.integer(month)) |>
  arrange(month)

p_a <- plot_event_panel(
  panel_a,
  title = "All SCP pilots",
  x_breaks = seq(-36, 36, by = 6),
  x_labels = format_x_labels(seq(-36, 36, by = 6), "-36+", "36+"),
  y_limits = c(-0.15, 0.10),
  y_breaks = seq(-0.15, 0.10, by = 0.05),
  att_label = format_att_label("a_full_sample")
)

p_b <- plot_event_panel(
  panel_b,
  title = "SCP pilots: waves 1\u20132",
  x_breaks = seq(-15, 57, by = 6),
  x_labels = format_x_labels(seq(-15, 57, by = 6), "-15", "57+"),
  y_limits = c(-0.15, 0.10),
  y_breaks = seq(-0.15, 0.10, by = 0.05),
  att_label = format_att_label("b_waves_1_2"),
  show_rainy = TRUE
)

p_c <- plot_event_panel(
  panel_c,
  title = "SCP pilots: waves 3\u20135",
  x_breaks = seq(-36, 36, by = 6),
  x_labels = format_x_labels(seq(-36, 36, by = 6), "-36+", "36+"),
  y_limits = c(-0.15, 0.10),
  y_breaks = seq(-0.15, 0.10, by = 0.05),
  att_label = format_att_label("c_waves_3_5"),
  show_rainy = TRUE
)

p_d <- ggplot(panel_d, aes(x = month, y = beta)) +
  geom_linerange(aes(ymin = ci_lo, ymax = ci_hi), linewidth = 0.5, color = blue_main) +
  geom_point(size = 1.9, color = blue_main) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  labs(x = "Month", y = "SCP effects on TCDI") +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Jan.", "Feb.", "Mar.", "Apr.", "May.", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    limits = c(-0.20, 0.20),
    breaks = seq(-0.20, 0.20, by = 0.10),
    labels = number_format(accuracy = 0.1, trim = TRUE)
  ) +
  theme_fig2() +
  theme(
    plot.title = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.3)
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

fig2_key <- wrap_elements(
  full = make_interval_legend(
    c("Coefficient estimate with 95% CI", "Rainy season after SCP implementation"),
    c(blue_main, rainy_col)
  ),
  ignore_tag = TRUE
)

fig2 <- ((p_a | p_b) / (p_c | p_d) / fig2_key) +
  plot_layout(heights = c(1, 1, 0.10)) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(
      family = "Helvetica", face = "bold",
      size = 18, color = "black"
    )
  )

ggsave(file.path(output_dir, "fig2_panel_a.png"), p_a, width = 12, height = 10, units = "cm", dpi = 600)
ggsave(file.path(output_dir, "fig2_panel_b.png"), p_b, width = 12, height = 10, units = "cm", dpi = 600)
ggsave(file.path(output_dir, "fig2_panel_c.png"), p_c, width = 12, height = 10, units = "cm", dpi = 600)
ggsave(file.path(output_dir, "fig2_panel_d.png"), p_d, width = 12, height = 10, units = "cm", dpi = 600)
ggsave(file.path(output_dir, "fig2_main.png"), fig2, width = 22, height = 18, units = "cm", dpi = 600)
ggsave(file.path(output_dir, "fig2_main.pdf"), fig2, width = 22, height = 18, units = "cm")
