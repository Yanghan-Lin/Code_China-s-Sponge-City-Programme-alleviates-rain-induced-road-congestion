################################################################################
# Project: China's Sponge City Programme and rain-induced road congestion
# File:    04_main_figure4_mechanism_plot.R
# Purpose: Draw Fig. 4a-l from Stata-exported annual mechanism estimates
# Author:  Yanghan Lin et al.
################################################################################

# If rebuilding procurement variables from raw contract records, first run
# submission_code/R/00_procurement_contract_mechanism_processing.R. Then run
# submission_code/Stata/04_main_figure4_mechanism_estimates.do before this
# plotting script.

project_root <- "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
submission_dir <- file.path(project_root, "submission_code")
output_dir <- file.path(submission_dir, "outputs")

required_packages <- c("ggplot2", "dplyr", "grid")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following R packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(ggplot2)
library(dplyr)
library(grid)

blue_main <- "#0072B2"

read_output <- function(file_name) {
  read.csv(file.path(output_dir, file_name), stringsAsFactors = FALSE)
}

event_df <- read_output("fig4_mechanism_event_study.csv") %>%
  mutate(
    panel = as.character(panel),
    k = as.numeric(k),
    beta = as.numeric(beta),
    se = as.numeric(se),
    ci_lo = as.numeric(ci_lo),
    ci_hi = as.numeric(ci_hi)
  )

att_df <- read_output("fig4_mechanism_att_summary.csv") %>%
  mutate(
    panel = as.character(panel),
    beta = as.numeric(beta),
    se = as.numeric(se),
    p_value = as.numeric(p_value),
    stars = ifelse(is.na(stars), "", stars)
  )

panel_specs <- data.frame(
  panel = letters[1:12],
  title = c(
    "Source-control LID procurement",
    "Gray drainage procurement",
    "Key-node O&M procurement",
    "Smart monitoring procurement",
    "Blue-green restoration procurement",
    "Planning and design service procurement",
    "Impervious surface ratio",
    "Urban vegetation coverage",
    "Urban greening investment",
    "Drainage pipeline density",
    "Drainage pipeline investment",
    "Stormwater pipeline density"
  ),
  y_title = c(
    "SCP effects on LIDP",
    "SCP effects on GDP",
    "SCP effects on KOM",
    "SCP effects on SMP",
    "SCP effects on BGP",
    "SCP effects on PDP",
    "SCP effects on ISR",
    "SCP effects on NDVI",
    "SCP effects on UGI",
    "SCP effects on DPD",
    "SCP effects on DPI",
    "SCP effects on SPD"
  ),
  y_min = c(-6, -6, -6, -6, -6, -6, -0.9, -0.09, -3, -0.9, -3, -0.9),
  y_max = c( 6,  6,  6,  6,  6,  6,  0.9,  0.09,  3,  0.9,  3,  0.9),
  y_by = c(2, 2, 2, 2, 2, 2, 0.3, 0.03, 1, 0.3, 1, 0.3),
  y_digits = c(0, 0, 0, 0, 0, 0, 1, 2, 0, 1, 0, 1),
  stringsAsFactors = FALSE
)

format_att_label <- function(panel_id) {
  row <- att_df[att_df$panel == panel_id, , drop = FALSE]
  if (nrow(row) != 1) return("")
  paste0(
    "ATT = ", sprintf("%.4f", row$beta),
    " (", sprintf("%.4f", row$se), ")",
    row$stars
  )
}

format_y_labels <- function(values, digits) {
  formatC(values, format = "f", digits = digits)
}

draw_panel <- function(spec) {
  df <- event_df %>%
    filter(panel == spec$panel) %>%
    arrange(k)

  y_breaks <- seq(spec$y_min, spec$y_max, by = spec$y_by)
  x_breaks <- -5:5
  x_labels <- c("-5+", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5+")

  ggplot(df, aes(x = k, y = beta)) +
    geom_linerange(
      aes(ymin = ci_lo, ymax = ci_hi),
      linewidth = 0.65,
      color = blue_main
    ) +
    geom_point(color = blue_main, size = 2.2) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = -1,
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.4
    ) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = format_att_label(spec$panel),
      family = "Helvetica",
      size = 4.0,
      hjust = 1.06,
      vjust = 1.35
    ) +
    labs(
      tag = spec$panel,
      title = spec$title,
      x = "Relative year",
      y = spec$y_title
    ) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(
      limits = c(spec$y_min, spec$y_max),
      breaks = y_breaks,
      labels = function(x) format_y_labels(x, spec$y_digits),
      expand = c(0, 0)
    ) +
    theme_bw(base_family = "Helvetica", base_size = 12) +
    theme(
      text = element_text(family = "Helvetica", size = 12, color = "black"),
      axis.text = element_text(size = 11, color = "black"),
      axis.title = element_text(size = 11, color = "black"),
      axis.title.x = element_text(margin = margin(t = 7)),
      axis.title.y = element_text(margin = margin(r = 7)),
      plot.title = element_text(
        family = "Helvetica",
        size = 12,
        color = "black",
        hjust = 0.5,
        face = "bold",
        margin = margin(t = 2, b = 5)
      ),
      plot.tag = element_text(
        family = "Helvetica",
        size = 20,
        color = "black",
        face = "bold"
      ),
      plot.tag.position = c(0.01, 0.99),
      legend.position = "none",
      aspect.ratio = 10 / 12,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(5, 8, 9, 8)
    )
}

plots <- lapply(seq_len(nrow(panel_specs)), function(i) draw_panel(panel_specs[i, ]))

for (i in seq_along(plots)) {
  ggsave(
    file.path(output_dir, sprintf("fig4_panel_%s.png", letters[i])),
    plots[[i]],
    width = 12,
    height = 10,
    units = "cm",
    dpi = 600
  )
}

save_grid <- function(plots, filename, width_cm = 36, height_cm = 40,
                      dpi = 600, device = c("png", "pdf")) {
  device <- match.arg(device)
  if (device == "png") {
    png(filename, width = width_cm, height = height_cm, units = "cm", res = dpi)
  } else {
    pdf(filename, width = width_cm / 2.54, height = height_cm / 2.54,
        family = "Helvetica")
  }
  on.exit(dev.off())

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 3)))
  for (i in seq_along(plots)) {
    row_i <- ceiling(i / 3)
    col_i <- ((i - 1) %% 3) + 1
    print(
      plots[[i]],
      vp = viewport(layout.pos.row = row_i, layout.pos.col = col_i)
    )
  }
}

save_grid(
  plots,
  file.path(output_dir, "fig4_mechanism_12_panel.png"),
  device = "png"
)

save_grid(
  plots,
  file.path(output_dir, "fig4_mechanism_12_panel.pdf"),
  device = "pdf"
)
