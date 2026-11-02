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
  formatC(ifelse(abs(values) < 1e-10, 0, values), format = "f", digits = digits)
}

# Match the final 180 mm figure, including its compact shared-key footer.
figure_width_cm <- 18
original_height_cm <- 20
legend_footer_pt <- 24
figure_height_cm <- original_height_cm + legend_footer_pt / 72 * 2.54
reference_width_px <- 8503
reference_height_px <- 9448
panel_tag_pt <- 13.7579616927
style_scale <- 0.5
frames <- rbind(
  c(462.5, 194.5, 2641, 2010), c(3296.5, 194.5, 5476, 2010),
  c(6131.5, 194.5, 8310, 2010), c(462.5, 2556.5, 2641, 4372),
  c(3296.5, 2556.5, 5476, 4372), c(6131.5, 2556.5, 8310, 4372),
  c(500.5, 4918.5, 2680, 6734), c(3360.5, 4918.5, 5540, 6734),
  c(6131.5, 4918.5, 8310, 6734), c(500.5, 7280.5, 2680, 9096),
  c(3296.5, 7280.5, 5476, 9096), c(6169.5, 7280.5, 8349, 9096)
)

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
      linewidth = 0.65 * style_scale, color = blue_main
    ) +
    geom_point(color = blue_main, size = 2.2 * style_scale) +
    geom_hline(
      yintercept = 0, linetype = "dashed", color = "gray40",
      linewidth = 0.4 * style_scale
    ) +
    geom_vline(
      xintercept = -1, linetype = "dashed", color = "gray40",
      linewidth = 0.4 * style_scale
    ) +
    annotate(
      "text", x = Inf, y = Inf,
      label = format_att_label(spec$panel), family = "Arial",
      size = 4.0 * style_scale, hjust = 1.06, vjust = 1.35
    ) +
    labs(x = "Relative year", y = spec$y_title) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(
      limits = c(spec$y_min, spec$y_max), breaks = y_breaks,
      labels = function(x) format_y_labels(x, spec$y_digits),
      expand = c(0, 0)
    ) +
    theme_bw(base_family = "Arial", base_size = 6) +
    theme(
      text = element_text(family = "Arial", size = 6, color = "black"),
      axis.text = element_text(size = 5.5, color = "black"),
      axis.title = element_text(size = 5.5, color = "black"),
      axis.title.x = element_text(margin = margin(t = 3.5)),
      axis.title.y = element_text(margin = margin(r = 3.5)),
      axis.ticks = element_line(colour = "black", linewidth = 0.125),
      axis.ticks.length = unit(1.375, "pt"),
      legend.position = "none", aspect.ratio = 10 / 12,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.25),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(2.5, 4, 4.5, 4)
    )
}

plots <- lapply(seq_len(nrow(panel_specs)), function(i) draw_panel(panel_specs[i, ]))

for (i in seq_along(plots)) {
  # Keep the original individual-panel output names.
  single_panel <- plots[[i]] +
    labs(tag = panel_specs$panel[i], title = panel_specs$title[i]) +
    theme(
      plot.title = element_text(
        family = "Arial", size = 6, color = "black", hjust = 0.5,
        face = "bold", margin = margin(t = 1, b = 2.5)
      ),
      plot.tag = element_text(
        family = "Arial", size = panel_tag_pt, color = "black", face = "bold"
      ),
      plot.tag.position = c(0.01, 0.99)
    )
  ggsave(
    file.path(output_dir, sprintf("fig4_panel_%s.png", letters[i])),
    single_panel, width = 6, height = 5, units = "cm", dpi = 600
  )
}

draw_panel_px <- function(plot_object, frame) {
  tab <- ggplotGrob(plot_object)
  cell <- tab$layout[tab$layout$name == "panel", ]
  x_unit <- figure_width_cm / reference_width_px
  y_unit <- original_height_cm / reference_height_px
  tab$widths[cell$l] <- unit((frame[3] - frame[1]) * x_unit, "cm")
  tab$heights[cell$t] <- unit((frame[4] - frame[2]) * y_unit, "cm")
  tab$respect <- FALSE
  left <- convertWidth(sum(tab$widths[seq_len(cell$l - 1)]), "cm", valueOnly = TRUE)
  top <- convertHeight(sum(tab$heights[seq_len(cell$t - 1)]), "cm", valueOnly = TRUE)
  pushViewport(viewport(
    x = unit(frame[1] * x_unit - left, "cm"),
    y = unit(figure_height_cm - frame[2] * y_unit + top, "cm"),
    width = sum(tab$widths), height = sum(tab$heights),
    just = c("left", "top"), clip = "off"
  ))
  grid.draw(tab)
  popViewport()
}

draw_shared_legend <- function() {
  label <- "Coefficient estimate with 95% CI"
  text_gp <- gpar(fontfamily = "Arial", fontsize = 7, col = "black")
  text_width <- convertWidth(grobWidth(textGrob(label, gp = text_gp)), "pt", valueOnly = TRUE)
  width_pt <- figure_width_cm / 2.54 * 72
  key_width <- 18
  text_gap <- 5
  start <- (width_pt - key_width - text_gap - text_width) / 2
  center_x <- start + key_width / 2
  center_y <- 12
  # Horizontal confidence interval and centre point, without end caps.
  grid.segments(
    x0 = unit(center_x - 5.5, "pt"), x1 = unit(center_x + 5.5, "pt"),
    y0 = unit(center_y, "pt"), y1 = unit(center_y, "pt"),
    gp = gpar(col = blue_main, lwd = 0.75, lineend = "butt")
  )
  grid.circle(
    x = unit(center_x, "pt"), y = unit(center_y, "pt"), r = unit(1.25, "pt"),
    gp = gpar(col = blue_main, fill = blue_main)
  )
  grid.text(
    label, x = unit(start + key_width + text_gap, "pt"), y = unit(center_y, "pt"),
    just = c("left", "centre"), gp = text_gp
  )
}

save_grid <- function(plots, filename, width_cm = figure_width_cm,
                      height_cm = figure_height_cm, dpi = 600,
                      device = c("png", "pdf")) {
  device <- match.arg(device)
  if (device == "png") {
    png(filename, width = width_cm, height = height_cm, units = "cm",
        res = dpi, type = "cairo-png", bg = "white")
  } else {
    cairo_pdf(filename, width = width_cm / 2.54, height = height_cm / 2.54,
              family = "Arial", bg = "white")
  }
  on.exit(dev.off())

  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  for (i in seq_along(plots)) {
    draw_panel_px(plots[[i]], frames[i, ])
    # Keep panel titles and letters editable and outside the plotting frames.
    grid.text(
      panel_specs$title[i],
      x = unit(mean(frames[i, c(1, 3)]) / reference_width_px, "npc"),
      y = unit(figure_height_cm - (frames[i, 2] - 43) /
                 reference_height_px * original_height_cm, "cm"),
      just = c("centre", "bottom"),
      gp = gpar(fontfamily = "Arial", fontface = "bold", fontsize = 6, col = "black")
    )
    grid.text(
      panel_specs$panel[i],
      x = unit(((i - 1) %% 3 * 2834 + 125) / reference_width_px, "npc"),
      y = unit(figure_height_cm - ((i - 1) %/% 3 * 2362 + 220) /
                 reference_height_px * original_height_cm, "cm"),
      just = c("left", "bottom"),
      gp = gpar(fontfamily = "Arial", fontface = "bold",
                fontsize = panel_tag_pt, col = "black")
    )
  }
  draw_shared_legend()
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
