################################################################################
# Project: China's Sponge City Programme and rain-induced road congestion
# File:    05_main_figure5_projection_plot.R
# Purpose: Draw the Fig. 5 projection time-series panels
# Author:  Yanghan Lin et al.
################################################################################

# Run submission_code/Stata/05_main_figure5_projection_processing.do before this
# script. The three map panels in Fig. 5 are prepared separately in ArcGIS Pro.

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

projection_df <- read.csv(
  file.path(output_dir, "fig5_projection_timeseries.csv"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    year = as.numeric(year),
    threshold = as.numeric(threshold),
    mean_days = as.numeric(mean_days),
    ci_lower = as.numeric(ci_lower),
    ci_upper = as.numeric(ci_upper),
    scenario_label = dplyr::case_when(
      scenario == "ssp245" ~ "SSP2-4.5",
      scenario == "ssp585" ~ "SSP5-8.5",
      TRUE ~ "Historical"
    )
  )

scenario_colors <- c(
  "SSP2-4.5" = "#1F77B4",
  "SSP5-8.5" = "#E6554A"
)

theme_fig5 <- function() {
  theme_bw(base_size = 10, base_family = "Helvetica") +
    theme(
      text = element_text(family = "Helvetica", size = 10, color = "black"),
      axis.text = element_text(family = "Helvetica", size = 10, color = "black"),
      axis.title.x = element_text(
        family = "Helvetica", size = 10, color = "black",
        margin = margin(t = 9)
      ),
      axis.title.y = element_text(
        family = "Helvetica", size = 10, color = "black",
        margin = margin(r = 9)
      ),
      plot.title = element_text(
        family = "Helvetica", size = 10, color = "black",
        hjust = 0.5, face = "bold",
        margin = margin(t = 2, b = 5)
      ),
      plot.tag = element_text(
        family = "Helvetica", size = 18, color = "black", face = "bold"
      ),
      plot.tag.position = c(0.01, 0.99),
      legend.position = c(0.98, 0.02),
      legend.justification = c(1, 0),
      legend.text = element_text(family = "Helvetica", size = 10, color = "black"),
      legend.title = element_text(family = "Helvetica", size = 10, color = "black"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(0.4, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(5, 10, 10, 10)
    )
}

plot_projection_panel <- function(city_group_value, zone_value, threshold_value,
                                  panel_tag, title_text, y_max, y_break_by) {
  panel_df <- projection_df %>%
    filter(
      city_group == city_group_value,
      climate_zone == zone_value,
      threshold == threshold_value
    )

  historical_df <- panel_df %>% filter(scenario == "historical")
  future_df <- panel_df %>% filter(scenario != "historical")

  ggplot(panel_df, aes(x = year)) +
    geom_ribbon(
      data = future_df,
      aes(ymin = ci_lower, ymax = ci_upper, fill = scenario_label),
      alpha = 0.15
    ) +
    geom_line(
      data = historical_df,
      aes(y = mean_days),
      color = "black",
      linewidth = 0.8
    ) +
    geom_line(
      data = future_df,
      aes(y = mean_days, color = scenario_label),
      linewidth = 0.8,
      alpha = 0.8
    ) +
    geom_vline(
      xintercept = c(2024.5, 2060.5),
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.4,
      alpha = 0.7
    ) +
    annotate(
      "text", x = 2019, y = y_max * 0.96,
      label = "Historical", size = 9 / .pt,
      hjust = 0.5, fontface = "bold", family = "Helvetica"
    ) +
    annotate(
      "text", x = 2042.5, y = y_max * 0.96,
      label = "Near-Future", size = 9 / .pt,
      hjust = 0.5, fontface = "bold", family = "Helvetica"
    ) +
    annotate(
      "text", x = 2080.5, y = y_max * 0.96,
      label = "Far-Future", size = 9 / .pt,
      hjust = 0.5, fontface = "bold", family = "Helvetica"
    ) +
    scale_color_manual(name = "Scenario", values = scenario_colors) +
    scale_fill_manual(name = "Scenario", values = scenario_colors) +
    scale_x_continuous(
      breaks = seq(2015, 2100, 10),
      expand = c(0.02, 0)
    ) +
    scale_y_continuous(
      limits = c(0, y_max),
      breaks = seq(0, y_max, y_break_by),
      expand = c(0.02, 0)
    ) +
    coord_cartesian(ylim = c(0, y_max), clip = "off") +
    labs(
      tag = panel_tag,
      title = title_text,
      x = "Year",
      y = paste0("Days with precipitation >", threshold_value, "mm")
    ) +
    theme_fig5()
}

panel_a <- plot_projection_panel(
  "sponge_cities", "semi_humid", 20,
  "a", "SCP pilots: semi-humid", 12, 3
)

panel_b <- plot_projection_panel(
  "others", "semi_humid", 20,
  "b", "Non-pilots: semi-humid", 12, 3
)

panel_d <- plot_projection_panel(
  "sponge_cities", "humid", 50,
  "d", "SCP pilots: humid", 5, 1
)

panel_e <- plot_projection_panel(
  "others", "humid", 50,
  "e", "Non-pilots: humid", 5, 1
)

panel_g <- plot_projection_panel(
  "sponge_cities", "hyper_humid", 80,
  "g", "SCP pilots: hyper-humid", 6, 2
)

panel_h <- plot_projection_panel(
  "others", "hyper_humid", 80,
  "h", "Non-pilots: hyper-humid", 6, 2
)

panel_list <- list(
  a = panel_a, b = panel_b,
  d = panel_d, e = panel_e,
  g = panel_g, h = panel_h
)

for (panel_name in names(panel_list)) {
  ggsave(
    file.path(output_dir, paste0("fig5_panel_", panel_name, "_projection.png")),
    panel_list[[panel_name]],
    width = 14,
    height = 10,
    units = "cm",
    dpi = 600
  )
}

fig5_timeseries <- (panel_a | panel_b) / (panel_d | panel_e) / (panel_g | panel_h)

ggsave(
  file.path(output_dir, "fig5_projection_timeseries_6panel.png"),
  fig5_timeseries,
  width = 28,
  height = 30,
  units = "cm",
  dpi = 600
)

ggsave(
  file.path(output_dir, "fig5_projection_timeseries_6panel.pdf"),
  fig5_timeseries,
  width = 28,
  height = 30,
  units = "cm"
)
