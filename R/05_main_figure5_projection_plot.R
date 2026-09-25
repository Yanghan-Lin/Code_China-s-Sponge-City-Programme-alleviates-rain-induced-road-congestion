################################################################################
# Project: China's Sponge City Programme and rain-induced road congestion
# File:    05_main_figure5_projection_plot.R
# Purpose: Draw the complete Fig. 5 projection time-series and map panels
# Author:  Yanghan Lin et al.
################################################################################

project_root <- "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
submission_dir <- file.path(project_root, "submission_code")
output_dir <- file.path(submission_dir, "outputs")
map_input_dir <- output_dir
boundary_dir <- file.path(project_root, "boundary_shp")

city_boundary_file <- file.path(
  boundary_dir, "2024年省市县三级行政区划数据（审图号：GS（2024）0650号）", "市.shp"
)
country_boundary_file <- file.path(boundary_dir, "中国国家边界", "country_32649_1.shp")
country_line_file <- file.path(boundary_dir, "中国国家边界", "country_line_32649.shp")

required_packages <- c("ggplot2", "dplyr", "patchwork", "grid", "sf")
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
library(sf)

# Read the prepared time-series means and confidence intervals.
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
  theme_bw(base_size = 10, base_family = "Arial") +
    theme(
      text = element_text(family = "Arial", size = 10, color = "black"),
      axis.text = element_text(family = "Arial", size = 10, color = "black"),
      axis.title.x = element_text(
        family = "Arial", size = 10, color = "black",
        margin = margin(t = 9)
      ),
      axis.title.y = element_text(
        family = "Arial", size = 10, color = "black",
        margin = margin(r = 9)
      ),
      plot.title = element_text(
        family = "Arial", size = 10, color = "black",
        hjust = 0.5, face = "bold",
        margin = margin(t = 2, b = 5)
      ),
      plot.tag = element_text(
        family = "Arial", size = 18, color = "black", face = "bold"
      ),
      plot.tag.position = c(0.01, 0.99),
      legend.position = c(0.98, 0.02),
      legend.justification = c(1, 0),
      legend.text = element_text(family = "Arial", size = 10, color = "black"),
      legend.title = element_text(family = "Arial", size = 10, color = "black"),
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
      aes(y = mean_days), color = "black", linewidth = 0.8
    ) +
    geom_line(
      data = future_df,
      aes(y = mean_days, color = scenario_label),
      linewidth = 0.8, alpha = 0.8
    ) +
    geom_vline(
      xintercept = c(2024.5, 2060.5), linetype = "dashed",
      color = "gray40", linewidth = 0.4, alpha = 0.7
    ) +
    annotate(
      "text", x = 2019, y = y_max * 0.96, label = "Historical",
      size = 9 / .pt, hjust = 0.5, fontface = "bold", family = "Arial"
    ) +
    annotate(
      "text", x = 2042.5, y = y_max * 0.96, label = "Near-Future",
      size = 9 / .pt, hjust = 0.5, fontface = "bold", family = "Arial"
    ) +
    annotate(
      "text", x = 2080.5, y = y_max * 0.96, label = "Far-Future",
      size = 9 / .pt, hjust = 0.5, fontface = "bold", family = "Arial"
    ) +
    scale_color_manual(name = "Scenario", values = scenario_colors) +
    scale_fill_manual(name = "Scenario", values = scenario_colors) +
    scale_x_continuous(breaks = seq(2015, 2100, 10), expand = c(0.02, 0)) +
    scale_y_continuous(
      limits = c(0, y_max), breaks = seq(0, y_max, y_break_by),
      expand = c(0.02, 0)
    ) +
    coord_cartesian(ylim = c(0, y_max), clip = "off") +
    labs(
      tag = panel_tag, title = title_text, x = "Year",
      y = paste0("Days with precipitation >", threshold_value, "mm")
    ) +
    theme_fig5()
}

panel_a <- plot_projection_panel(
  "sponge_cities", "semi_humid", 20,
  "a", "SCP pilots: semi-humid", 10, 2
)
panel_b <- plot_projection_panel(
  "others", "semi_humid", 20,
  "b", "Non-pilots: semi-humid", 10, 2
)
panel_d <- plot_projection_panel(
  "sponge_cities", "humid", 50,
  "d", "SCP pilots: humid", 3.5, 0.5
)
panel_e <- plot_projection_panel(
  "others", "humid", 50,
  "e", "Non-pilots: humid", 3.5, 0.5
)
panel_g <- plot_projection_panel(
  "sponge_cities", "hyper_humid", 80,
  "g", "SCP pilots: hyper-humid", 3.0, 0.5
)
panel_h <- plot_projection_panel(
  "others", "hyper_humid", 80,
  "h", "Non-pilots: hyper-humid", 3.0, 0.5
)

panel_list <- list(a = panel_a, b = panel_b, d = panel_d, e = panel_e, g = panel_g, h = panel_h)
for (panel_name in names(panel_list)) {
  ggsave(
    file.path(output_dir, paste0("fig5_panel_", panel_name, "_projection.png")),
    panel_list[[panel_name]], width = 14, height = 10, units = "cm", dpi = 600
  )
}

fig5_timeseries <- (panel_a | panel_b) / (panel_d | panel_e) / (panel_g | panel_h)
ggsave(
  file.path(output_dir, "fig5_projection_timeseries_6panel.png"),
  fig5_timeseries, width = 28, height = 30, units = "cm", dpi = 600
)
ggsave(
  file.path(output_dir, "fig5_projection_timeseries_6panel.pdf"),
  fig5_timeseries, width = 28, height = 30, units = "cm", device = cairo_pdf
)

# Read and project the administrative boundaries.
sf_use_s2(FALSE)
map_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +datum=WGS84 +units=m +no_defs"
city_sf <- st_read(city_boundary_file, quiet = TRUE) %>%
  mutate(city_code = as.character(`市代码`)) %>%
  st_transform(map_crs) %>% st_make_valid() %>% st_simplify(dTolerance = 300)
country_sf <- st_read(country_boundary_file, quiet = TRUE) %>%
  st_transform(map_crs) %>% st_make_valid() %>% st_simplify(dTolerance = 300)
country_lines <- st_read(country_line_file, quiet = TRUE) %>%
  st_transform(map_crs) %>% st_simplify(dTolerance = 300)
mainland <- suppressWarnings(
  st_crop(st_transform(country_sf, 4326), xmin = 73, xmax = 136, ymin = 18, ymax = 54)
) %>% st_transform(map_crs)
sf_use_s2(TRUE)

rotation_matrix <- function(angle) {
  matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2, 2)
}
rotate_geometry <- function(x, angle) {
  st_set_crs(st_geometry(x) * t(rotation_matrix(angle)), NA)
}
rotated_extent <- function(angle) {
  box <- st_bbox(rotate_geometry(mainland, angle))
  list(
    x = unname(box[c("xmin", "xmax")]) + c(-80000, 80000),
    y = unname(box[c("ymin", "ymax")]) + c(-180000, 80000)
  )
}
to_geographic <- function(xy, angle) {
  original <- as.numeric(rotation_matrix(-angle) %*% xy)
  st_transform(st_sfc(st_point(original), crs = map_crs), 4326)
}

# Rotate the Albers map and central-meridian north arrow together.
map_rotation <- 3 * pi / 180
extent <- rotated_extent(map_rotation)
map_x <- extent$x
map_y <- extent$y
for (object_name in c("city_sf", "country_sf", "country_lines")) {
  object <- get(object_name)
  st_geometry(object) <- rotate_geometry(object, map_rotation)
  assign(object_name, object)
}
inset_box <- st_bbox(
  rotate_geometry(
    st_transform(
      st_as_sfc(st_bbox(c(xmin = 105, ymin = 3, xmax = 125, ymax = 24), crs = st_crs(4326))),
      map_crs
    ),
    map_rotation
  )
)

blue_colors <- c("#87B6D8", "#5799C6", "#327FB6", "#1564A0", "#084A85", "#08306B")
star_vertices <- function(x, y, radius_x, radius_y = radius_x) {
  angle <- pi / 2 + (0:9) * pi / 5
  radius <- rep(c(1, 0.382), 5)
  data.frame(
    x = x + radius_x * radius * cos(angle),
    y = y + radius_y * radius * sin(angle)
  )
}

# Use blue legend blocks, with zero events shown separately.
block_legend <- function(breaks, threshold) {
  n <- length(breaks) - 1
  blocks <- data.frame(
    xmin = 0, xmax = 0.32, ymin = 0:(n - 1), ymax = 1:n,
    colour = blue_colors
  )
  ggplot(blocks) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour),
      colour = "grey35", linewidth = 0.25
    ) +
    scale_fill_identity() +
    annotate(
      "text", x = 0.42, y = 0:n, label = format(breaks, trim = TRUE),
      hjust = 0, size = 8 / .pt, family = "Arial"
    ) +
    annotate(
      "rect", xmin = 0, xmax = 0.32, ymin = -1.3, ymax = -0.65,
      fill = "grey88", colour = "grey35", linewidth = 0.25
    ) +
    annotate(
      "text", x = 0.42, y = -0.97, label = "No events",
      hjust = 0, size = 7.5 / .pt, family = "Arial"
    ) +
    geom_polygon(
      data = star_vertices(0.16, n + 2.8, 0.11, 0.23),
      aes(x = x, y = y), inherit.aes = FALSE,
      fill = NA, colour = "#F07840", linewidth = 0.35
    ) +
    annotate(
      "text", x = 0.42, y = n + 2.8, label = "SCP pilots",
      hjust = 0, size = 8 / .pt, family = "Arial"
    ) +
    annotate(
      "text", x = 0, y = n + 1.5,
      label = paste0("Mean days >", threshold, " mm\n(2025-2100)"),
      hjust = 0, size = 8 / .pt, family = "Arial", lineheight = 1.1
    ) +
    coord_cartesian(
      xlim = c(-0.02, 1.9), ylim = c(-1.6, n + 3.3),
      expand = FALSE, clip = "off"
    ) +
    theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
}

# Map the prepared city means without recalculating the projections.
make_map_panel <- function(threshold, tag, breaks) {
  values <- read.csv(
    file.path(map_input_dir, sprintf("city_level_ssp_%dmm_new.csv", threshold)),
    stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"
  )
  values$city_code <- as.character(values$city_code)
  values$value <- as.numeric(values[[paste0("avg_", threshold, "mm")]])
  mapped <- city_sf %>% inner_join(values, by = "city_code")
  mapped$bin <- cut(mapped$value, breaks = breaks, include.lowest = TRUE)
  mapped$map_colour <- ifelse(
    mapped$value == 0, "#E0E0E0", blue_colors[as.integer(mapped$bin)]
  )

  points <- suppressWarnings(st_point_on_surface(mapped[mapped$sponge_city == 1, ]))
  point_xy <- st_coordinates(points)
  stars <- st_sfc(lapply(seq_len(nrow(point_xy)), function(j) {
    xy <- as.matrix(star_vertices(point_xy[j, 1], point_xy[j, 2], 45000))
    st_polygon(list(rbind(xy, xy[1, ])))
  }))

  map_plot <- ggplot() +
    geom_sf(data = country_sf, fill = "white", colour = "grey45", linewidth = 0.22) +
    geom_sf(data = city_sf, fill = NA, colour = "grey72", linewidth = 0.12) +
    geom_sf(data = mapped, aes(fill = map_colour), colour = "grey65", linewidth = 0.15) +
    geom_sf(data = country_lines, colour = "grey50", linewidth = 0.2) +
    geom_sf(data = country_sf, fill = NA, colour = "grey38", linewidth = 0.34) +
    geom_sf(data = stars, fill = NA, colour = "#F07840", linewidth = 0.3) +
    scale_fill_identity() +
    coord_sf(xlim = map_x, ylim = map_y, expand = FALSE, datum = NA) +
    theme_void() + theme(plot.margin = margin(0, 0, 0, 0))

  # Calibrate the scale bar to local geodesic distance.
  bar_x <- map_x[1] + diff(map_x) * 0.08
  bar_y <- map_y[1] + diff(map_y) * 0.055
  bar_start <- to_geographic(c(bar_x, bar_y), map_rotation)
  bar_length <- uniroot(function(length) {
    as.numeric(
      st_distance(bar_start, to_geographic(c(bar_x + length, bar_y), map_rotation))
    ) - 1000000
  }, c(500000, 2000000))$root
  map_plot <- map_plot +
    annotate(
      "rect", xmin = bar_x, xmax = bar_x + bar_length / 2,
      ymin = bar_y, ymax = bar_y + 42000,
      fill = "black", linewidth = 0.25
    ) +
    annotate(
      "rect", xmin = bar_x + bar_length / 2, xmax = bar_x + bar_length,
      ymin = bar_y, ymax = bar_y + 42000,
      fill = "white", colour = "black", linewidth = 0.25
    ) +
    annotate(
      "text", x = bar_x + c(0, 0.5, 1) * bar_length,
      y = bar_y - 95000, label = c("0", "500", "1,000 km"),
      size = 6.5 / .pt, family = "Arial"
    )

  arrow_xy <- c(map_x[1] + diff(map_x) * 0.10, bar_y + 280000)
  north <- as.numeric(rotation_matrix(map_rotation) %*% c(0, 1))
  east <- c(north[2], -north[1])
  tip <- arrow_xy + north * 340000
  notch <- arrow_xy + north * 90000
  left <- arrow_xy - east * 95000
  right <- arrow_xy + east * 95000
  half_arrow <- function(vertices) data.frame(x = vertices[, 1], y = vertices[, 2])
  map_plot <- map_plot +
    geom_polygon(
      data = half_arrow(rbind(tip, left, notch)), aes(x = x, y = y),
      inherit.aes = FALSE, fill = "black", colour = "black", linewidth = 0.35
    ) +
    geom_polygon(
      data = half_arrow(rbind(tip, notch, right)), aes(x = x, y = y),
      inherit.aes = FALSE, fill = "white", colour = "black", linewidth = 0.35
    ) +
    annotate(
      "text", x = tip[1], y = tip[2] + 120000, label = "N",
      size = 10 / .pt, fontface = "bold", family = "Times New Roman"
    )

  inset <- ggplot() +
    geom_sf(data = country_sf, fill = "white", colour = "grey50", linewidth = 0.2) +
    geom_sf(data = mapped, aes(fill = map_colour), colour = "grey65", linewidth = 0.1) +
    geom_sf(data = country_lines, colour = "grey50", linewidth = 0.2) +
    geom_sf(data = country_sf, fill = NA, colour = "grey38", linewidth = 0.3) +
    scale_fill_identity() +
    coord_sf(
      xlim = unname(inset_box[c("xmin", "xmax")]),
      ylim = unname(inset_box[c("ymin", "ymax")]),
      expand = FALSE, datum = NA
    ) +
    theme_void() +
    theme(
      panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.35),
      plot.margin = margin(0, 0, 0, 0)
    )

  full <- ggplot() +
    theme_void() +
    labs(tag = tag) +
    theme(
      plot.tag = element_text(size = 18, face = "bold", family = "Arial"),
      plot.tag.position = c(0.01, 0.99),
      plot.margin = margin(5, 2, 10, 2)
    )
  full +
    inset_element(map_plot, left = 0, bottom = 0, right = 0.82, top = 1, align_to = "full") +
    inset_element(
      block_legend(breaks, threshold), left = 0.82, bottom = 0.29,
      right = 1, top = 0.92, align_to = "full"
    ) +
    inset_element(inset, left = 0.808, bottom = 0.01, right = 0.983, top = 0.27, align_to = "full")
}

panel_c <- make_map_panel(20, "c", c(0, 2, 4, 6, 8, 10, 12))
panel_f <- make_map_panel(50, "f", c(0, 1, 2, 3, 4, 5, 7))
panel_i <- make_map_panel(80, "i", c(0, 1, 2, 3, 4, 5, 7))

for (panel_name in c("c", "f", "i")) {
  ggsave(
    file.path(output_dir, paste0("fig5_panel_", panel_name, "_map.png")),
    get(paste0("panel_", panel_name)),
    width = 14, height = 10, units = "cm", dpi = 600, bg = "white"
  )
}

# Assemble and export the complete nine-panel figure.
fig5_full <- wrap_plots(
  panel_a, panel_b, panel_c,
  panel_d, panel_e, panel_f,
  panel_g, panel_h, panel_i,
  ncol = 3, widths = c(1, 1, 1.08)
)
ggsave(
  file.path(output_dir, "fig5_projection_9panel.png"),
  fig5_full, width = 43.12, height = 30, units = "cm", dpi = 600, bg = "white"
)
ggsave(
  file.path(output_dir, "fig5_projection_9panel.pdf"),
  fig5_full, width = 43.12, height = 30, units = "cm",
  device = cairo_pdf, bg = "white"
)
ggsave(
  file.path(output_dir, "fig5_projection_9panel_preview.png"),
  fig5_full, width = 43.12, height = 30, units = "cm", dpi = 140, bg = "white"
)
