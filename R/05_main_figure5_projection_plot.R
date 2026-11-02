################################################################################
# Project: China's Sponge City Programme and rain-induced road congestion
# File:    05_main_figure5_projection_plot.R
# Purpose: Draw the complete Fig. 5 projection time-series and map panels
# Drawing: Final aligned layout with projected latitude/longitude labels.
#          Original input paths, CSV columns and output names are retained.
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

# All panel positions below are in millimetres on the final publication page.
# A single set of panel heights aligns every upper and lower frame exactly.
# The outer padding also keeps the right latitude labels and bottom axis titles visible.
figure_width_mm <- 192
figure_height_mm <- 164
panel_width_mm <- 54
panel_height_mm <- 42
panel_x_mm <- c(9.5, 72.0, 133.5)
panel_y_mm <- c(113, 60, 7)
# Reserve enough room for the complete Historical label before the 2024.5 divider.
time_x_limits <- c(2011, 2101.7)
period_label_x <- c(mean(c(time_x_limits[1], 2024.5)), 2042.5, 2080.5)
legend_bar_width_mm <- 20
inset_width_mm <- 8.5
inset_right_mm <- 0.8
inset_bottom_mm <- 0.8
font_pt <- 5.0
title_pt <- 5.1
period_pt <- 4.2
map_font_pt <- 4.15
# Match Fig2's panel labels at its final 180 mm submission width.
tag_pt <- 13.7579616927
font_family <- 'Arial'
line_scale <- 18 / 43.12
blue_colours <- c('#87B6D8', '#5799C6', '#327FB6', '#1564A0', '#084A85', '#08306B')

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
  ggplot() +
    geom_ribbon(data = future_df, aes(year, ymin = ci_lower, ymax = ci_upper, fill = scenario_label), alpha = 0.15, linewidth = 0) +
    geom_line(data = historical_df, aes(year, mean_days), colour = 'black', linewidth = 0.8 * line_scale) +
    geom_line(data = future_df, aes(year, mean_days, colour = scenario_label), linewidth = 0.8 * line_scale, alpha = 0.8) +
    geom_vline(xintercept = c(2024.5, 2060.5), linetype = 'dashed', colour = 'grey40', linewidth = 0.4 * line_scale, alpha = 0.7) +
    annotate('text', x = period_label_x, y = y_max * 0.965,
      label = c('Historical', 'Near-Future', 'Far-Future'),
      size = period_pt / .pt, fontface = 'bold', family = font_family) +
    scale_colour_manual(name = 'Scenario', values = scenario_colors) +
    scale_fill_manual(name = 'Scenario', values = scenario_colors) +
    scale_x_continuous(limits = time_x_limits, breaks = seq(2015, 2095, 10), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, y_max, y_break_by), expand = c(0.02, 0)) +
    coord_cartesian(ylim = c(0, y_max), clip = 'on') +
    labs(x = 'Year', y = paste0('Days with precipitation >', threshold_value, ' mm')) +
    guides(colour = guide_legend(override.aes = list(linewidth = 0.8 * line_scale)), fill = guide_legend()) +
    theme_bw(base_size = font_pt, base_family = font_family) +
    theme(text = element_text(colour = 'black'), axis.text = element_text(colour = 'black'),
      axis.title.x = element_text(margin = margin(t = 2.6)),
      axis.title.y = element_text(margin = margin(r = 2.6)),
      axis.ticks = element_line(linewidth = 0.18), axis.ticks.length = unit(0.6, 'mm'),
      panel.grid = element_blank(), panel.border = element_rect(fill = NA, colour = 'black', linewidth = 0.22),
      legend.position = 'inside', legend.position.inside = c(0.98, 0.025), legend.justification = c(1, 0),
      legend.title = element_text(size = 4.7), legend.text = element_text(size = 4.7),
      legend.key = element_blank(), legend.background = element_blank(),
      legend.key.width = unit(2.3, 'mm'), legend.key.height = unit(2.0, 'mm'),
      legend.spacing.y = unit(0.3, 'mm'), legend.margin = margin(0, 0, 0, 0),
      plot.margin = margin(0, 0, 0, 0))
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
# Preserve the original Albers projection and three-degree page rotation.
map_rotation <- 3 * pi / 180
rotated_box <- st_bbox(rotate_geometry(mainland, map_rotation))
map_x <- unname(rotated_box[c("xmin", "xmax")]) + c(-260000, 850000)
map_y_top <- unname(rotated_box["ymax"]) + 140000
map_y <- c(map_y_top - diff(map_x) * panel_height_mm / panel_width_mm, map_y_top)
for (object_name in c("city_sf", "country_sf", "country_lines")) {
  object <- get(object_name)
  st_geometry(object) <- rotate_geometry(object, map_rotation)
  assign(object_name, object)
}
project_xy <- function(lon, lat) {
  points <- st_sfc(lapply(seq_along(lon), function(i) st_point(c(lon[i], lat[i]))), crs = 4326)
  xy <- st_coordinates(st_transform(points, map_crs))
  t(rotation_matrix(map_rotation) %*% t(xy))
}

# Curves and edge labels are calculated from the same projected graticule.
graticule <- lapply(seq(50, 160, 10), function(lon) {
  xy <- project_xy(rep(lon, 501), seq(-10, 75, length.out = 501))
  list(type = 'E', degree = lon, xy = xy)
})
graticule <- c(graticule, lapply(seq(0, 70, 10), function(lat) {
  xy <- project_xy(seq(40, 170, length.out = 601), rep(lat, 601))
  list(type = 'N', degree = lat, xy = xy)
}))
graticule_sf <- st_sf(geometry = st_sfc(lapply(graticule, function(g) st_linestring(g$xy))))
crossings <- function(xy, dimension, value) {
  d <- xy[, dimension] - value
  indices <- which(d[-length(d)] * d[-1] <= 0 & diff(xy[, dimension]) != 0)
  if (!length(indices)) return(matrix(numeric(), 0, 2))
  do.call(rbind, lapply(indices, function(i) {
    xy[i, ] + (xy[i + 1, ] - xy[i, ]) * (-d[i] / (d[i + 1] - d[i]))
  }))
}
edge_labels <- do.call(rbind, lapply(graticule, function(g) {
  sides <- if (g$type == 'E' && g$degree %% 20 == 0) c('bottom', 'top') else if (g$type == 'N' && g$degree %in% c(20, 30, 40, 50)) c('left', 'right') else character()
  if (!length(sides)) return(NULL)
  do.call(rbind, lapply(sides, function(side) {
    dimension <- if (side %in% c('left', 'right')) 1 else 2
    edge_value <- switch(side, left = map_x[1], right = map_x[2], bottom = map_y[1], top = map_y[2])
    xy <- crossings(g$xy, dimension, edge_value)
    if (!nrow(xy)) return(NULL)
    u <- (xy[, 1] - map_x[1]) / diff(map_x)
    v <- (xy[, 2] - map_y[1]) / diff(map_y)
    keep <- u >= -1e-8 & u <= 1 + 1e-8 & v >= -1e-8 & v <= 1 + 1e-8
    if (side %in% c('left', 'right')) keep <- keep & v > 0.035 & v < 0.965 else keep <- keep & u > 0.035 & u < 0.965
    if (!any(keep)) return(NULL)
    data.frame(side = side, degree = g$degree, type = g$type, u = u[keep], v = v[keep], label = paste0(g$degree, '\u00b0', g$type))
  }))
}))

star_vertices <- function(x, y, radius_x, radius_y = radius_x) {
  angle <- pi / 2 + (0:9) * pi / 5
  radius <- rep(c(1, 0.382), 5)
  data.frame(
    x = x + radius_x * radius * cos(angle),
    y = y + radius_y * radius * sin(angle)
  )
}

# Read the same prepared city means as the original script.
# Plotting does not recalculate, round or adjust the supplied values.
read_map_panel <- function(threshold, breaks) {
  values <- read.csv(
    file.path(map_input_dir, sprintf("city_level_ssp_%dmm_new.csv", threshold)),
    stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"
  )
  values$city_code <- as.character(values$city_code)
  values$value <- as.numeric(values[[paste0("avg_", threshold, "mm")]])
  mapped <- city_sf %>% inner_join(values, by = "city_code")
  mapped$bin <- cut(mapped$value, breaks = breaks, include.lowest = TRUE)
  mapped$map_colour <- ifelse(
    mapped$value == 0, "#E0E0E0", blue_colours[as.integer(mapped$bin)]
  )

  points <- suppressWarnings(st_point_on_surface(mapped[mapped$sponge_city == 1, ]))
  point_xy <- st_coordinates(points)
  stars <- st_sfc(lapply(seq_len(nrow(point_xy)), function(j) {
    xy <- as.matrix(star_vertices(point_xy[j, 1], point_xy[j, 2], 45000))
    st_polygon(list(rbind(xy, xy[1, ])))
  }))

  list(mapped = mapped, stars = stars, values = values)
}

inset_extent <- project_xy(c(105, 125, 125, 105), c(3, 3, 24, 24))
inset_x <- range(inset_extent[, 1]); inset_y <- range(inset_extent[, 2])
inset_w <- inset_width_mm
inset_h <- inset_w * diff(inset_y) / diff(inset_x)

map_panel <- function(d, inset = FALSE) {
  if (!inset) {
    plot <- ggplot() +
      geom_sf(data = country_sf, fill = 'white', colour = NA) +
      geom_sf(data = d$mapped, aes(fill = map_colour), colour = NA) +
      geom_sf(data = graticule_sf, colour = '#DDE1E5', linewidth = 0.07, alpha = 0.95) +
      geom_sf(data = city_sf, fill = NA, colour = 'grey72', linewidth = 0.12 * line_scale) +
      geom_sf(data = d$mapped, fill = NA, colour = 'grey65', linewidth = 0.15 * line_scale) +
      geom_sf(data = country_lines, colour = 'grey50', linewidth = 0.2 * line_scale) +
      geom_sf(data = country_sf, fill = NA, colour = 'grey38', linewidth = 0.34 * line_scale) +
      geom_sf(data = d$stars, fill = NA, colour = '#F07840', linewidth = 0.3 * line_scale) +
      coord_sf(xlim = map_x, ylim = map_y, expand = FALSE, datum = NA)
  } else {
    # Preserve the map's projected aspect ratio in the inset as well.
    plot <- ggplot() +
      geom_sf(data = country_sf, fill = 'white', colour = 'grey50', linewidth = 0.2 * line_scale) +
      geom_sf(data = d$mapped, aes(fill = map_colour), colour = 'grey65', linewidth = 0.1 * line_scale) +
      geom_sf(data = graticule_sf, colour = '#E5E8EB', linewidth = 0.045) +
      geom_sf(data = country_lines, colour = 'grey50', linewidth = 0.2 * line_scale) +
      geom_sf(data = country_sf, fill = NA, colour = 'grey38', linewidth = 0.3 * line_scale) +
      coord_sf(xlim = inset_x, ylim = inset_y, expand = FALSE, datum = NA)
  }
  grob <- ggplotGrob(plot + scale_fill_identity() + theme_void() +
    theme(panel.background = element_rect(fill = 'white', colour = NA), plot.margin = margin(0, 0, 0, 0)))
  grob$grobs[[which(grob$layout$name == 'panel')]]
}

# The panel order, city groups, thresholds and map breaks are unchanged.
time_specs <- list(
  list(tag = "a", row = 1, col = 1, title = "SCP pilots: semi-humid"),
  list(tag = "b", row = 1, col = 2, title = "Non-pilots: semi-humid"),
  list(tag = "d", row = 2, col = 1, title = "SCP pilots: humid"),
  list(tag = "e", row = 2, col = 2, title = "Non-pilots: humid"),
  list(tag = "g", row = 3, col = 1, title = "SCP pilots: hyper-humid"),
  list(tag = "h", row = 3, col = 2, title = "Non-pilots: hyper-humid"))
map_specs <- list(
  list(tag = "c", threshold = 20, breaks = c(0, 2, 4, 6, 8, 10, 12)),
  list(tag = "f", threshold = 50, breaks = c(0, 1, 2, 3, 4, 5, 7)),
  list(tag = "i", threshold = 80, breaks = c(0, 1, 2, 3, 4, 5, 7)))

# Measure and export on the same font-capable device. The temporary file is
# only a layout device; it is not an additional publication output.
layout_device_file <- tempfile(fileext = ".pdf")
cairo_pdf(layout_device_file, width = figure_width_mm / 25.4,
          height = figure_height_mm / 25.4, family = font_family, bg = "white")
time_grobs <- lapply(time_specs, function(s) ggplotGrob(panel_list[[s$tag]]))
map_data <- lapply(map_specs, function(s) read_map_panel(s$threshold, s$breaks))
map_grobs <- lapply(map_data, map_panel)
inset_grobs <- lapply(map_data, map_panel, inset = TRUE)
dev.off()
unlink(layout_device_file)

text_mm <- function(label, x, y, size, just = 'centre', rot = 0, face = 'plain') {
  grid.text(label, x = unit(x, 'mm'), y = unit(y, 'mm'), just = just, rot = rot,
    gp = gpar(fontfamily = font_family, fontsize = size, fontface = face, col = 'black'))
}
line_mm <- function(x, y, col = 'black', lwd = 0.55) grid.lines(unit(x, 'mm'), unit(y, 'mm'), gp = gpar(col = col, lwd = lwd))
rect_mm <- function(x, y, w, h, fill = NA, col = 'black', lwd = 0.55) {
  grid.rect(x = unit(x, 'mm'), y = unit(y, 'mm'), width = unit(w, 'mm'), height = unit(h, 'mm'),
    just = c('left', 'bottom'), gp = gpar(fill = fill, col = col, lwd = lwd))
}
draw_time_at <- function(g, x, y) {
  p <- g$layout[g$layout$name == 'panel', ][1, ]
  g$widths[p$l:p$r] <- unit(panel_width_mm, 'mm')
  g$heights[p$t:p$b] <- unit(panel_height_mm, 'mm')
  left <- sum(g$widths[seq_len(p$l - 1)])
  bottom <- sum(g$heights[seq.int(p$b + 1, length(g$heights))])
  pushViewport(viewport(x = unit(x, 'mm') - left, y = unit(y, 'mm') - bottom,
    width = sum(g$widths), height = sum(g$heights), just = c('left', 'bottom'), clip = 'off'))
  grid.draw(g)
  popViewport()
}
draw_legend <- function(s, x, y) {
  left <- x + 1.5; bottom <- y + 1.15
  bar_y <- bottom + 3.65; bar_w <- legend_bar_width_mm; bar_h <- 1.45
  text_mm(paste0('Mean days >', s$threshold, ' mm'), left, bottom + 9.0, 4.15, just = 'left')
  text_mm('(2025-2100)', left, bottom + 6.85, 4.0, just = 'left')
  n <- length(s$breaks) - 1
  for (j in seq_len(n)) rect_mm(left + (j - 1) * bar_w / n, bar_y, bar_w / n, bar_h, blue_colours[j], col = NA)
  rect_mm(left, bar_y, bar_w, bar_h, col = 'grey40', lwd = 0.3)
  for (j in seq_along(s$breaks)) text_mm(s$breaks[j], left + (j - 1) * bar_w / n, bar_y - 1.0, 3.85)
  rect_mm(left, bottom - 0.05, 1.8, 1.35, '#E0E0E0', col = 'grey40', lwd = 0.3)
  text_mm('No events', left + 2.6, bottom + 0.58, 3.85, just = 'left')
  star <- star_vertices(left + 12.2, bottom + 0.62, 0.72)
  grid.polygon(unit(star$x, 'mm'), unit(star$y, 'mm'), gp = gpar(fill = NA, col = '#F07840', lwd = 0.45))
  text_mm('SCP pilots', left + 13.4, bottom + 0.58, 3.85, just = 'left')
}
draw_map_at <- function(s, grob, inset_grob, x, y) {
  pushViewport(viewport(x = unit(x, 'mm'), y = unit(y, 'mm'),
    width = unit(panel_width_mm, 'mm'), height = unit(panel_height_mm, 'mm'), just = c('left', 'bottom'), clip = 'on'))
  grid.draw(grob)
  popViewport()
  draw_legend(s, x, y)
  ix <- x + panel_width_mm - inset_w - inset_right_mm; iy <- y + inset_bottom_mm
  pushViewport(viewport(x = unit(ix, 'mm'), y = unit(iy, 'mm'), width = unit(inset_w, 'mm'),
    height = unit(inset_h, 'mm'), just = c('left', 'bottom'), clip = 'on'))
  grid.draw(inset_grob)
  popViewport()
  rect_mm(ix, iy, inset_w, inset_h, lwd = 0.4)
  rect_mm(x, y, panel_width_mm, panel_height_mm, lwd = 0.62)
  for (j in seq_len(nrow(edge_labels))) {
    e <- edge_labels[j, ]; xx <- x + e$u * panel_width_mm; yy <- y + e$v * panel_height_mm
    if (e$side == 'bottom') {
      line_mm(c(xx, xx), c(y, y - 0.6), lwd = 0.4)
      text_mm(e$label, xx, y - 1.7, map_font_pt)
    } else if (e$side == 'top') {
      line_mm(c(xx, xx), c(y + panel_height_mm, y + panel_height_mm + 0.6), lwd = 0.4)
      text_mm(e$label, xx, y + panel_height_mm + 1.8, map_font_pt)
    } else if (e$side == 'left') {
      line_mm(c(x, x - 0.6), c(yy, yy), lwd = 0.4)
      text_mm(e$label, x - 1.8, yy, map_font_pt, rot = 90)
    } else {
      line_mm(c(x + panel_width_mm, x + panel_width_mm + 0.6), c(yy, yy), lwd = 0.4)
      text_mm(e$label, x + panel_width_mm + 1.7, yy, map_font_pt, rot = 90)
    }
  }
}
draw_time_panels <- function() {
  for (i in seq_along(time_specs)) {
    s <- time_specs[[i]]; x <- panel_x_mm[s$col]; y <- panel_y_mm[s$row]
    draw_time_at(time_grobs[[i]], x, y)
    text_mm(s$title, x + panel_width_mm / 2,
            y + panel_height_mm + 2.8, title_pt, face = "bold")
    text_mm(s$tag, x - 7.8, y + panel_height_mm + 4.6,
            tag_pt, just = "left", face = "bold")
  }
}
draw_figure <- function() {
  grid.newpage()
  pushViewport(viewport(width = unit(figure_width_mm, "mm"),
                        height = unit(figure_height_mm, "mm")))
  draw_time_panels()
  for (i in seq_along(map_specs)) {
    x <- panel_x_mm[3]; y <- panel_y_mm[i]
    draw_map_at(map_specs[[i]], map_grobs[[i]], inset_grobs[[i]], x, y)
    text_mm(map_specs[[i]]$tag, x - 2.8, y + panel_height_mm + 4.6,
            tag_pt, just = "left", face = "bold")
  }
  popViewport()
}
draw_timeseries <- function() {
  grid.newpage()
  draw_time_panels()
}
draw_time_panel <- function(i) {
  grid.newpage()
  s <- time_specs[[i]]
  draw_time_at(time_grobs[[i]], 9.5, 7)
  text_mm(s$title, 9.5 + panel_width_mm / 2,
          7 + panel_height_mm + 2.8, title_pt, face = "bold")
  text_mm(s$tag, 1.7, 7 + panel_height_mm + 4.6,
          tag_pt, just = "left", face = "bold")
}
draw_map_panel <- function(i) {
  grid.newpage()
  draw_map_at(map_specs[[i]], map_grobs[[i]], inset_grobs[[i]], 5, 7)
  text_mm(map_specs[[i]]$tag, 2.2, 7 + panel_height_mm + 4.6,
          tag_pt, just = "left", face = "bold")
}

# Export the original collection of PNG/PDF files under their original names.
for (i in seq_along(time_specs)) {
  png(file.path(output_dir, paste0("fig5_panel_", time_specs[[i]]$tag, "_projection.png")),
      width = 65.5, height = 56, units = "mm", res = 600, type = "cairo", bg = "white")
  draw_time_panel(i)
  dev.off()
}
png(file.path(output_dir, "fig5_projection_timeseries_6panel.png"),
    width = 130, height = figure_height_mm, units = "mm", res = 600,
    type = "cairo", bg = "white")
draw_timeseries()
dev.off()
cairo_pdf(file.path(output_dir, "fig5_projection_timeseries_6panel.pdf"),
          width = 130 / 25.4, height = figure_height_mm / 25.4,
          family = font_family, bg = "white")
draw_timeseries()
dev.off()
for (i in seq_along(map_specs)) {
  png(file.path(output_dir, paste0("fig5_panel_", map_specs[[i]]$tag, "_map.png")),
      width = 64, height = 56, units = "mm", res = 600, type = "cairo", bg = "white")
  draw_map_panel(i)
  dev.off()
}
png(file.path(output_dir, "fig5_projection_9panel.png"),
    width = figure_width_mm, height = figure_height_mm, units = "mm",
    res = 600, type = "cairo", bg = "white")
draw_figure()
dev.off()
cairo_pdf(file.path(output_dir, "fig5_projection_9panel.pdf"),
          width = figure_width_mm / 25.4, height = figure_height_mm / 25.4,
          family = font_family, bg = "white")
draw_figure()
dev.off()
png(file.path(output_dir, "fig5_projection_9panel_preview.png"),
    width = figure_width_mm, height = figure_height_mm, units = "mm",
    res = 140, type = "cairo", bg = "white")
draw_figure()
dev.off()
