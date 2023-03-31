
calc_srt <- function(slope, adj = 0.0) {
  atan(slope) * 360 / (2*pi) + adj
}

rline <- function(theta, r = 1, ...) {
  lines(c(0, r * cos(theta)), c(0, r * sin(theta)), ...)
}

rarrow <- function(theta, r = 1, ...) {
  arrows(x0 = 0, y0 = 0, x1 = r * cos(theta), y1 = r * sin(theta), ...)
}

xy <- function(theta, r = 1) {
  list(x = r * cos(theta), y = r * sin(theta))
}

cell <- function(x, y, h, w, col = NA, border = NA) {
  rect(x - h/2, y - w/2, x + h/2, y + w/2, col = col, border = border)
}

plot_circle <- function(x, y, r = 1, prop = 1, ...) {
  thetas <- seq(0, prop * 2*pi, length.out = 100)
  xs <- x + cos(thetas) * r
  ys <- y + sin(thetas) * r
  if (prop == 1) {
    polygon(xs, ys, ...)
  } else if (prop < 1) {
    xs <- c(x, xs, x)
    ys <- c(y, ys, y)
    polygon(xs, ys, ...)
  } else {
    stop ("prop invalid!")
  }
}


add_vertical_scale <- function(scale_max, scale_min, text_cex = 1.0) {

  par(mar = c(0, 0, 0, 0))

  plot(NULL, type = "n", axes = FALSE, ann = FALSE, ylim = c(0, 1), xlim = c(0, 1))

  scale_bottom_y <- 0.2
  scale_top_y <- 0.8
  cell_width <- 0.2
  ys <- seq(scale_bottom_y, scale_top_y, length.out = 100)
  delta <- diff(ys)[1]
  water_values <- seq(scale_min, 0, length.out = length(ys)/2)
  land_values <- seq(0, scale_max, length.out = length(ys)/2 + 1)
  land_values <- land_values[-1]
  data_values <- c(water_values, land_values)
  scale_zero_y <- ys[which.min(abs(data_values))] - delta/2
  cols <- data_gradient2(data_values, my.start = scale_min, my.stop = scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30)

  rect(0.5 - cell_width / 2, ys - delta/2, 0.5 + cell_width / 2, ys + delta/2, col = cols, border = cols)
  rect(0.5 - cell_width/2, scale_bottom_y - 0.005, 0.5 + cell_width/2, scale_top_y + 0.005)
  curve(scale_zero_y + 0 * x, from = 0.5 - cell_width / 2, to = 0.5 + cell_width / 2, col = zero_line_col, add = TRUE)

  text(0.5 + 1.7 * cell_width, c(scale_top_y, scale_zero_y, scale_bottom_y), labels = c(scale_max, "0", scale_min), cex = text_cex)
  text(0.5, scale_top_y + 0.06, labels = "$\\overline{w} \\Delta \\overline{x}$", cex = text_cex * 1.3)

}
