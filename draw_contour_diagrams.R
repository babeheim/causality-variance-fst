
rm(list = ls())

source("project_support.R")

draw_arrows <- FALSE

my_xlab <- "frequency of $A$, $\\overline{x}$"
my_ylab <- "$F_{ST}$"

k <- 0.6 # equilibrium p-tilde
# note that A is GBT in invisible hand only if k > 0.5

zero_line_col <- col_alpha("black", 0.5)

tikz("figures/prisonersDilemmaContourDiagram.tex", height = 4, width = 4)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

land_scale_max <- 1
water_scale_min <- -1.0

phi <- 0.6
b <- 7
c <- phi * b

plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)

# add polygons to represent regions above/below a certain `level`

xs <- seq(0, 1, by = 0.01)

# need base colors defined by the 0 line
level <- 0
ys <- (level + xs * (1 - xs) * c) / (xs * (1 - xs) * b)
ys[is.na(ys)] <- c/b

ocean_x <- c(0, xs, 1)
ocean_y <- c(0, ys, 0)
polygon(ocean_x, ocean_y, col = water_colors[length(water_colors)], border = NA)
land_x <- c(0, xs, 1)
land_y <- c(1, ys, 1)
polygon(land_x, land_y, col = land_colors[1], border = NA)

# now create polygons at various levels 

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

for (i in 1:length(land_levels)) {
  level <- land_levels[i]
  ys <- (level + xs * (1 - xs) * c) / (xs * (1 - xs) * b)
  ys[ys > 1] <- 1.1
  ys[ys < 0] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = 1.0, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

for (i in 1:length(water_levels)) {
  level <- water_levels[i]
  ys <- (level + xs * (1 - xs) * c) / (xs * (1 - xs) * b)
  ys[ys > 1] <- 1.1
  ys[ys < 0] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = 1.0, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

# add annotations

polygon(c(0, 1, 1, 0), c(0, 0, 1, 1))

mtext("Prisoner's Dilemma", side = 3, line = 0.3)

mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, 1), labels = c(0, 1), cex.axis = 1.5) 
axis(2, at = c(0, c/b, 1), labels = c("0", "$\\displaystyle\\frac{c}{b}$", "1"), las = 2, cex.axis = 1.5)

abline(h = c/b, col = zero_line_col)

if (draw_arrows) {
  sca <- 3
  for (x in c(0.25, 0.5, 0.75)) {
    for (F in c(0.25, 0.5, 0.75)) {
      z <- x * (1 - x) * (b * F - c)
      if (z > 0) {
        text(x, F, labels = "$\\rightarrow$", cex = abs(z) * sca + 2)
      } else {
        text(x, F, labels = "$\\leftarrow$", cex = abs(z) * sca + 2)
      }
    }
  }
}

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# add scale

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()



tikz("figures/prisonersDilemmaSynergyContourDiagram.tex", height = 4, width = 4)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

land_scale_max <- 1
water_scale_min <- -0.4

k <- 2
m <- 6
n <- 4
l <- m/(m-n)

b <- n
d <- m - b
c <- k * d

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * seq(0.01, 0.3, length.out = 20)

plot_synergistic_contours(k, n, m)

mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = 1.5)
axis(2, at = c(0, c / (b + d), 1), labels = c("0", "$\\frac{c}{b+d}$", "1"), las = 1, cex.axis = 1.5)
axis(4, at = c(-100, (c - d) / b, 100), labels = c("", "$\\frac{c-d}{b}$", ""), las = 1, cex.axis = 1.5)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()



tikz("figures/stagHuntContourDiagram.tex", height = 4, width = 4, standAlone = FALSE)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

land_scale_max <- 1
water_scale_min <- -0.4

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * seq(0.01, 0.3, length.out = 20)

k <- 0.6
m <- 5
n <- 0
l <- m/(m-n)

plot_synergistic_contours(k, n, m)

mtext("\\textit{Stag Hunt}", side = 3, line = 0.3)
mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = 1.5)
axis(2, at = c(0, k, 1), labels = c("0", "$k$", "1"), las = 1, cex.axis = 1.5)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()



tikz("figures/hawkDoveContourDiagram.tex", height = 4, width = 4, standAlone = FALSE)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

land_scale_max <- 1
water_scale_min <- -0.4

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * seq(0.01, 0.3, length.out = 20)

k <- 0.6
m <- 1
n <- m * (2-k)/(1-k)
l <- m/(m-n)

plot_synergistic_contours(k, n, m)

mtext("\\textit{Hawk-Dove}", side = 3, line = 0.3)
mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = 1.5)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = 1.5)
axis(4, at = (1-k)/(1-l), labels = "$\\frac{1-k}{2-k}$", las = 1, line = -0.7, tick = FALSE, cex.axis = 1.5)
axis(4, at = (1-k)/(1-l), labels = FALSE, tck = -0.02)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()



tikz("figures/pureCoordinationContourDiagram.tex", height = 4, width = 4, standAlone = FALSE)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

land_scale_max <- 1
water_scale_min <- -1.0

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

k <- 0.6
m <- 10
n <- -m * (1-k)/k
l <- m/(m-n)

plot_synergistic_contours(k, n, m)

mtext("\\textit{Pure Coordination}", side = 3, line = 0.3)
mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = 1.5)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = 1.5)
abline(v = k, col = zero_line_col)
polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()



tikz("figures/invisibleHandContourDiagram.tex", height = 4, width = 4, standAlone = FALSE)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

land_scale_max <- 1
water_scale_min <- -0.5

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * seq(0.01, 0.7, length.out = 20)

k <- 0.6
m <- (-5)
n <- -m * k/(1-k)
l <- m/(m-n)

plot_synergistic_contours(k, n, m)

mtext("\\textit{Invisible Hand}", side = 3, line = 0.3)
mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, 1), labels = c("0", "1"), cex.axis = 1.5)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = 1.5)
axis(1, at = c(l, k), labels = c("$\\ell$", "$k$"), cex.axis = 1.5)

if (k == 0.5) {
  abline(v = k, col = zero_line_col)
} else if (k < 0.5) {
  x <- seq(0, 1, by = 0.01)
  points(x, (x - k)/(x - l), type = "l", col = zero_line_col)
  axis(2, at = k/l, labels = "$\\frac{k}{1 - k}", las = 1, line = -0.7, tick = FALSE, cex.axis = 1.5)
  axis(2, at = k/l, labels = FALSE, tck = -0.01)
} else {
  x <- seq(0, 1, by = 0.01)
  points(x, (x - k)/(x - l), type = "l", col = zero_line_col)
  axis(4, at = (1-k)/(1-l), labels = "$\\frac{1-k}{k}$", las = 1, line = -0.7, tick = FALSE, cex.axis = 1.5)
  axis(4, at = (1-k)/(1-l), labels = FALSE, tck = -0.02)
}

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()



tikz("figures/impureCoordinationContourDiagram.tex", height = 4, width = 4, standAlone = FALSE)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

land_scale_max <- 1.5
water_scale_min <- -0.5

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * seq(0.01, 0.7, length.out = 20)

k <- 1/7 # game structure in Allen and Nowak 2015
# when theta = 0, its a Stag Hunt
# when theta = atan2(y = -(1-k), x = k), its a pure coordinatino game
# so it just has to be between these values!
theta <- atan2(-5, 2) # game structure in Allen and Nowak 2015
stopifnot(theta < 0)
stopifnot(theta > atan2(y = -(1-k), x = k))

m <- cos(theta) * 8 
n <- sin(theta) * 8
l <- m/(m-n)

plot_synergistic_contours(k, n, m)

mtext("Impure Coordination", side = 3, line = 0.3)
mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, l, 1), labels = c("0", "$\\ell$", "1"), cex.axis = 1.5)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = 1.5)
axis(1, at = k, labels = "$k$", cex.axis = 1.5)

if (k == 0.5) {
  abline(v = k, col = zero_line_col)
} else if (k > 0.5) {
  x <- seq(0, k, by = 0.01)
  points(x, (x - k)/(x - l), type = "l", col = zero_line_col)
  axis(2, at = k/l, labels = "$\\displaystyle\\frac{k}{\\ell}$", las = 1, cex.axis = 1.5)
} else {
  x <- seq(0, k, by = 0.01)
  points(x, (x - k)/(x - l), type = "l", col = zero_line_col)
  axis(2, at = k/l, labels = "$\\displaystyle\\frac{k}{\\ell}$", las = 1, cex.axis = 1.5)
}

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()



tikz("figures/fourGameContourDiagram.tex", width = 6.5, height = 6.5 * 6/6.7)

layout(matrix(c(1, 2, 5,
                3, 4, 5), nrow=2, byrow=TRUE), widths = c(3,3,0.7), heights = c(3,3,6), respect = TRUE)
                 
# layout.show(5)

k <- 0.6 # unless k >= 0.5 invisible hand isn't a game where A is GBT

# define 'max' of the color scale
land_scale_max <- 1
water_scale_min <- -0.7

axis_text <- 1.5

par(mar = c(3.5, 3.5, 3, 2))

# Stag Hunt

m <- 5
n <- 0
l <- m/(m-n)

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

plot_synergistic_contours(k, n, m)

mtext("\\textit{Stag Hunt}", side = 3, line = 0.3, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = axis_text)
axis(2, at = c(0, k, 1), labels = c("0", "$k$", "1"), las = 1, cex.axis = axis_text)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# Hawk-Dove

m <- 1
n <- m * (2-k)/(1-k)
l <- m/(m-n)

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

plot_synergistic_contours(k, n, m)

mtext("\\textit{Hawk Dove}", side = 3, line = 0.3, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = axis_text)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = axis_text)
axis(4, at = (1-k)/(1-l), labels = "$\\frac{1-k}{2-k}$", las = 1, line = -0.7, tick = FALSE, cex.axis = axis_text)
axis(4, at = (1-k)/(1-l), labels = FALSE, tck = -0.02)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# Pure Coordination

m <- 10
n <- -m * (1-k)/k
l <- m/(m-n)

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

plot_synergistic_contours(k, n, m)

mtext("\\textit{Pure Coordination}", side = 3, line = 0.3, cex = 0.9)

mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = axis_text)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = axis_text)
abline(v = k, col = zero_line_col)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# Invisible Hand

m <- (-3.9)
n <- -m * k/(1-k)
l <- m/(m-n)

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

plot_synergistic_contours(k, n, m)

mtext("\\textit{Invisible Hand}", side = 3, line = 0.3, cex = 0.9)

mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)

axis(1, at = c(0, 1), labels = c("0", "1"), cex.axis = axis_text)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = axis_text)
axis(1, at = c(m/(m-n), k), labels = c("$\\ell$", "$k$"), cex.axis = axis_text)

if (k == 0.5) {
  abline(v = k, col = zero_line_col)
} else if (k < 0.5) {
  x <- seq(0, 1, by = 0.01)
  points(x, (x - k)/(x - l), type = "l", col = zero_line_col)
  axis(2, at = k/l, labels = "D", las = 1)
} else {
  x <- seq(k, 1, by = 0.01)
  points(x, (x - k)/(x - l), type = "l", col = zero_line_col)
  axis(4, at = (1-k)/(1-l), labels = "$\\frac{1-k}{k}$", las = 1, line = -0.7, tick = FALSE, cex.axis = axis_text)
  axis(4, at = (1-k)/(1-l), labels = FALSE, tck = -0.02)
}

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max, text_cex = 1.3)

dev.off()
