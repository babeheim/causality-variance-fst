
rm(list = ls())

source("project_support.R")

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

# add scale

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()




tikz("figures/stagHuntContourDiagram.tex", height = 4, width = 4)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

# Stag Hunt

land_scale_max <- 1
water_scale_min <- -0.4

k <- 0.6
m <- 5
n <- 0
phi <- n/(n-m)

plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)

# add land/water boundary 

xs <- seq(0, 1, by = 0.01)
ys <- (xs - k)/((xs + phi) - 1)
ys[ys == Inf | ys < -0.01] <- -0.01
land_x <- c(0, xs, 1)
land_y <- c(1, ys, 1)
polygon(land_x, land_y, col = land_colors[1], border = NA)

ys <- (xs - k)/((xs + phi) - 1)
ys[ys == Inf | ys > 1.1] <- 1.1
water_x <- c(0, xs, 1)
water_y <- c(0, ys, 0)
polygon(water_x, water_y, col = water_colors[length(water_colors)], border = NA)

# now show various levels

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * seq(0.01, 0.3, length.out = 20)

for (i in 1:length(land_levels)) {
  level <- land_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == Inf | ys > 1.1] <- 1.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

for (i in 1:length(water_levels)) {
  level <- water_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

ys <- (xs - k)/((xs + phi) - 1)
points(xs, ys, col = col_alpha("black", 0.3), type = "l")

mtext("\\textit{Stag Hunt}", side = 3, line = 0.3)

mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = 1.5)
axis(2, at = c(0, k, 1), labels = c("0", "$k$", "1"), las = 1, cex.axis = 1.5)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# add scale

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()



tikz("figures/hawkDoveContourDiagram.tex", height = 4, width = 4)

# Hawk Dove

land_scale_max <- 1.0
water_scale_min <- -0.1

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

k <- 0.6
m <- 1
n <- m * (2-k)/(1-k)
phi <- n/(n-m)

plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)

# add land/water boundary 

xs <- seq(0, 1, by = 0.01)
ys <- (xs - k)/((xs + phi) - 1)
ys[ys == Inf | ys < -0.01] <- -0.01
land_x <- c(0, xs, 1)
land_y <- c(1, ys, 1)
polygon(land_x, land_y, col = land_colors[1], border = NA)

ys <- (xs - k)/((xs + phi) - 1)
ys[ys == Inf | ys > 1.1] <- 1.1
water_x <- c(0, xs, 1)
water_y <- c(0, ys, 0)
polygon(water_x, water_y, col = water_colors[length(water_colors)], border = NA)

# now show various levels

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * seq(0.01, 0.1, length.out = 20)

for (i in 1:length(land_levels)) {
  level <- land_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == Inf | ys > 1.1] <- 1.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

for (i in 1:length(water_levels)) {
  level <- water_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

curve((x - k)/((x + phi) - 1), add = TRUE, col = col_alpha("black", 0.3))

mtext("\\textit{Hawk Dove}", side = 3, line = 0.3)

mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = 1.5)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = 1.5)
axis(4, at = (1-k)/phi, labels = "$\\frac{1-k}{2-k}$", las = 1, line = -0.7, tick = FALSE, cex.axis = 1.5)
axis(4, at = (1-k)/phi, labels = FALSE, tck = -0.02)

x <- seq(0, 1, by = 0.01)
points(x, (x - k)/((x + phi) - 1), type = "l", col = zero_line_col)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# add scale

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()



tikz("figures/pureCoordinationContourDiagram.tex", height = 4, width = 4)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

# Pure Coordination

land_scale_max <- 1
water_scale_min <- -1.0

k <- 0.6
m <- 10
n <- -m * (1-k)/k

phi <- n/(n-m)

plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)
# xaxs sets the interval calculation for the x-axis, "r" extends range by 4 percent at each end and is the default
# "i" uses the internal original data range

# add land/water boundary 

water_x <- c(0, k, k, 0)
water_y <- c(0, 0, 1, 1)
polygon(water_x, water_y, col = water_colors[length(water_colors)], border = NA)

land_x <- c(1, k, k, 1)
land_y <- c(0, 0, 1, 1)
polygon(land_x, land_y, col = land_colors[1], border = NA)

# now show various levels

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

xs <- seq(k, 1, 0.01)

for (i in 1:length(land_levels)) {
  level <- land_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys > 100 | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

xs <- seq(0, k, 0.01)

for (i in 1:length(water_levels)) {
  level <- water_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

mtext("\\textit{Pure Coordination}", side = 3, line = 0.3)
mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = 1.5)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = 1.5)
abline(v = k, col = zero_line_col)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# add scale

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()




tikz("figures/invisibleHandContourDiagram.tex", height = 4, width = 4)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

# Invisible Hand

land_scale_max <- 1
water_scale_min <- -0.5

k <- 0.6

m <- (-5)
n <- -m * k/(1-k)
phi <- n/(n-m)

plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)

# add land/water boundary 

xs <- seq(k, 1, by = 0.01)
ys <- (xs - k)/((xs + phi) - 1)
land_x <- c(0, 0, xs, 1)
land_y <- c(1, 0, ys, 1)
polygon(land_x, land_y, col = land_colors[1], border = NA)

water_x <- c(1, xs)
water_y <- c(0, ys)
polygon(water_x, water_y, col = water_colors[length(water_colors)], border = NA)

# now show various levels

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * seq(0.01, 0.7, length.out = 20)

xs <- seq(0, 1, 0.001)

for (i in 1:length(land_levels)) {
  level <- land_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  ys[ys == Inf | ys > 1.1] <- 1.1
  # try combining them into one big polygon
  level_xs <- c(xs[xs < (1-phi)], rev(xs[xs > (1-phi)]))
  level_ys <- c(ys[xs < (1-phi)], rev(ys[xs > (1-phi)]))
  combined_works <- !any(abs(diff(level_ys)) > 1.0)
  if (combined_works) {
    # weird monotonic edge case
    not_monotonic <- !(
      abs(ys[which(xs == max(xs[xs < (1-phi)]))] -
      ys[which(xs == min(xs[xs > (1-phi)]))]) < 0.05
    )
    if (not_monotonic) {
      polygon(level_xs, level_ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
    }
  } else {
    # plot two distinct polygons
    polygon(xs[xs < (1-phi)], ys[xs < (1-phi)], border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
    polygon(xs[xs > (1-phi)], ys[xs > (1-phi)], border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
  }
}

xs <- seq(k, 1, 0.01)

for (i in 1:length(water_levels)) {
  level <- water_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

mtext("\\textit{Invisible Hand}", side = 3, line = 0.3)

mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, 1), labels = c("0", "1"), cex.axis = 1.5)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = 1.5)
axis(1, at = c(-m/(n-m), k), labels = c("$1-k$", "$k$"), cex.axis = 1.0)

if (k == 0.5) {
  abline(v = k, col = zero_line_col)
} else if (k < 0.5) {
  x <- seq(0, 1, by = 0.01)
  points(x, (x - k)/((x + phi) - 1), type = "l", col = zero_line_col)
  axis(2, at = k/(1-phi), labels = "$\\frac{k}{1 - k}", las = 1, line = -0.7, tick = FALSE, cex.axis = 1.5)
  axis(2, at = k/(1-phi), labels = FALSE, tck = -0.01)
} else {
  x <- seq(0, 1, by = 0.01)
  points(x, (x - k)/((x + phi) - 1), type = "l", col = zero_line_col)
  axis(4, at = (1-k)/phi, labels = "$\\frac{1-k}{k}$", las = 1, line = -0.7, tick = FALSE, cex.axis = 1.5)
  axis(4, at = (1-k)/phi, labels = FALSE, tck = -0.02)
}

abline(v = -m/(n-m), lty = 2, col = col_alpha("white", 0.8))

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# add scale

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()


# it would be extremely convenient to produce a script that can make *arbitrary* phase diagrams, given a particular angle theta. im not that far away TBH

tikz("figures/impureCoordinationContourDiagram.tex", height = 4, width = 4)

layout(mat = matrix(c(0, 1, 2), 1, 3, byrow = TRUE), widths = c(1,5,1), heights = c(5, 5, 5), respect = TRUE)

par(mar = c(3, 3, 3, 2))

# Impure Coordination

land_scale_max <- 1
water_scale_min <- -0.5

k <- 0.6

# when theta = 0, its a Stag Hunt
# when theta = atan2(y = -(1-k), x = k), its a pure coordinatino game
# so it just has to be between these values!
theta <- -pi/8
stopifnot(theta < 0)
stopifnot(theta > atan2(y = -(1-k), x = k))

m <- cos(theta) * 8 
n <- sin(theta) * 8

phi <- n/(n-m)

plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = FALSE)

# add land/water boundary 

xs <- seq(0, 1, by = 0.01)
ys <- (xs - k)/((xs + phi) - 1)
ys[ys == Inf | ys < -0.01] <- -0.01
land_x <- c(0, xs, 1)
land_y <- c(1, ys, 1)
polygon(land_x, land_y, col = land_colors[1], border = NA)

xs <- seq(0, k, by = 0.01)
ys <- (xs - k)/((xs + phi) - 1)
ys[ys == Inf | ys > 1.1] <- 1.1
water_x <- c(0, xs)
water_y <- c(0, ys)
polygon(water_x, water_y, col = water_colors[length(water_colors)], border = NA)

# now show various levels

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * seq(0.01, 0.7, length.out = 20)

xs <- seq(0, 1, 0.001)

for (i in 1:length(land_levels)) {
  level <- land_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  ys[ys == Inf | ys > 1.1] <- 1.1
  # try combining them into one big polygon
  level_xs <- c(xs[xs < (1-phi)], rev(xs[xs > (1-phi)]))
  level_ys <- c(ys[xs < (1-phi)], rev(ys[xs > (1-phi)]))
  combined_works <- !any(abs(diff(level_ys)) > 1.0)
  if (combined_works) {
    # weird monotonic edge case
    not_monotonic <- !(
      abs(ys[which(xs == max(xs[xs < (1-phi)]))] -
      ys[which(xs == min(xs[xs > (1-phi)]))]) < 0.05
    )
    if (not_monotonic) {
      polygon(level_xs, level_ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
    }
  } else {
    # plot two distinct polygons
    polygon(xs[xs < (1-phi)], ys[xs < (1-phi)], border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
    polygon(xs[xs > (1-phi)], ys[xs > (1-phi)], border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
  }
}

xs <- seq(0, k, 0.01)

for (i in 1:length(water_levels)) {
  level <- water_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

mtext("Impure Coordination", side = 3, line = 0.3)

mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, 1), labels = c("0", "1"), cex.axis = 1.5)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = 1.5)
axis(1, at = k, labels = "$k$", cex.axis = 1.5)

if (k == 0.5) {
  abline(v = k, col = zero_line_col)
} else if (k > 0.5) {
  x <- seq(0, k, by = 0.01)
  points(x, (x - k)/((x + phi) - 1), type = "l", col = zero_line_col)
  axis(2, at = k/(1-phi), labels = "$\\displaystyle\\frac{k}{\\ell}$", las = 1, cex.axis = 1.5)
} else {
  x <- seq(0, k, by = 0.01)
  points(x, (x - k)/((x + phi) - 1), type = "l", col = zero_line_col)
  axis(4, at = (1-k)/phi, labels = "$\\frac{1-k}{k}$", las = 1, line = -0.7, tick = FALSE, cex.axis = 1.5)
  axis(4, at = (1-k)/phi, labels = FALSE, tck = -0.02)
}

abline(v = -m/(n-m), lty = 2, col = col_alpha("white", 0.8))
# corresponds to pbar = $1-\\varphi$

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# add scale

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max)

dev.off()




# # par("csi") is height of (default-sized) characters in inches.

# d <- 4
# d2 <- d*d
# layout.mat <- matrix(1:d2, byrow=TRUE, ncol=d) # plot matrix
# layout.mat <- cbind(layout.mat, rep(0, d)) # space
# layout.mat <- cbind(layout.mat, rep(d2+1, d)) # column on the right side
# layout.mat <- rbind(c(rep(18,d),0,0),layout.mat) #Add row on top
# wspace <- 6*par("csi")*2.54 # width of the space in character height in cm
# wside <- 3*par("csi")*2.54 # width of the right side in character height in cm

# # lcm = specifies absolute dimensions in centimeters for widths and heights
# #Note adjustments to heights
# layout(layout.mat, respect = TRUE,
#   widths=c(rep(1, d), lcm(wspace), lcm(wside)),
#   heights = c(0.25,rep(1,nrow(layout.mat)-1))
# )
# layout.show(d2+1)

# # The ‘respect’ argument controls whether a unit column-width is the
# #    same physical measurement on the device as a unit row-height.

# par(mar=rep(0, 4), oma=c(4,4,6,4))

# for(i in 1:d){
#     for(j in 1:d){
#         plot.new()
#         plot.window(xlim=c(0,1), ylim=c(0,1))
#         ll <- par("usr")
#         rect(ll[1], ll[3], ll[2], ll[4])
#         text(0.5, 0.5, paste("i=",i,", j=",j,sep=""), cex=1.4)
#     }
# }
# plot.new()
# plot.window(xlim=c(0,1), ylim=c(0,1))
# ll <- par("usr")
# rect(ll[1], ll[3], ll[2], ll[4])
# text(0.5, 0.5, "side", cex=1.4)

# ## title
# plot.new()
# plot.window(xlim=c(0,1), ylim=c(0,1))
# ll <- par("usr")
# rect(ll[1], ll[3], ll[2], ll[4])
# text(0.5, 0.5, "top", cex=1.4)





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
phi <- n/(n-m)

plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)

# add land/water boundary 

xs <- seq(0, 1, by = 0.01)
ys <- (xs - k)/((xs + phi) - 1)
ys[ys == Inf | ys < -0.01] <- -0.01
land_x <- c(0, xs, 1)
land_y <- c(1, ys, 1)
polygon(land_x, land_y, col = land_colors[1], border = NA)

ys <- (xs - k)/((xs + phi) - 1)
ys[ys == Inf | ys > 1.1] <- 1.1
water_x <- c(0, xs, 1)
water_y <- c(0, ys, 0)
polygon(water_x, water_y, col = water_colors[length(water_colors)], border = NA)

# now show various levels

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

for (i in 1:length(land_levels)) {
  level <- land_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == Inf | ys > 1.1] <- 1.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

for (i in 1:length(water_levels)) {
  level <- water_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

ys <- (xs - k)/((xs + phi) - 1)
points(xs, ys, col = col_alpha("black", 0.3), type = "l")

mtext("\\textit{Stag Hunt}", side = 3, line = 0.3, cex = 0.9)
mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = axis_text)
axis(2, at = c(0, k, 1), labels = c("0", "$k$", "1"), las = 1, cex.axis = axis_text)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# Hawk-Dove

m <- 1
n <- m * (2-k)/(1-k)
phi <- n/(n-m)

plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)

# add land/water boundary 

xs <- seq(0, 1, by = 0.01)
ys <- (xs - k)/((xs + phi) - 1)
ys[ys == Inf | ys < -0.01] <- -0.01
land_x <- c(0, xs, 1)
land_y <- c(1, ys, 1)
polygon(land_x, land_y, col = land_colors[1], border = NA)

ys <- (xs - k)/((xs + phi) - 1)
ys[ys == Inf | ys > 1.1] <- 1.1
water_x <- c(0, xs, 1)
water_y <- c(0, ys, 0)
polygon(water_x, water_y, col = water_colors[length(water_colors)], border = NA)

# now show various levels

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

for (i in 1:length(land_levels)) {
  level <- land_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == Inf | ys > 1.1] <- 1.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

for (i in 1:length(water_levels)) {
  level <- water_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

curve((x - k)/((x + phi) - 1), add = TRUE, col = col_alpha("black", 0.3))

mtext("\\textit{Hawk Dove}", side = 3, line = 0.3, cex = 0.9)

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"), cex.axis = axis_text)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = axis_text)
axis(4, at = (1-k)/phi, labels = "$\\frac{1-k}{2-k}$", las = 1, line = -0.7, tick = FALSE, cex.axis = axis_text)
axis(4, at = (1-k)/phi, labels = FALSE, tck = -0.02)

x <- seq(0, 1, by = 0.01)
points(x, (x - k)/((x + phi) - 1), type = "l", col = zero_line_col)

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

# Pure Coordination

m <- 10
n <- -m * (1-k)/k
phi <- n/(n-m)

plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)
# xaxs sets the interval calculation for the x-axis, "r" extends range by 4 percent at each end and is the default
# "i" uses the internal original data range

# add land/water boundary 

water_x <- c(0, k, k, 0)
water_y <- c(0, 0, 1, 1)
polygon(water_x, water_y, col = water_colors[length(water_colors)], border = NA)

land_x <- c(1, k, k, 1)
land_y <- c(0, 0, 1, 1)
polygon(land_x, land_y, col = land_colors[1], border = NA)

# now show various levels

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

xs <- seq(k, 1, 0.01)

for (i in 1:length(land_levels)) {
  level <- land_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys > 100 | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

xs <- seq(0, k, 0.01)

for (i in 1:length(water_levels)) {
  level <- water_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

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
phi <- n/(n-m)

plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)

# add land/water boundary 

xs <- seq(k, 1, by = 0.01)
ys <- (xs - k)/((xs + phi) - 1)
land_x <- c(0, 0, xs, 1)
land_y <- c(1, 0, ys, 1)
polygon(land_x, land_y, col = land_colors[1], border = NA)

water_x <- c(1, xs)
water_y <- c(0, ys)
polygon(water_x, water_y, col = water_colors[length(water_colors)], border = NA)

# now show various levels

land_levels <- c(0.01, 0.05, seq(0.1, 1, length.out = 20))
water_levels <- (-1) * land_levels

xs <- seq(0, 1, 0.001)

for (i in 1:length(land_levels)) {
  level <- land_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  ys[ys == Inf | ys > 1.1] <- 1.1
  # try combining them into one big polygon
  level_xs <- c(xs[xs < (1-phi)], rev(xs[xs > (1-phi)]))
  level_ys <- c(ys[xs < (1-phi)], rev(ys[xs > (1-phi)]))
  combined_works <- !any(abs(diff(level_ys)) > 1.0)
  if (combined_works) {
    # weird monotonic edge case
    not_monotonic <- !(
      abs(ys[which(xs == max(xs[xs < (1-phi)]))] -
      ys[which(xs == min(xs[xs > (1-phi)]))]) < 0.05
    )
    if (not_monotonic) {
      polygon(level_xs, level_ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
    }
  } else {
    # plot two distinct polygons
    polygon(xs[xs < (1-phi)], ys[xs < (1-phi)], border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
    polygon(xs[xs > (1-phi)], ys[xs > (1-phi)], border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
  }
}

xs <- seq(k, 1, 0.01)

for (i in 1:length(water_levels)) {
  level <- water_levels[i]
  ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((1 - phi - xs))
  ys[ys == -Inf | ys < -0.1] <- -0.1
  polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
}

mtext("\\textit{Invisible Hand}", side = 3, line = 0.3, cex = 0.9)

mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)

axis(1, at = c(0, 1), labels = c("0", "1"), cex.axis = axis_text)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1, cex.axis = axis_text)
axis(1, at = c(-m/(n-m), k), labels = c("$1-k$", "$k$"), cex.axis = axis_text)

if (k == 0.5) {
  abline(v = k, col = zero_line_col)
} else if (k < 0.5) {
  x <- seq(0, 1, by = 0.01)
  points(x, (x - k)/((x + phi) - 1), type = "l", col = zero_line_col)
  axis(2, at = k/(1-phi), labels = "D", las = 1)
} else {
  x <- seq(k, 1, by = 0.01)
  points(x, (x - k)/((x + phi) - 1), type = "l", col = zero_line_col)
  axis(4, at = (1-k)/phi, labels = "$\\frac{1-k}{k}$", las = 1, line = -0.7, tick = FALSE, cex.axis = axis_text)
  axis(4, at = (1-k)/phi, labels = FALSE, tck = -0.02)
}

abline(v = -m/(n-m), lty = 2, col = col_alpha("white", 0.8))

polygon(c(0, 0, 1, 1), c(0, 1, 1, 0))

add_vertical_scale(scale_min = water_scale_min, scale_max = land_scale_max, text_cex = 1.3)

dev.off()


