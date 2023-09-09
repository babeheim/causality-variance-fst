
rm(list = ls())

source("project_support.R")

Fst_effect <- function(theta, xbar) {
  m <- cos(theta)
  n <- sin(theta)
  xbar * n + (1 - xbar) * m
}

k <- 0.6
# note that only if k > 0.5, A is the GBT in Invisible Hand, otherwise B is the GBT in that game

# cut points:

theta_pure_coord <- atan2(-(1-k), k) # boundary for A to be GBT

theta_stag_hunt <- atan2(0, k)
stopifnot(all.equal(theta_stag_hunt, 0))

theta_pd <- atan2(k, k)
stopifnot(all.equal(theta_pd, pi/4))

theta_hawk_dove <- atan2(2-k, 1-k)

theta_commensalism <- atan2(k, 0)
stopifnot(all.equal(theta_commensalism, pi/2))

theta_invisible_hand <- atan2(k, -(1-k))

theta_pure_anti_coord <- atan2((1-k), -k) # the other boundary for A as GBT

tikz("figures/freqDependentFstMarginals.tex", width = 6, height = 3.2)

par(mfrow = c(1, 1))

par(mar = c(4.1, 4.1, 0, 0))

ymax <- 1.1
ymin <- -3

xmax <- theta_pure_anti_coord
xmin <- theta_pure_coord

plot(NULL, ylim = c(ymin, ymax), xlim = c(theta_pure_coord, theta_pure_anti_coord), xaxs="i", yaxs="i", frame.plot = TRUE,
  xlab = "interaction payoff structure, $\\theta$ (radians)", ylab = "effect of $F_{ST}$ on selection for $A$")

# pure coord to stag hunt
rect(theta_pure_coord, ymin, theta_stag_hunt, ymax, border = NA, col = simple_coord_col)
# stag hunt to prisoner's dilemma
rect(theta_stag_hunt, ymin, theta_pd, ymax, border = NA, col = coord_dilemma_col)
# prisoner's dilemma to commensalism
rect(theta_pd, ymin, theta_commensalism, ymax, border = NA, col = anti_coord_dilemma_col)
# commensalism to pure anti-coord
rect(theta_commensalism, ymin, theta_pure_anti_coord, ymax, border = NA, col = complementarity_col)

abline(h = 0, lty = 2)

xs1 <- seq(theta_pure_coord, theta_pd - 0.001, length.out = 1000)
xs2 <- seq(theta_pd + 0.001, theta_pure_anti_coord, length.out = 1000)
xs <- c(xs1, xs2)

text_size <- 0.9
offset <- 0.07
x <- 1.75


points(xs, Fst_effect(xs, 1.0), col = gray(0.0), type = "l")
text(x, Fst_effect(x, 1.0) - 0.16, labels = "$\\overline{x} = 1$", col = gray(0.0), cex = text_size)

points(xs, Fst_effect(xs, 0.5), col = gray(0.4), type = "l")
text(x, Fst_effect(x, 0.5) - 0.20, labels = "$\\overline{x} = .5$", col = gray(0.4), cex = text_size)

points(xs, Fst_effect(xs, 0.0), col = gray(0.6), type = "l")
text(x, Fst_effect(x, 0.0) - 0.25, labels = "$\\overline{x} = 0$", col = gray(0.6), cex = text_size)

lines(c(theta_pure_coord, theta_pure_coord), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_pure_coord + offset, -2.0, "Pure Coordination", srt = 90, cex = text_size)

lines(c(theta_stag_hunt, theta_stag_hunt), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_stag_hunt + offset, -2.4, "Stag Hunt", srt = 90, cex = text_size)

lines(c(theta_pd, theta_pd), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_pd + offset, -1.95, "Prisoner's Dilemma", srt = 90, cex = text_size)

lines(c(theta_hawk_dove, theta_hawk_dove), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_hawk_dove + offset, -2.3, "Hawk Dove", srt = 90, cex = text_size)

lines(c(theta_invisible_hand, theta_invisible_hand), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_invisible_hand + offset, -2.2, "Invisible Hand", srt = 90, cex = text_size)

dev.off()

