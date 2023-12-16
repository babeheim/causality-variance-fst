
rm(list = ls())

source("project_support.R")

Fst_effect <- function(theta, xbar) {
  m <- cos(theta)
  n <- sin(theta)
  xbar * n + (1 - xbar) * m
}

k <- 0.6
# note that only if k > 0.5, A is the GBT in Invisible Hand, otherwise B is the GBT in that game

ymax <- 1.1
ymin <- -3

# important theta values (going counter-clockwise); _A means A is GBT; _B means B is GBT
theta_stag_hunt_B <- atan2(-k, 0)
theta_pure_coord <- atan2(-(1-k), k) # (defines boundary for A to be GBT)
theta_stag_hunt_A <- atan2(0, k)
theta_pd_A <- atan2(k, k)
theta_hawk_dove_A <- atan2(2-k, 1-k)
theta_commensalism_A <- atan2(k, 0)
theta_invisible_hand <- atan2(k, -(1-k)) # A is GBT only if k > 0.5
theta_pure_anti_coord <- atan2((1-k), -k) # the other boundary for A as GBT
theta_commensalism_B <- atan2(0, -k)
theta_hawk_dove_B <- atan2(-k, -(1+k)) + 2 * pi
theta_pd_B <- atan2(-k, -k) + 2 * pi

stopifnot(all.equal(theta_stag_hunt_A, 0))
stopifnot(all.equal(theta_pd_A, pi/4))
stopifnot(all.equal(theta_commensalism_A, pi/2))

tikz("figures/freqDependentFstMarginals.tex", width = 6, height = 3.2)

par(mfrow = c(1, 1))

par(mar = c(4.1, 4.1, 0, 0))

# show the full spectrum, starting and ending at Pure Coordination 
xmin <- theta_pure_coord
xmax <- theta_pure_coord + 2 * pi

plot(NULL, ylim = c(ymin, ymax), xlim = c(xmin, xmax), xaxs="i", yaxs="i", frame.plot = TRUE,
  xlab = "interaction payoff structure, $\\theta$ (radians)", ylab = "effect of $F_{ST}$ on selection for $A$")

# colors for A-half
rect(theta_stag_hunt_B, ymin, theta_stag_hunt_A, ymax, border = NA, col = simple_coord_col)
rect(theta_stag_hunt_A, ymin, theta_pd_A, ymax, border = NA, col = coord_dilemma_col)
rect(theta_pd_A, ymin, theta_commensalism_A, ymax, border = NA, col = anti_coord_dilemma_col)
rect(theta_commensalism_A, ymin, theta_commensalism_B, ymax, border = NA, col = complementarity_col)

# additional colors for the B-half
rect(theta_commensalism_B, ymin, theta_pd_B, ymax, border = NA, col = anti_coord_dilemma_col)
rect(theta_pd_B, ymin, theta_stag_hunt_B + 2*pi, ymax, border = NA, col = coord_dilemma_col)
rect(theta_stag_hunt_B + 2*pi, ymin, theta_stag_hunt_A + 2*pi, ymax, border = NA, col = simple_coord_col)

# add mask for B-half
rect(theta_pure_anti_coord, ymin, theta_pure_coord + 2*pi, ymax, col = col_alpha("white", 0.7), border = NA)

axis(1, at = c(-100, atan2((1-k), -k), 100), labels = c("", "$\\theta^*$", ""))
axis(3, at = c(-100, 100), labels = c("", ""))

abline(h = 0, lty = 2)

xs <- seq(xmin, xmax, length.out = 1000)

# effect lines
points(xs, Fst_effect(xs, 1.0), col = gray(0.0), type = "l")
points(xs, Fst_effect(xs, 0.5), col = gray(0.4), type = "l")
points(xs, Fst_effect(xs, 0.0), col = gray(0.6), type = "l")

# effect line labels

text_size <- 0.9
x <- 2.8

text(x, Fst_effect(x, 1.0) + 0.15, labels = "$\\overline{x} = 1$", col = gray(0.0), cex = text_size, srt = -32)
text(x, Fst_effect(x, 0.5) - 0.22, labels = "$\\overline{x} = .5$", col = gray(0.4), cex = text_size, srt = -20)
text(x, Fst_effect(x, 0.0) - 0.21, labels = "$\\overline{x} = 0$", col = gray(0.6), cex = text_size, srt = -10)

# lines for specific games

offset <- 0.10

## lines for A-as-GBT games

lines(c(theta_pure_coord, theta_pure_coord), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_pure_coord + offset, -2.0, "Pure Coordination", srt = 90, cex = text_size)

lines(c(theta_stag_hunt_A, theta_stag_hunt_A), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_stag_hunt_A + offset, -2.4, "Stag Hunt", srt = 90, cex = text_size)

lines(c(theta_pd_A, theta_pd_A), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_pd_A + offset, -1.95, "Prisoner's Dilemma", srt = 90, cex = text_size)

lines(c(theta_hawk_dove_A, theta_hawk_dove_A), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_hawk_dove_A + offset, -2.3, "Hawk Dove", srt = 90, cex = text_size)

lines(c(theta_invisible_hand, theta_invisible_hand), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_invisible_hand + offset, -2.2, "Invisible Hand", srt = 90, cex = text_size)

# lines for B-as-GBT games

lines(c(theta_hawk_dove_B, theta_hawk_dove_B), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_hawk_dove_B + offset, -2.3, "Hawk Dove", srt = 90, cex = text_size, col = gray(0.1, 0.5))

lines(c(theta_pd_B, theta_pd_B), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_pd_B + offset, -1.95, "Prisoner's Dilemma", srt = 90, cex = text_size, col = gray(0.1, 0.5))

lines(c(theta_stag_hunt_B + 2 * pi, theta_stag_hunt_B + 2 * pi), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_stag_hunt_B + 2 * pi + offset, -2.4, "Stag Hunt", srt = 90, cex = text_size, col = gray(0.1, 0.5))

lines(c(theta_pure_coord + 2 * pi, theta_pure_coord + 2 * pi), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta_pure_coord + 2 * pi - offset, -2.0, "Pure Coordination", srt = 90, cex = text_size, col = gray(0.1, 0.5))

dev.off()

