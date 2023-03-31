
rm(list = ls())

source("project_support.R")

# using m, n notation

vt <- 0 # arbitrary
m <- 2
k <- 0.6
n <- 10

a <- vt + (1 - k) * m
b <- vt - k * m
c <- vt + (1 - k) * n
d <- vt - k * n

v0 <- vt - k * a - (1-k) * b

# cut points:
# atan2(-(1-k), k),      # pure coordination
# atan2(0, k),           # stag hunt
# atan2(k, (1 + k)),     # Void
# atan2(k, k),           # pure common interest
# atan2((2-k), (1-k)),   # Hawk-Dove
# atan2(k, 0),           # commensalism
# atan2(k, -(1-k))       # invisible hand

tikz("figures/freqDependentFstMarginals.tex", width = 6, height = 3.2)

par(mfrow = c(1, 1))

par(mar = c(4.1, 4.1, 0, 0))

ymax <- 1.1
ymin <- -3

xmax <- max(c(pi + atan(-(k/(1-k))), pi + atan(-(1-k)/k)))
xmin <- min(c(atan(-(1-k)/k), atan(-(1-k)/k)))

xs1 <- seq(xmin, pi/4 - 0.001, length.out = 1000)
xs2 <- seq(pi/4 + 0.001, xmax, length.out = 1000)

plot(NULL, ylim = c(ymin, ymax), xlim = c(xmin, xmax), xaxs="i", yaxs="i", frame.plot = TRUE,
  xlab = "interaction payoff structure, $\\theta$ (radians)", ylab = "effect of $F_{ST}$ on selection for A")

# pure coordination to stag hunt
rect(atan2(-(1-k), k), ymin, 0, ymax, border = NA, col = coordination_col)
# stag hunt to pure common interest
rect(0, ymin, pi/4, ymax, border = NA, col = surplus_col)
# pure common interest to commensalism
rect(pi/4, ymin, pi/2, ymax, border = NA, col = competitive_col)
# commensalism to anti-coordination boundary
rect(pi/2, ymin, atan2((1-k), -k), ymax, border = NA, col = complementary_col)

abline(h = 0, lty = 2)

Fst_effect <- function(theta, xbar) -(sin(theta)-cos(theta)) * (xbar - (1 - cos(theta) / (cos(theta) - sin(theta))))

xs <- c(xs1, xs2)

points(xs, Fst_effect(xs, 1.0), col = gray(0.0), type = "l")
points(xs, Fst_effect(xs, 0.5), col = gray(0.4), type = "l")
points(xs, Fst_effect(xs, 0.0), col = gray(0.6), type = "l")

text_size <- 0.9

offset <- 0.07

x <- 1.75
text(x, -(sin(x)-cos(x)) * (0.0 - (1 - cos(x) / (cos(x) - sin(x)))) - 0.16, labels = "$\\overline{x} = 0$", col = gray(0.6), cex = text_size)
text(x, -(sin(x)-cos(x)) * (0.5 - (1 - cos(x) / (cos(x) - sin(x)))) - 0.20, labels = "$\\overline{x} = .5$", col = gray(0.4), cex = text_size)
text(x, -(sin(x)-cos(x)) * (1.0 - (1 - cos(x) / (cos(x) - sin(x)))) - 0.25, labels = "$\\overline{x} = 1$", col = gray(0.0), cex = text_size)

theta <- atan2(-(1-k), k)
lines(c(theta, theta), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta + offset, -2.0, "Pure Coordination", srt = 90, cex = text_size)

theta <- 0
lines(c(theta, theta), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta + offset, -2.4, "Stag Hunt", srt = 90, cex = text_size)

theta <- pi/4
lines(c(theta, theta), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta + offset, -1.95, "Prisoner's Dilemma", srt = 90, cex = text_size)

theta <- atan2(2-k, 1-k)
lines(c(theta, theta), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta + offset, -2.3, "Hawk Dove", srt = 90, cex = text_size)

theta <- atan2(k, -(1-k))
lines(c(theta, theta), c(ymin, ymax), col = gray(0.3, 0.6))
text(theta + offset, -2.2, "Invisible Hand", srt = 90, cex = text_size)

dev.off()

