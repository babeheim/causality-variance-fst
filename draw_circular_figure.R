
# [-] recolor to indicate A is the GBT
# second order phase diagram

rm(list = ls())

source("project_support.R")

xstar <- function(beta, r = 1) r / sqrt(1 + beta^2)

k <- 0.6 # equilibrium frequency of A
# if k > 0.5, A-eqm is better in the invisible hand game 

text_size <- 0.7

complementary_col <- col_alpha("red", 0.2)
competitive_col <- col_alpha("orange", 0.2)
coordination_col <- col_alpha("blue", 0.2)
surplus_col <- col_alpha("lightblue", 0.2)

light <- gray(0.5)



tikz("figures/freqDependentPhases.tex", height = 4, width = 4)

par(mar = c(0, 0, 0, 0))

plot(NULL, xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25), axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "")

cat("plot colored sectors\n")

# color sector II
xc <- seq(-1, 0, by = 0.001)
yc <- sqrt(1 - xc^2)
xl <- c(0, 0, -1)
yl <- c(1, 0, 0)
polygon(c(xc, xl), c(yc, yl), col = complementary_col, border = NA)

# color sector IV
xc <- seq(1, 0, by = -0.001)
yc <- -sqrt(1 - xc^2)
xl <- c(0, 0, 1)
yl <- c(-1, 0, 0)
polygon(c(xc, xl), c(yc, yl), col = coordination_col, border = NA)

# color sector I, bottom
xc <- seq(1, sqrt(2)/2, length.out = 300)
yc <- sqrt(1 - xc^2)
xl <- seq(sqrt(2)/2, 0, length.out = 10)
polygon(c(xc, xl), c(yc, xl), col = surplus_col, border = NA)

# color sector III, bottom
xc <- -seq(0, sqrt(2)/2, length.out = 300)
yc <- -sqrt(1 - xc^2)
xl <- -seq(sqrt(2)/2, 0, length.out = 10)
polygon(c(xc, xl), c(yc, xl), col = surplus_col, border = NA)

# color sector III, top
xc <- -seq(1, sqrt(2)/2, length.out = 300)
yc <- -sqrt(1 - xc^2)
xl <- -seq(sqrt(2)/2, 0, length.out = 10)
polygon(c(xc, xl), c(yc, xl), col = competitive_col, border = NA)

# color sector I, top
xc <- seq(0, sqrt(2)/2, length.out = 300)
yc <- sqrt(1 - xc^2)
xl <- seq(sqrt(2)/2, 0, length.out = 10)
polygon(c(xc, xl), c(yc, xl), col = competitive_col, border = NA)

# add mask for B-half
# slope: -(1 - k) / k
thetas <- atan2((1 - k), -k) + seq(0, pi, length.out = 100)
ys <- sin(thetas)
xs <- cos(thetas)
polygon(xs, ys, col = col_alpha("white", 0.7), border = NA)

# label radii for each named game
hd_r <- 0.75
cb_r <- 0.67
com_r <- 0.69
sh_r <- 0.75

cat("draw axis labels\n")

text(xy(pi/2, 0.79), labels = "$n \\rightarrow$", pos = 4, srt = 90, cex = text_size)
text(xy(0, 0.85), labels = "$m \\rightarrow$", pos = 1, cex = text_size)

beta <- -(1 - k) / k
pstar <- "$p^* = 1/2$"
# rline(atan(beta), 2, col = gray(0.3, 0.3))
# rline(pi + atan(beta), 2, col = gray(0.3, 0.3))
text(xy(atan(beta), 1.2), labels = pstar, srt = calc_srt(beta), cex = text_size)
text(xy(pi + atan(beta), 1.2), labels = pstar, srt = calc_srt(beta), cex = text_size)
text(xy(atan(beta) + 0.07, 0.62), labels = "Pure Coordination", srt = calc_srt(beta), cex = text_size)
rarrow(atan(beta), length = 0.1)
text(xy(pi + atan(beta) - 0.06, 0.58), labels = "Pure Anticoordination", srt = calc_srt(beta), cex = text_size)
rarrow(pi + atan(beta), length = 0.1)

beta <- 0
pstar <- "$p^* = k/2$"
text(xy(atan(beta), 1.2), labels = pstar, srt = calc_srt(beta), cex = text_size)
text(xy(pi + atan(beta), 1.2), labels = pstar, srt = calc_srt(beta), cex = text_size)
rarrow(atan(beta), length = 0.1)
text(xy(atan(beta) + 0.06, sh_r), labels = "Stag Hunt", srt = calc_srt(beta), cex = text_size)
rarrow(pi + atan(beta), col = light, length = 0.1)
text(xy(pi + atan(beta) - 0.07, com_r), labels = "Commensalism", srt = calc_srt(beta), cex = text_size, col = light)

beta <- k / (1 + k)
pstar <- "$p^* = 0$"
text(xy(atan(beta), 1.15), labels = pstar, srt = calc_srt(beta), cex = text_size)
text(xy(pi + atan(beta), 1.15), labels = pstar, srt = calc_srt(beta), cex = text_size)
text(xy(atan(beta) + 0.06, cb_r), labels = "The Void", srt = calc_srt(beta), cex = text_size)
rarrow(atan(beta), length = 0.1)
text(xy(pi + atan(beta) - 0.07, hd_r), labels = "Hawk-Dove", srt = calc_srt(beta), cex = text_size, col = light)
rarrow(pi + atan(beta), col = light, length = 0.1)

beta <- 1
text(xy(atan(beta) + 0.07, 0.6), labels = "Prisoner's Dilemma", srt = calc_srt(beta), cex = text_size)
rarrow(atan(beta), length = 0.1)
text(xy(pi + atan(beta) + 0.07, 0.6), labels = "Prisoner's Dilemma", srt = calc_srt(beta), cex = text_size, col = light)
rarrow(pi + atan(beta), col = light, length = 0.1)

beta <- (2 - k) / (1 - k)
pstar <- "$p^* = 1$"
text(xy(atan(beta), 1.15), labels = pstar, srt = calc_srt(beta), cex = text_size)
text(xy(pi + atan(beta), 1.15), labels = pstar, srt = calc_srt(beta), cex = text_size)
rarrow(atan(beta), length = 0.1)
text(xy(atan(beta) + 0.06, hd_r), labels = "Hawk-Dove", srt = calc_srt(beta), cex = text_size)
rarrow(pi + atan(beta), col = light, length = 0.1)
text(xy(pi + atan(beta) - 0.07, cb_r), labels = "The Void", srt = calc_srt(beta), cex = text_size, col = light)

beta <- Inf
pstar <- "$p^* = \\frac{k + 1}{2}$"
text(xy(atan(beta), 1.2), labels = pstar, srt = calc_srt(beta), cex = text_size)
text(xy(pi + atan(beta), 1.2), labels = pstar, srt = calc_srt(beta), cex = text_size)
text(xy(atan(beta) + 0.08, com_r), labels = "Commensalism", srt = calc_srt(beta), cex = text_size)
rarrow(atan(beta), length = 0.1)
text(xy(pi + atan(beta) - 0.07, sh_r), labels = "Stag Hunt", srt = calc_srt(beta), cex = text_size, col = light)
rarrow(pi + atan(beta), col = light, length = 0.1)

rowing_col <- ifelse(k > 0.5, light, "black")
invisible_hand_col <- ifelse(k > 0.5, "black", light)

beta <- (-k / (1 - k))
pstar <- "$p^* = k$"
text(xy(atan(beta), 1.15), labels = pstar, srt = calc_srt(beta), cex = text_size)
text(xy(pi + atan(beta), 1.15), labels = pstar, srt = calc_srt(beta), cex = text_size)
rarrow(atan(beta), length = 0.1, col = rowing_col)
text(xy(atan(beta) + 0.07, 0.8), labels = "Rowing", srt = calc_srt(beta), cex = text_size, col = rowing_col)
rarrow(pi + atan(beta), length = 0.1, col = invisible_hand_col)
text(xy(pi + atan(beta) - 0.07, 0.7), labels = "Invisible Hand", srt = calc_srt(beta), cex = text_size, col = invisible_hand_col)


dev.off()
