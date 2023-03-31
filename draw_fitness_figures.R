
rm(list = ls())

source("project_support.R")

k <- 0.6 # equilibrium p-tilde
# best to set at 0.5

text_size <- 0.7

xlabs <- "group $j$ frequency of \\textcolor[HTML]{00AFB9}{A}, $p_j$"
ylabs <- "individual fitness, $w_{i,j}$"



tikz("figures/prisonersDilemmaPayoffs.tex", height = 3, width = 3)

par(mar = c(5.1, 4.1, 4.1, 2.1))

b <- 1
c <- 0.2

w0 <- 0 # anchor point for Defect when p = 0
w_min <- w0 - c
w_max <- w0 + b

plot(NULL, axes = FALSE, xlab = xlabs, ylab = ylabs, xlim = c(0, 1), ylim = c(w_min, w_max), xaxs="i", yaxs="i", main = "Prisoner's Dilemma")
lines(c(0, 1), c(w0 - c, w0 + b - c), col = A_col)
lines(c(0, 1), c(w0, w0 + b), col = B_col)

axis(1, at = c(0, 1))
axis(2, at = c(-1000, 1000), labels = FALSE)

mtext("$c$", side = 2, line = 1.0, at = w0 - c/2, las = 1)
mtext("$\\overbrace{\\hspace{0.6cm}}$", side = 2, line = 0.1, at = w0 - c/2)

mtext("$b$", side = 4, line = 1.0, at = w0 + b/2 - c, las = 1)
mtext("$\\underbrace{\\hspace{3cm}}$", side = 4, line = -0.5, at = w0 + b/2 - c)

dev.off()



tikz("figures/freqDependentPayoffs.tex", height = 3, width = 3)

par(mar = c(5.1, 4.1, 4.1, 2.1))

k <- 0.6
n <- 1 
m <- -n * (1 - k) / k
wt <- 0 + m * k # where w0 = 0

a <- wt + (1 - k) * m
b <- wt - k * m
c <- wt + (1 - k) * n
d <- wt - k * n

plot(NULL, axes = FALSE, xlim = c(0, 1), ylim = c(d, c), xlab = xlabs, ylab = ylabs, xaxs = "i", yaxs = "i")
lines(c(0, 1), c(b, a), col = A_col)
lines(c(0, 1), c(d, c), col = B_col)

lines(c(k, k), c(d, wt), lty = 2, col = col_alpha("black", 0.3))
lines(c(0, 1), c(wt, wt), lty =2, col = col_alpha("black", 0.3))

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"))
axis(2, at = c(-1000, wt, 1000), labels = c(NA, "$\\tilde{w}$", NA), tick = TRUE, las = 1)
points(k, wt, cex = 1, pch = 16)

mtext("$kn$", side = 2, line = 1, at = wt - k*n/2, las = 0)
mtext("$\\overbrace{\\hspace{2cm}}$", side = 2, line = 0, at = wt - k*n/2)

mtext("$(1-k)m$", side = 4, line = 0.5, at = wt + (1-k)*m/2, las = 0)
mtext("$\\underbrace{\\hspace{1.0cm}}$", side = 4, line = -0.7, at = wt + (1-k)*m/2)

text(0.25, wt + (0.25 - k) * n + 0.05, labels = "$x_{i,j}=0$", col = B_col, srt = atan(0.75 * n) * 360/(2*pi))

text(0.25, wt + (0.25 - k) * m + 0.05, labels = "$x_{i,j}=1$", col = A_col, srt = atan(0.75 * m) * 360/(2*pi))

dev.off()




tikz("figures/stagHuntPayoffs.tex", height = 3, width = 3)

par(mar = c(5.1, 4.1, 4.1, 2.1))

m <- 1
n <- 0
wt <- 0 + m * k # where w0 = 0

a <- wt + (1 - k) * m
b <- wt - k * m
c <- wt + (1 - k) * n
d <- wt - k * n

plot(NULL, axes = FALSE, xlim = c(0, 1), ylim = c(b, a), main = "Stag Hunt", xlab = xlabs, ylab = ylabs, xaxs = "i", yaxs = "i")
lines(c(0, 1), c(b, a), col = A_col)
lines(c(0, 1), c(d, c), col = B_col)

lines(c(k, k), c(b, wt), lty = 2, col = col_alpha("black", 0.3))

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"))
axis(2, at = c(-1000, wt, 1000), labels = c(NA, "$\\tilde{w}$", NA), tick = TRUE, las = 1)
points(k, wt, cex = 1, pch = 16)

mtext("$km$", side = 2, line = 1, at = wt - k*m/2, las = 0)
mtext("$\\overbrace{\\hspace{2cm}}$", side = 2, line = 0, at = wt - k*m/2)

mtext("$(1-k)m$", side = 4, line = 0.5, at = wt + (1-k)*m/2, las = 0)
mtext("$\\underbrace{\\hspace{1.5cm}}$", side = 4, line = -0.7, at = wt + (1-k)*m/2)

text(0.25, wt + (0.25 - k) * n + 0.05, labels = "$x_{i,j}=0$", col = B_col, srt = atan(0.75 * n) * 360/(2*pi))

text(0.25, wt + (0.25 - k) * m + 0.05, labels = "$x_{i,j}=1$", col = A_col, srt = atan(0.75 * m) * 360/(2*pi))

dev.off()



tikz("figures/hawkDovePayoffs.tex", height = 3, width = 3)

par(mar = c(5.1, 4.1, 4.1, 2.1))

k <- 0.6
n <- 1
m <- n * (1 - k) / (2 - k)
wt <- 0 + m * k # where w0 = 0

a <- wt + (1 - k) * m
b <- wt - k * m
c <- wt + (1 - k) * n
d <- wt - k * n

plot(NULL, axes = FALSE, xlim = c(0, 1), ylim = c(d, c), main = "Hawk Dove", xlab = xlabs, ylab = ylabs, xaxs = "i", yaxs = "i")
lines(c(0, 1), c(b, a), col = A_col)
lines(c(0, 1), c(d, c), col = B_col)

lines(c(k, k), c(d, wt), lty = 2, col = col_alpha("black", 0.3))
lines(c(0, 1), c(wt, wt), lty =2, col = col_alpha("black", 0.3))

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"))
axis(2, at = c(-1000, wt, 1000), labels = c(NA, "$\\tilde{w}$", NA), tick = TRUE, las = 1)
points(k, wt, cex = 1, pch = 16)

mtext("$kn$", side = 2, line = 1, at = wt - k*n/2, las = 0)
mtext("$\\overbrace{\\hspace{2cm}}$", side = 2, line = 0, at = wt - k*n/2)

mtext("$(1-k)m$", side = 4, line = 0.5, at = wt + (1-k)*m/2, las = 0)
mtext("$\\underbrace{\\hspace{0.1cm}}$", side = 4, line = -0.7, at = wt + (1-k)*m/2)

text(0.25, wt + (0.25 - k) * n - 0.06, labels = "$x_{i,j}=0$", col = B_col, srt = atan(0.75 * n) * 360/(2*pi))

text(0.25, wt + (0.25 - k) * m - 0.05, labels = "$x_{i,j}=1$", col = A_col, srt = atan(0.75 * m) * 360/(2*pi))

dev.off()



tikz("figures/pureCoordinationPayoffs.tex", height = 3, width = 3)

par(mar = c(5.1, 4.1, 4.1, 2.1))

k <- 0.6
m <- 1
n <- -m * (1 - k) / k
wt <- 0 + m * k # where w0 = 0

a <- wt + (1 - k) * m
b <- wt - k * m
c <- wt + (1 - k) * n
d <- wt - k * n

plot(NULL, axes = FALSE, xlim = c(0, 1), ylim = c(b, a), main = "Pure Coordination", xlab = xlabs, ylab = ylabs, xaxs = "i", yaxs = "i")
lines(c(0, 1), c(b, a), col = A_col)
lines(c(0, 1), c(d, c), col = B_col)

lines(c(k, k), c(b, wt), lty = 2, col = col_alpha("black", 0.3))
lines(c(0, 1), c(wt, wt), lty =2, col = col_alpha("black", 0.3))

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"))
axis(2, at = c(-1000, wt, 1000), labels = c(NA, "$\\tilde{w}$", NA), tick = TRUE, las = 1)
points(k, wt, cex = 1, pch = 16)

mtext("$km$", side = 2, line = 1, at = wt - k*m/2, las = 0)
mtext("$\\overbrace{\\hspace{2cm}}$", side = 2, line = 0, at = wt - k*m/2)

mtext("$(1-k)n$", side = 4, line = 0.5, at = wt + (1-k)*n/2, las = 0)
mtext("$\\underbrace{\\hspace{1.0cm}}$", side = 4, line = -0.7, at = wt + (1-k)*n/2)

text(0.25, wt + (0.25 - k) * n + 0.07, labels = "$x_{i,j}=0$", col = B_col, srt = atan(0.75 * n) * 360/(2*pi))

text(0.25, wt + (0.25 - k) * m + 0.07, labels = "$x_{i,j}=1$", col = A_col, srt = atan(0.75 * m) * 360/(2*pi))

dev.off()



tikz("figures/invisibleHandPayoffs.tex", height = 3, width = 3)

par(mar = c(5.1, 4.1, 4.1, 2.1))

k <- 0.6
n <- 1 
m <- -n * (1 - k) / k
wt <- 0 + m * k # where w0 = 0

a <- wt + (1 - k) * m
b <- wt - k * m
c <- wt + (1 - k) * n
d <- wt - k * n

plot(NULL, axes = FALSE, xlim = c(0, 1), ylim = c(d, c), main = "Invisible Hand", xlab = xlabs, ylab = ylabs, xaxs = "i", yaxs = "i")
lines(c(0, 1), c(b, a), col = A_col)
lines(c(0, 1), c(d, c), col = B_col)

lines(c(k, k), c(d, wt), lty = 2, col = col_alpha("black", 0.3))
lines(c(0, 1), c(wt, wt), lty =2, col = col_alpha("black", 0.3))

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"))
axis(2, at = c(-1000, wt, 1000), labels = c(NA, "$\\tilde{w}$", NA), tick = TRUE, las = 1)
points(k, wt, cex = 1, pch = 16)

mtext("$kn$", side = 2, line = 1, at = wt - k*n/2, las = 0)
mtext("$\\overbrace{\\hspace{2cm}}$", side = 2, line = 0, at = wt - k*n/2)

mtext("$(1-k)m$", side = 4, line = 0.5, at = wt + (1-k)*m/2, las = 0)
mtext("$\\underbrace{\\hspace{1.0cm}}$", side = 4, line = -0.7, at = wt + (1-k)*m/2)

text(0.25, wt + (0.25 - k) * n + 0.05, labels = "$x_{i,j}=0$", col = B_col, srt = atan(0.75 * n) * 360/(2*pi))

text(0.25, wt + (0.25 - k) * m + 0.05, labels = "$x_{i,j}=1$", col = A_col, srt = atan(0.75 * m) * 360/(2*pi))

dev.off()



tikz("figures/freqDependentModel.tex", height = 6/2, width = 6)

par(mfrow = c(1, 2))

par(mar = c(5.1, 4.1, 4.1, 2.1))

k <- 0.6
n <- 1 
m <- -n * (1 - k) / k
wt <- 0 + m * k # where w0 = 0

a <- wt + (1 - k) * m
b <- wt - k * m
c <- wt + (1 - k) * n
d <- wt - k * n

plot(NULL, axes = FALSE, xlim = c(0, 1), ylim = c(d, c), xlab = xlabs, ylab = ylabs, xaxs = "i", yaxs = "i")
lines(c(0, 1), c(b, a), col = A_col)
lines(c(0, 1), c(d, c), col = B_col)

lines(c(k, k), c(d, wt), lty = 2, col = col_alpha("black", 0.3))
lines(c(0, 1), c(wt, wt), lty =2, col = col_alpha("black", 0.3))

axis(1, at = c(0, k, 1), labels = c("0", "$k$", "1"))
axis(2, at = c(-1000, wt, 1000), labels = c(NA, "$\\tilde{w}$", NA), tick = TRUE, las = 1)
points(k, wt, cex = 1, pch = 16)

mtext("$kn$", side = 2, line = 1, at = wt - k*n/2, las = 0)
mtext("$\\overbrace{\\hspace{2cm}}$", side = 2, line = 0, at = wt - k*n/2)

mtext("$(1-k)m$", side = 4, line = 0.5, at = wt + (1-k)*m/2, las = 0)
mtext("$\\underbrace{\\hspace{1.0cm}}$", side = 4, line = -0.7, at = wt + (1-k)*m/2)

text(0.25, wt + (0.25 - k) * n + 0.05, labels = "$x_{i,j}=0$", col = B_col, srt = atan(0.75 * n) * 360/(2*pi))

text(0.25, wt + (0.25 - k) * m + 0.05, labels = "$x_{i,j}=1$", col = A_col, srt = atan(0.75 * m) * 360/(2*pi))



par(mar = c(0, 0, 0, 0))

plot(NULL, xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25), axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "")

# draw a circle
# xs <- seq(-1, 1, by = 0.001)
# ys <- sqrt(0.9^2 - xs^2)
# points(xs, ys, type = "l", col = light)
# points(xs, -ys, type = "l", col = light)


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



# radii for each game label

cat("draw axis labels\n")

text(xy(pi/2, 0.79), labels = "$n \\rightarrow$", pos = 4, srt = 90, cex = text_size)
text(xy(0, 0.85), labels = "$m \\rightarrow$", pos = 1, cex = text_size)

# beta <- 1
# rline(atan(beta), r = 1.1, lty = 3)
# rline(pi + atan(beta), r = 1.1, lty = 3)

beta <- -(1 - k) / k
rline(atan(beta), 2, col = gray(0.3, 0.3))
rline(pi + atan(beta), 2, col = gray(0.3, 0.3))

beta <- -(1 - k) / k
text(xy(atan(beta) + 0.07, 0.62), labels = "Pure Coordination", srt = calc_srt(beta), cex = text_size)
rarrow(atan(beta), r = 1.2, length = 0.1)

beta <- 0
rarrow(atan(beta), r = 1.2, length = 0.1)
text(xy(atan(beta) + 0.06, 0.75), labels = "Stag Hunt", srt = calc_srt(beta), cex = text_size)

beta <- 1
rarrow(atan(beta), r = 1.2, length = 0.1)
text(xy(atan(beta) + 0.06, 0.6), labels = "Prisoner's Dilemma", srt = calc_srt(beta), cex = text_size)

beta <- (2 - k) / (1 - k)
rarrow(atan(beta), r = 1.2, length = 0.1)
text(xy(atan(beta) + 0.06, 0.70), labels = "Hawk-Dove", srt = calc_srt(beta), cex = text_size)

beta <- (-k / (1 - k))
rarrow(pi + atan(beta), r = 1.2, length = 0.1)
text(xy(pi + atan(beta) - 0.07, 0.65), labels = "Invisible Hand", srt = calc_srt(beta), cex = text_size)

dev.off()
