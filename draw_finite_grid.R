
rm(list = ls())

source("project_support.R")

library(tikzDevice)
library(gtools)

# M groups, N individuals

calc_points <- function(M, N) {
  counts <- combinations(N + 1, M, v = 0:N, repeats.allowed = TRUE)
  stopifnot(nrow(counts) == choose(N + M, M))
  frqs <- counts / N

  dat <- data.frame(
    pbar = apply(frqs, 1, mean)
  )
  dat$btw <- apply((frqs - dat$pbar)^2, 1, mean)
  vars <- frqs * (1 - frqs)
  dat$win <- apply(vars, 1, mean)

  dat$total <- dat$btw + dat$win
  dat$Fst <- dat$btw / dat$total

  return(dat)
}




pdf("figures/finiteGridSims.pdf", height = 2.1, width = 6)

my_xlab <- expression(bar("x"))
my_ylab <- expression('F'['ST'])

par(mfrow = c(1, 3))

par(mar = c(4, 4, 3, 2))

ms <- c(2, 5, 8) # groups
ns <- c(10, 3) # ppl
cols <- c("lightblue", "black") # light then dark

for (i in 1:length(ms)) {
  for (j in 1:length(ns)) {
    m <- ms[i]; n <- ns[j]
    dat <- calc_points(m, n)
    if (j == 1) {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = TRUE)

      mtext(paste0(m, " groups"), side = 3, line = 0.3)
      mtext(my_xlab, side = 1, line = 2.4, cex = 0.9)
      mtext(my_ylab, side = 2, line = 2.4, cex = 0.9)

      axis(1, at = c(0, 1), labels = c(0, 1), cex.axis = 1.5) 
      axis(2, at = c(0, 1), labels = c("0", "1"), las = 2, cex.axis = 1.5)
    }
    points(dat$pbar, dat$Fst, col = col_alpha(cols[j], 0.9), pch = 16)
  }
}

dev.off()
