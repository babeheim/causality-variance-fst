
plot_circular_regions <- function() {

  plot(NULL, xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25), axes = FALSE, frame.plot = FALSE, xlab = "", ylab = "")

  cat("plot colored sectors\n")

  # color sector II
  xc <- seq(-1, 0, by = 0.001)
  yc <- sqrt(1 - xc^2)
  xl <- c(0, 0, -1)
  yl <- c(1, 0, 0)
  polygon(c(xc, xl), c(yc, yl), col = complementarity_col, border = NA)

  # color sector IV
  xc <- seq(1, 0, by = -0.001)
  yc <- -sqrt(1 - xc^2)
  xl <- c(0, 0, 1)
  yl <- c(-1, 0, 0)
  polygon(c(xc, xl), c(yc, yl), col = simple_coord_col, border = NA)

  # color sector I, bottom
  xc <- seq(1, sqrt(2)/2, length.out = 300)
  yc <- sqrt(1 - xc^2)
  xl <- seq(sqrt(2)/2, 0, length.out = 10)
  polygon(c(xc, xl), c(yc, xl), col = coord_dilemma_col, border = NA)

  # color sector III, bottom
  xc <- -seq(0, sqrt(2)/2, length.out = 300)
  yc <- -sqrt(1 - xc^2)
  xl <- -seq(sqrt(2)/2, 0, length.out = 10)
  polygon(c(xc, xl), c(yc, xl), col = coord_dilemma_col, border = NA)

  # color sector III, top
  xc <- -seq(1, sqrt(2)/2, length.out = 300)
  yc <- -sqrt(1 - xc^2)
  xl <- -seq(sqrt(2)/2, 0, length.out = 10)
  polygon(c(xc, xl), c(yc, xl), col = anti_coord_dilemma_col, border = NA)

  # color sector I, top
  xc <- seq(0, sqrt(2)/2, length.out = 300)
  yc <- sqrt(1 - xc^2)
  xl <- seq(sqrt(2)/2, 0, length.out = 10)
  polygon(c(xc, xl), c(yc, xl), col = anti_coord_dilemma_col, border = NA)

  # add mask for B-half
  # slope: -(1 - k) / k
  thetas <- atan2((1 - k), -k) + seq(0, pi, length.out = 100)
  ys <- sin(thetas)
  xs <- cos(thetas)
  polygon(xs, ys, col = col_alpha("white", 0.7), border = NA)

  beta <- -(1 - k) / k
  rline(atan(beta), 2, col = gray(0.3, 0.3))
  rline(pi + atan(beta), 2, col = gray(0.3, 0.3))

  cat("draw axis labels\n")

  text(xy(pi/2, 0.79), labels = "$n \\rightarrow$", pos = 4, srt = 90, cex = text_size)
  text(xy(0, 0.85), labels = "$m \\rightarrow$", pos = 1, cex = text_size)


}
