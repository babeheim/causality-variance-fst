
plot_synergistic_contours <- function(k, n, m) {

  l <- m/(m-n)

  plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxs="i", yaxs="i", tck = 0.02, axes = FALSE, xlab = "", ylab = "", frame.plot = FALSE)
  # xaxs sets the interval calculation for the x-axis, "r" extends range by 4 percent at each end and is the default
  # "i" uses the internal original data range

  if (isTRUE(all.equal(l, k))) {
  # this has to come first b/c floating-point strangeness might also satisfy another condition below
  # special case, no GBT exists

    if (m > 0 & n < 0) {
      # pure coordination type - land to the right of l, water to the left
      if (l > 0) {
        polygon(c(0, min(1, l), min(1, l), 0), c(0, 0, 1, 1), col = water_colors[length(water_colors)], border = NA)
        # now show various levels
        for (i in 1:length(water_levels)) {
          level <- water_levels[i]
          xs <- seq(0, min(1, l), by = 0.001)
          ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((l - xs))
          # fix y-boundaries
          if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
          if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
          if (any(0 < ys & ys < 1)) {
            polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
          }
        }
      }
      if (l < 1) {
        polygon(c(1, max(0, l), max(0, l), 1), c(0, 0, 1, 1), col = land_colors[1], border = NA)
        # now show various levels
        for (i in 1:length(land_levels)) {
          level <- land_levels[i]
          xs <- seq(max(0, l), 1, by = 0.001)
          ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((l - xs))
          # fix y-boundaries
          if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
          if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
          if (any(0 < ys & ys < 1)) {
            polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
          }
        }
      }
    } else if (m < 0 & n > 0) {
      # pure anti-coord type - water to the right of l, land to the left
      if (l > 0) {
        polygon(c(0, min(l, 1), min(l, 1), 0), c(0, 0, 1, 1), col = land_colors[1], border = NA)
        # now show various levels
        for (i in 1:length(land_levels)) {
          level <- land_levels[i]
          xs <- seq(0, min(1, l), by = 0.001)
          ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((l - xs))
          # fix y-boundaries
          if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
          if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
          if (any(0 < ys & ys < 1)) {
            polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
          }
        }
      }
      if (l < 1) {
        polygon(c(1, max(0, l), max(0, l), 1), c(0, 0, 1, 1), col = water_colors[length(water_colors)], border = NA)
        for (i in 1:length(water_levels)) {
          level <- water_levels[i]
          xs <- seq(max(0, l), 1, by = 0.001)
          ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((l - xs))
          # fix y-boundaries
          if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
          if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
          if (any(0 < ys & ys < 1)) {
            polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
          }
        }
      }
    }

    abline(v = l, col = zero_line_col) 

    # actually when k < 0 or k > 1, m and n can also be both positive and this be true...gotta figure out the general case...

  } else if (l < k) {
    # zero-line approaches Inf as x approaches l from the left
    # Complementarity
    # Anti-Coordination Dilemmas

    if (l > 0) {
      # draw the gradient-reversal space
      polygon(c(0, l, l, 0), c(0, 0, 1, 1), col = land_colors[1], border = NA)

      for (i in 1:length(land_levels)) {
        level <- land_levels[i]
        xs <- seq(0, min(1, l), by = 0.001)
        ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((l - xs))
        # fix y-boundaries
        if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
        if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
        if (any(0 < ys & ys < 1)) {
          if (ys[1] < 0 & ys[length(ys)] > 1) {
            # add corner points to ensure the polygon closes
            xs <- c(xs, min(1, l), min(1, l))
            ys <- c(ys, 1, 0)
          }
          polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
        }
      }
    }
    if (l < 1) {
      # draw the normal space above the zero line
      xs <- seq(max(0, l), 1, by = 0.001)
      ys <- (xs - k)/(xs - l)
      # fix y-boundaries
      if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
      if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
      # add corner points to ensure the polygon closes
      xs <- c(min(xs), xs, max(xs))
      ys <- c(1, ys, 1)
      polygon(xs, ys, col = land_colors[1], border = NA)

      # now show various levels

      for (i in 1:length(land_levels)) {
        level <- land_levels[i]
        xs <- seq(max(0, l), 1, by = 0.001)
        ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((l - xs))
        if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
        if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
        if (any(ys > 0 & ys < 1)) {
          # add points to ensure polygon closes
          if (ys[1] < 0 & ys[length(ys)] > 1) {
            xs <- c(max(0, l), max(0, l), xs)
            ys <- c(600, -600, ys)
          }
          polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
        }
      }

      if (k < 1) {
        # draw the normal space below the zero-line
        xs <- seq(max(0, k), 1, by = 0.001)
        ys <- (xs - k)/(xs - l)
        if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
        if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
        # add corner points to ensure the polygon closes
        xs <- c(xs, 1, max(0, l))
        ys <- c(ys, 0, 0)
        polygon(c(max(0, k), xs, 1, max(0, k)), c(0, ys, 0, 0), col = water_colors[length(water_colors)], border = NA)
        
        # now show various levels
        
        for (i in 1:length(water_levels)) {
          level <- water_levels[i]
          xs <- seq(max(0, k), 1, by = 0.001)
          ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((l - xs))
          # fix y-boundaries
          if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
          if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
          if (any(0 < ys & ys < 1)) {
            polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
          }
        }
      }

      # draw the zero line itself
      xs <- seq(max(0, k), 1, by = 0.001)
      ys <- (xs - k)/(xs - l)
      points(xs, ys, type = "l", col = zero_line_col) 

      # and the l-line
      abline(v = l, lty = 2, col = col_alpha("white", 0.8))

    }

  } else if (l > k) {
  # zero-line approaches -Inf as x approaches l from the left
  # Simple Coordination
  # Coordination Dilemmas

    if (l < 1) {
      # draw the gradient reversal space
      polygon(c(1, l, l, 1), c(0, 0, 1, 1), col = land_colors[1], border = NA)

      for (i in 1:length(land_levels)) {
        level <- land_levels[i]
        xs <- seq(max(0, l), 1, by = 0.001)
        ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((l - xs))
        if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
        if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
        if (any(0 < ys & ys < 1)) {
          if (ys[1] > 1 & ys[length(ys)] < 0) {
            # add corner points to ensure the polygon closes
            xs <- c(max(0, l), max(0, l), xs)
            ys <- c(0, 1, ys)
          }
          polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
        }
      }
    }
    if (l > 0) {
      xs <- seq(0, min(1, l), by = 0.001)
      ys <- (xs - k)/(xs - l)
      # fix y-boundaries
      if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
      if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
      # add corner points to ensure the polygon closes
      xs <- c(min(xs), xs, max(xs))
      ys <- c(1, ys, 1)
      polygon(xs, ys, col = land_colors[1], border = NA)

      # now show various levels

      for (i in 1:length(land_levels)) {
        level <- land_levels[i]
        xs <- seq(0, min(1, l), by = 0.001)
        ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((l - xs))
        # fix y-boundaries
        if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
        if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
        if (any(0 < ys & ys < 1)) {
          # add corner points to ensure the polygon closes
          if (ys[1] > 1 & ys[length(ys)] < 0) {
            xs <- c(xs, min(1, l), min(1, l))
            ys <- c(ys, -600, 600)
          }
          polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
        }
      }

      if (k > 0) {
        # draw the normal space below the zero-line
        xs <- seq(0, k, by = 0.001)
        ys <- (xs - k)/(xs - l)
        # add corner points to ensure the polygon closes
        xs <- c(xs, min(1, k), 0)
        ys <- c(ys, 0, 0)
        polygon(c(0, xs, k, 0), c(0, ys, 0, 0), col = water_colors[length(water_colors)], border = NA)
        
        # now show various levels
        
        for (i in 1:length(water_levels)) {
          level <- water_levels[i]
          xs <- seq(0, min(1, k), by = 0.001)
          ys <- (level/((m-n)*(xs * (1-xs))) - (xs - k))/((l - xs))
          # fix y-boundaries
          if (ys[1] == Inf | ys[1] == -Inf) ys[1] <- ys[2]
          if (ys[length(ys)] == Inf | ys[length(ys)] == -Inf) ys[length(ys)] <- ys[length(ys)-1]
          if (any(0 < ys & ys < 1)) {
            polygon(xs, ys, border = NA, col = data_gradient2(level, my.start = water_scale_min, my.stop = land_scale_max, lower_colors = water_colors, upper_colors = land_colors, n_bins = 30))
          }
        }

      }

      # draw the zero line itself
      xs <- seq(0, min(k, 1), by = 0.001)
      ys <- (xs - k)/(xs - l)
      points(xs, ys, type = "l", col = zero_line_col) 
      
      # and the l-line
      abline(v = l, lty = 2, col = col_alpha("white", 0.8))

    }

  } else {
    stop("should not happen")
  }

}
