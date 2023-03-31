
rm(list = ls())

unlink("figures", recursive = TRUE)
dir.create("figures")

source("draw_circular_figure.R")

source("draw_contour_diagrams.R")

source("draw_fitness_figures.R")

source("draw_pies.R")

source("draw_theta_gradient.R")

source("draw_finite_grid.R")
