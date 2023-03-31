
library(tikzDevice)
library(RColorBrewer)
library(viridis)
library(dplyr)

source("R/color_functions.R")
source("R/sim_functions.R")
source("R/plot_functions.R")
source("R/math_functions.R")

light <- gray(0)

A_col <- "#00AFB9" # green
B_col <- "#FFCE86" # yellow

complementary_col <- col_alpha("red", 0.2)
competitive_col <- col_alpha("orange", 0.2)
coordination_col <- col_alpha("blue", 0.2)
surplus_col <- col_alpha("lightblue", 0.2)

options(warnPartialMatchDollar=TRUE)
