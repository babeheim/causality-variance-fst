
library(tikzDevice)
library(RColorBrewer)
library(viridis)
library(dplyr)

files <- list.files("R", full.names = TRUE)
for (i in 1:length(files)) {
  source(files[i])
}

light <- gray(0)

A_col <- "#00AFB9" # green
B_col <- "#FFCE86" # yellow

complementarity_col <- col_alpha("red", 0.2)
anti_coord_dilemma_col <- col_alpha("orange", 0.2)
simple_coord_col <- col_alpha("blue", 0.2)
coord_dilemma_col <- col_alpha("lightblue", 0.2)

options(warnPartialMatchDollar=TRUE)
