list.of.packages <- c(
  'shiny',
  'LaplacesDemon',
  'ggplot2',
  'logitnorm',
  'actuar',
  'reshape',
  'mvtnorm',
  'ggExtra',
  'gridExtra',
  'DirichletReg',
  'scatterplot3d',
  'tidyverse'
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

library(shiny)
runApp("App-1", launch.browser = T)
