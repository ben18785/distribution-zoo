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
  'tidyverse',
  'rjson'
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

library(shiny)
runApp("App-1")

rsconnect::setAccountInfo(name='ben18785', token='5698C07FA0452DF286D563737A3B882D', secret='Ch5paWJPUOqPudGF5NeRcwSvdQSJZHoHiOMt7Zic')
    