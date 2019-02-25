rm(list=ls())
library(rstan)
setwd("~/Desktop/distribution-viewer")
stan_model <- stan_model('stan_test.stan')
sampling(stan_model, data=list(x=-9.5), algorithm="Fixed_param")


x <- c(10, 2, 2)
mux <- 2.6
muy <- -2.4
sigmax <- 2
sigmay <- 1.6
rho <- -0.4
fit <- sampling(stan_model, data=list(x=x, mux=mux, muy=muy, sigmax=sigmax, sigmay=sigmay, rho=rho), algorithm="Fixed_param")
print(fit)
params <- rstan::extract(fit)
exp(params$log_prob[1])
exp(params$log_prob1[1])

samples <- params$x_rng
samples[2,,]
median(map_dbl(seq(1,4000,1), ~samples[.,,][1,2]))
