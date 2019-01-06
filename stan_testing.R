library(rstan)

stan_model <- stan_model('stan_test.stan')

x <- 5
fit <- sampling(stan_model, data=list(x=x, mu=0, sigma=1.5), algorithm="Fixed_param")
print(fit)
params <- extract(fit)
exp(params$log_prob[1])
