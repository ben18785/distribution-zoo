library(rstan)

stan_model <- stan_model('lkj_test.stan')

N <- 2
X <- diag(c(1, 1))
fit <- sampling(stan_model, data=list(K=N, X=X, eta=10), algorithm="Fixed_param")
print(fit)
params <- extract(fit)
exp(params$log_prob[1])
