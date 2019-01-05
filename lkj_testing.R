library(rstan)

stan_model <- stan_model('lkj_test.stan')

N <- 2
X <- diag(N)
fit <- sampling(stan_model, data=list(K=N, X=X, eta=2), algorithm="Fixed_param")
print(fit)
params <- extract(fit)
exp(params$log_prob[1])
