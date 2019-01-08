functions{
  real logitnormal_lpdf(real x, real mu, real sigma){
    real temp = (logit(x) - mu)^2 / (2 * sigma^2);
    if(sigma < 0)
      return(log(0));
    return(-log(sigma) - 0.5 * log(2 * pi()) - temp - log(x) - log(1.0 - x));
  }
}

data{
  int x[3];
  real mux;
  real muy;
  real rho;
  real sigmax;
  real sigmay;
}

generated quantities{
  // calling function

  real log_prob = poisson_lpmf(20| 21);
  int x_rng[3] = multinomial_rng(to_vector([0.375, 0.3125, 0.3125]), 11);
  real prob = exp(binomial_lpmf(5| 29, 0.13));
  real prob1 = exp(multi_normal_cholesky_lpdf([-3, 1]| [-3.2, 1.2], [[0.8, 0], [-0.32, 1.56767343538123]]));
}