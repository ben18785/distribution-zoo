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
  real log_prob = multinomial_lpmf(x| to_vector([0.375, 0.3125, 0.3125]));
  int x_rng[3] = multinomial_rng(to_vector([0.375, 0.3125, 0.3125]), 11);
  real prob = exp(inv_wishart_lpdf([[3, 0.5], [0.5, 2]]| 7.4, [[3, 0], [0, 2]]));
  real prob1 = exp(inv_gamma_lpdf(7.3| 3.9, 1.1));
}