

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
  real prob = exp(inv_gamma_lpdf(7.3| 3.9, 0.909090909090909));
  real prob1 = exp(inv_gamma_lpdf(7.3| 3.9, 1.1));
}