

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
  real prob = exp(inv_chi_square_lpdf(20| 5.4));
}