data{
  real x;
  real mu;
  real sigma;
}

generated quantities{
  real log_prob = cauchy_lpdf(x| mu, sigma) - cauchy_lccdf(0| mu, sigma);
  real x_rng = cauchy_rng(mu, sigma);
  while(x_rng < 0)
    x_rng = cauchy_rng(mu, sigma);
    
}