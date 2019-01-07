

data{
  vector[2] x;
  real mux;
  real muy;
  real rho;
  real sigmax;
  real sigmay;
}

generated quantities{
  real log_prob = multi_normal_lpdf(x| [mux, muy], [[sigmax^2, sigmax*sigmay*rho],[sigmax*sigmay*rho, sigmay^2]]);
  real log_prob1 = multi_normal_cholesky_lpdf(x| [2.6, -2.4], [[2, 0], [-0.64, 1.46642422238587]]);
  vector[2] x_rng = multi_normal_cholesky_rng(to_vector([-4.6, 2.8]), [[1.8, 0], [0.96, 1.28]]);
    
}