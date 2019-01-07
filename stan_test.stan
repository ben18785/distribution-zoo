

data{
  vector[2] x;
  real mux;
  real muy;
  real rho;
  real sigmax;
  real sigmay;
}

generated quantities{
  real log_prob = multi_student_t_lpdf(x| 21.6, [-2.8, 1.8], [[5.76, 1.92], [1.92, 4]]);
  real log_prob1 = multi_normal_cholesky_lpdf(x| [2.6, -2.4], [[2, 0], [-0.64, 1.46642422238587]]);
  vector[2] x_rng = multi_student_t_rng(21.6, to_vector([-2.8, 1.8]), [[5.76, 1.92], [1.92, 4]]);
    
}