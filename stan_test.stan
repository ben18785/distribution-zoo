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

  real log_prob = gamma_rng(2, 0.74);
  
  vector[2] x_rng = multi_normal_rng(to_vector([-4, -5.4]), [[16, 10.88], [10.88, 11.56]]);
  vector[2] x_rng1 = multi_student_t_rng(2, to_vector([2.2, -4]), [[0.16, -0.128], [-0.128, 0.64]]);
  
}