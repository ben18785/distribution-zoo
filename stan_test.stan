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
  
  int x_rng[3];
  x_rng = multinomial_rng(to_vector([0.212121212121212, 0.522727272727273, 0.265151515151515]), 81);
  
}