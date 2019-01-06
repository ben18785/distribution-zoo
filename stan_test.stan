functions{
  real logitnormal_lpdf(real x, real mu, real sigma){
    real temp = (logit(x) - mu)^2 / (2 * sigma^2);
    if(sigma < 0)
      return(log(0));
    return(-log(sigma) - 0.5 * log(2 * pi()) - temp - log(x) - log(1.0 - x));
  }
}


data{
  real x;
  real mu;
  real sigma;
}

generated quantities{
  real log_prob = logitnormal_lpdf(x| mu, sigma);
  real x_rng = inv_logit(normal_rng(mu, sigma));
    
}