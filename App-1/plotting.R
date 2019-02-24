
fScaleFull <- function(input) {
  lAllReal <- c('Normal','Uniform','t','Cauchy')
  lUpperReal <- c('Exponential','LogNormal','Gamma','HalfCauchy',
                  'InverseGamma','InverseChiSquared')
  
  if (input$dist%in% lAllReal){
    lScale <- seq(-input$n,input$n,2*input$n/200)
  } else if (input$dist%in% lUpperReal){
    lScale <- seq(0,input$n,input$n/400)
  } else{
    lScale <- seq(0,1,1/200)
  }
  return(lScale)
}

fScaleFull1 <- function(input){
  lScale <- switch(input$dist1,
                   Bernoulli=c(0,1),
                   Binomial=seq(0,input$binomial_size,1),
                   DiscreteUniform=seq(input$discreteuniform_lower - 1, input$discreteuniform_upper + 1, 1),
                   Poisson=seq(0,input$poisson_range,1),
                   NegativeBinomial=seq(0,input$negativebinomial_range,1),
                   BetaBinomial=seq(0,input$betabinomial_size,1)
  )
  return(lScale)
}

fExtraFunctionInputsFull <- function(input){
  lExtra <- switch(input$dist,
                   Normal=paste("mean=",input$normal_mu,",sd=",input$normal_sigma),
                   Uniform = paste("min=",input$uniform_a,",max=",input$uniform_b),
                   LogNormal = paste("meanlog=",input$lognormal_mu,",sdlog=",input$lognormal_sigma),
                   Exponential = paste("rate=",input$exponential_rate),
                   Gamma=paste("shape=",input$gamma_shape,",rate=",input$gamma_rate),
                   t = paste("mu=",input$t_mu,",sigma=",input$t_sigma,",nu=",input$t_nu),
                   Beta=paste("shape1=",input$beta_a,",shape2=",input$beta_b),
                   Cauchy=paste("location=",input$cauchy_location,",scale=",input$cauchy_scale),
                   HalfCauchy=paste("location=",input$halfcauchy_location,",scale=",input$halfcauchy_scale),
                   InverseGamma=paste("shape=",input$inversegamma_shape,",scale=",input$inversegamma_scale),
                   InverseChiSquared=paste("df=",input$inversechisquared_df),
                   LogitNormal=paste("mu=",input$logitnormal_mu,",sigma=",input$logitnormal_sigma),
                   paste("mean=1,sd=1"))
  return(lExtra)
}

fExtra1FunctionInputsFull <- function(input){
  lExtra <- switch(input$dist1,
                   Bernoulli=paste("prob=",input$bernoulli_prob),
                   Binomial=paste("size=",input$binomial_size,",prob=",input$binomial_prob),
                   DiscreteUniform=paste("min=", input$discreteuniform_lower,",max=",input$discreteuniform_upper),
                   Poisson=paste("lambda=",input$poisson_lambda),
                   NegativeBinomial=paste("mu=",input$negativebinomial_mean,",size=",input$negativebinomial_dispersion),
                   BetaBinomial=paste("n=",input$betabinomial_size,",alpha=",input$betabinomial_shape1,",beta=",input$betabinomial_shape2),
                   paste("mean=1,sd=1"))
  return(lExtra)
}
