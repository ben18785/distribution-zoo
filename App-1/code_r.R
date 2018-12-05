
fRHelper <- function(mainName, params, input, import=NULL){
  switch(input$property,
        pdf=fMakeFunctionPaste(mainName=paste0("d", mainName),
                               params=params, prefixparams="x",
                               import=import),
        log_pdf=fMakeFunctionPaste(mainName=paste0("d", mainName),
                                   params=params, prefixparams="x",
                                   postfixparams="log=TRUE",
                                   import=import),
        random=fMakeFunctionPaste(mainName=paste0("r", mainName),
                                  params=params, prefixparams="n",
                                  import=import))
}


fRcode <- function(input){
  text <-
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fRHelper("norm", c(input$normal_mu, input$normal_sigma), input),
             Uniform=fRHelper("unif", c(input$uniform_a, input$uniform_b), input),
             LogNormal=fRHelper("lnorm", c(input$lognormal_mu,input$lognormal_sigma), input),
             Exponential=fRHelper("exp", input$exponential_rate, input),
             Gamma=fRHelper("gamma", c(input$gamma_shape, input$gamma_rate), input),
             t=fRHelper("st", c(input$t_mu,input$t_sigma, input$t_nu), input, import="library(LaplacesDemon)"),
             Beta=fRHelper("beta", c(input$beta_a,input$beta_b), input),
             Cauchy=fRHelper("cauchy", c(input$cauchy_location,input$cauchy_scale), input),
             HalfCauchy=fRHelper("halfcauchy", c(input$halfcauchy_location,input$halfcauchy_scale), input, import="library(LaplacesDemon)"),
             InverseGamma=fRHelper("invgamma", c(input$inversegamma_shape, 1.0 / input$inversegamma_scale), input, import="library(actuar)"),
             InverseChiSquared=fRHelper("invchisq", input$inversechisquared_df, input, import="library(LaplacesDemon)"),
             LogitNormal=fRHelper("logitnorm", c(input$logitnormal_mu, input$logitnormal_sigma), input, import="library(logitnorm)"))
    }else if(input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=fRHelper("binom", c(1, input$bernoulli_prob), input),
             Binomial=fRHelper("binom", c(input$binomial_size, input$binomial_prob), input),
             Poisson=fRHelper("pois", input$poisson_lambda, input),
             NegativeBinomial=fRHelper("nbinom", c(input$negativebinomial_mean, input$negativebinomial_dispersion), input)
             )
    }
           
  
  return(prismCodeBlock(text, language = "r"))
}
