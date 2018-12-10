
fRHelper <- function(mainName, params, input, import=NULL, named_arguments=NULL){
  switch(input$property,
        pdf=fMakeFunctionPaste(mainName=paste0("d", mainName),
                               params=params, prefixparams="x",
                               import=import, named_arguments=named_arguments),
        log_pdf=fMakeFunctionPaste(mainName=paste0("d", mainName),
                                   params=params, prefixparams="x",
                                   postfixparams="log=TRUE",
                                   import=import, named_arguments=named_arguments),
        random=fMakeFunctionPaste(mainName=paste0("r", mainName),
                                  params=params, prefixparams="n",
                                  import=import, named_arguments=named_arguments))
}

dBetaBinomialCode <- paste("# function definition",
                           "dbetabinom <- function(x, size, alpha, beta, log=FALSE){",
                           "  if(!log)",
                           "    return(choose(size, x) * beta(x + alpha, size - x + beta) / beta(alpha, beta))",
                           "  else",
                           "    return(log(choose(size, x) * beta(x + alpha, size - x + beta) / beta(alpha, beta)))",
                           "}",
                           "# calling function",
                           sep="\n")
rBetaBinomialCode <- paste("# function definition",
                           "rbetabinom <- function(n, size, alpha, beta){",
                           "  theta <- rbeta(n, alpha, beta)",
                           "  return(rbinom(n, size, theta))",
                           "}",
                           "# calling function",
                           sep="\n")
dBetaBinomialFull <- function(size, shape1, shape2, input){
  if(input$property!="random")
    paste(dBetaBinomialCode,
        fRHelper("betabinom", c(size, shape1, shape2), input),
        sep="\n")
  else
    paste(rBetaBinomialCode,
          fRHelper("betabinom", c(size, shape1, shape2), input),
          sep="\n")
}


dmvnorm2DCode <-
  paste("# 2d mvt normal pdf",
        "library(mvtnorm)",
        "dmvrnorm2D <- function(x, mux, muy, sigmax, sigmay, rho, log=FALSE){",
        "  return(dmvnorm(x, c(mux, muy),",
        "                 matrix(c(sigmax^2, sigmax * sigmay * rho,",
        "                          sigmax * sigmay * rho, sigmay^2),",
        "                         ncol = 2),",
        "                 log))",
        "}",
        "# calling function (note x must be 2d)",
        sep = "\n")
rmvnorm2DCode <-
  paste("# 2d mvt normal random samples",
        "library(mvtnorm)",
        "rmvrnorm2D <- function(n, mux, muy, sigmax, sigmay, rho){",
        "  return(rmvnorm(n, c(mux, muy),",
        "                 matrix(c(sigmax^2, sigmax * sigmay * rho,",
        "                          sigmax * sigmay * rho, sigmay^2),",
        "                        ncol = 2)))",
        "}",
        "# calling function",
        sep = "\n")


dmvt2DCode <-
  paste("# 2d mvt Student-t distribution pdf",
        "library(mvtnorm)",
        "dmvt2D <- function(x, mux, muy, sigmax, sigmay, rho, df, log=FALSE){",
        "  return(dmvt(x, c(mux, muy),",
        "              matrix(c(sigmax^2, sigmax * sigmay * rho,",
        "                       sigmax * sigmay * rho, sigmay^2),",
        "                     ncol = 2),",
        "              df,",
        "              log))",
        "}",
        "# calling function (note x must be 2d)",
        sep = "\n")
rmvt2DCode <-
  paste("# 2d mvt Student-t random samples (note argument order different to pdf)",
        "library(mvtnorm)",
        "rmvt2D <- function(n, mux, muy, sigmax, sigmay, rho, df){",
        "  return(rmvt(n,",
        "              matrix(c(sigmax^2, sigmax * sigmay * rho,",
        "                       sigmax * sigmay * rho, sigmay^2),",
        "                     ncol = 2),",
        "              df,",
        "              c(mux, muy)))",
        "}",
        "# calling function",
        sep = "\n")

dMVNormalFull <- function(mux, muy, sigmax, sigmay, rho, input){
  if(input$property!="random")
    paste(dmvnorm2DCode,
          fRHelper("mvrnorm2D", c(mux, muy, sigmax, sigmay, rho), input),
          sep="\n")
  else
    paste(rmvnorm2DCode,
          fRHelper("mvrnorm2D", c(mux, muy, sigmax, sigmay, rho), input),
          sep="\n")
}

dMVTFull <- function(mux, muy, sigmax, sigmay, rho, df, input){
  if(input$property!="random")
    paste(dmvt2DCode,
          fRHelper("mvt2D", c(mux, muy, sigmax, sigmay, rho, df), input),
          sep="\n")
  else
    paste(rmvt2DCode,
          fRHelper("mvt2D", c(mux, muy, sigmax, sigmay, rho, df), input),
          sep="\n")
}

dMultinomialFull <- function(prob1, prob2, prob3, input){
  if(input$property!="random"){
    fRHelper("multinom", c(input$multinomial_prob1, input$multinomial_prob2, input$multinomial_prob3), input,
             named_arguments=c("prob"))
  }else{
    fMakeFunctionPaste("rmultinom",
                       params=c(input$multinomial_prob1, input$multinomial_prob2, input$multinomial_prob3),
                       prefixparams=c("n", "size"),
                       named_arguments=c("prob"))
  }
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
             NegativeBinomial=fRHelper("nbinom", c(input$negativebinomial_mean, input$negativebinomial_dispersion), input,
                                       named_arguments=c("mu", "size")),
             BetaBinomial=dBetaBinomialFull(input$betabinomial_size, input$betabinomial_shape1, input$betabinomial_shape2, input)
             )
    }else if(input$distType=='Multivariate'){
      switch(input$dist2,
             MultivariateNormal=dMVNormalFull(input$multivariatenormal_mux,
                                              input$multivariatenormal_muy,
                                              input$multivariatenormal_sigmax,
                                              input$multivariatenormal_sigmay,
                                              input$multivariatenormal_rho, input),
             MultivariateT=dMVTFull(input$multivariatet_mux,
                                    input$multivariatet_muy,
                                    input$multivariatet_sigmax,
                                    input$multivariatet_sigmay,
                                    input$multivariatet_rho, 
                                    input$multivariatet_df, input),
             Multinomial=dMultinomialFull(input$multinomial_prob1, input$multinomial_prob2, input$multinomial_prob3, input)
      )
    }
           
  
  return(prismCodeBlock(text, language = "r"))
}
