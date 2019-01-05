
fStanHelper <- function(mainName, params, input, import=NULL,
                        named_arguments=NULL, vector_params=FALSE, end_brace=FALSE){
  switch(input$property,
        pdf=fMakeFunctionPaste_stan(mainName=paste0("exp(", mainName, "_lpdf"),
                               params=params, prefixparams="x",
                               import=import, named_arguments=named_arguments,
                               vector_params=vector_params, end_brace = T),
        log_pdf=fMakeFunctionPaste_stan(mainName=paste0(mainName, "_lpdf"),
                                   params=params, prefixparams="x",
                                   import=import, named_arguments=named_arguments,
                                   vector_params=vector_params),
        random=fMakeFunctionPaste_stan(mainName=paste0(mainName, "_rng"),
                                  params=params,
                                  import=import, named_arguments=named_arguments,
                                  vector_params=vector_params))
}


fStanCode <- function(input){
  text <-
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fStanHelper("normal", c(input$normal_mu, input$normal_sigma), input),
             Uniform=fStanHelper("unif", c(input$uniform_a, input$uniform_b), input),
             LogNormal=fStanHelper("lnorm", c(input$lognormal_mu,input$lognormal_sigma), input),
             Exponential=fStanHelper("exp", input$exponential_rate, input),
             Gamma=fStanHelper("gamma", c(input$gamma_shape, input$gamma_rate), input),
             t=fStanHelper("st", c(input$t_mu,input$t_sigma, input$t_nu), input, import="library(LaplacesDemon)"),
             Beta=fStanHelper("beta", c(input$beta_a,input$beta_b), input),
             Cauchy=fStanHelper("cauchy", c(input$cauchy_location,input$cauchy_scale), input),
             HalfCauchy=dHalfCauchyFull(input$halfcauchy_location, input$halfcauchy_scale, input),
             InverseGamma=fStanHelper("invgamma", c(input$inversegamma_shape, 1.0 / input$inversegamma_scale), input, import="library(actuar)"),
             InverseChiSquared=fStanHelper("invchisq", input$inversechisquared_df, input, import="library(LaplacesDemon)"),
             LogitNormal=fStanHelper("logitnorm", c(input$logitnormal_mu, input$logitnormal_sigma), input, import="library(logitnorm)"))
    }else if(input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=fStanHelper("binom", c(1, input$bernoulli_prob), input),
             Binomial=fStanHelper("binom", c(input$binomial_size, input$binomial_prob), input),
             Poisson=fStanHelper("pois", input$poisson_lambda, input),
             NegativeBinomial=fStanHelper("nbinom", c(input$negativebinomial_mean, input$negativebinomial_dispersion), input,
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
             Multinomial=dMultinomialFull(input$multinomial_prob1, input$multinomial_prob2, input$multinomial_prob3, input),
             Wishart=dWishartFull(input$wishart_df, input),
             InverseWishart=dInverseWishartFull(input$inversewishart_df, input),
             LKJ=fLKJ_1(input$lkj_eta, input$lkj_dimension, input),
             Dirichlet=if_else(input$dirichlet_dimension==2, fStanHelper("dirichlet", c(input$dirichlet_alpha1, input$dirichlet_alpha2), input, vector_params = TRUE, import="library(LaplacesDemon)"),
                               if_else(input$dirichlet_dimension==3, fStanHelper("dirichlet", c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3), input, vector_params = TRUE, import="library(LaplacesDemon)"),
                                       if_else(input$dirichlet_dimension==4, fStanHelper("dirichlet", c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3, input$dirichlet_alpha4), input, vector_params = TRUE, import="library(LaplacesDemon)"), "test")))
      )
    }
           
  
  return(prismCodeBlock(text, language = "r"))
}
