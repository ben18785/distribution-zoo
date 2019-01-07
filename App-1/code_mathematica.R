fMathematicaHelper <- function(mainName, params, input, import=NULL, named_arguments=NULL, vector_params=FALSE,
                               mathematica_vector=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0("PDF[", mainName, "Distribution"),
                                params=params, postfixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params,
                                mathematica = T,
                                mathematica_vector=mathematica_vector),
         log_pdf=fMakeFunctionPaste(mainName=paste0("Log@PDF[", mainName, "Distribution"),
                                    params=params, postfixparams="x",
                                    import=import, named_arguments=named_arguments,
                                    vector_params=vector_params,
                                    mathematica = T,
                                    mathematica_vector=mathematica_vector),
         random=fMakeFunctionPaste(mainName=paste0("RandomVariate[", mainName, "Distribution"),
                                   params=params, postfixparams="n",
                                   import=import, named_arguments=named_arguments,
                                   vector_params=vector_params,
                                   mathematica = T,
                                   mathematica_vector=mathematica_vector))
}

dHalfCauchy_mathematica <- function(cauchy_location, cauchy_scale){
  top <- paste0("aDist = TruncatedDistribution[{0, \\[Infinity]}, CauchyDistribution[", cauchy_location, ", ", cauchy_scale, "]]")
  bottom <- "PDF[aDist, x]"
  return(paste(top, bottom, sep = "\n"))
}

dHalfCauchy_log_mathematica <- function(cauchy_location, cauchy_scale){
  top <- paste0("aDist = TruncatedDistribution[{0, \\[Infinity]}, CauchyDistribution[", cauchy_location, ", ", cauchy_scale, "]]")
  bottom <- "Log@PDF[aDist, x]"
  return(paste(top, bottom, sep = "\n"))
}

rHalfCauchy_mathematica <- function(cauchy_location, cauchy_scale){
  top <- paste0("aDist = TruncatedDistribution[{0, \\[Infinity]}, CauchyDistribution[", cauchy_location, ", ", cauchy_scale, "]]")
  bottom <- "RandomVariate[aDist, n]"
  return(paste(top, bottom, sep = "\n"))
}

fHalfCauchy_mathematica <- function(input){
  switch(input$property,
         pdf=dHalfCauchy_mathematica(input$halfcauchy_location,input$halfcauchy_scale),
         log_pdf=dHalfCauchy_log_mathematica(input$halfcauchy_location,input$halfcauchy_scale),
         random=rHalfCauchy_mathematica(input$halfcauchy_location,input$halfcauchy_scale)
  )
}

dLogitNormal_mathematica <- function(logit_mu, logit_sigma){
  top <- paste0("aDist=TransformedDistribution[LogisticSigmoid[x], x \\[Distributed] NormalDistribution[", logit_mu, ", ",  logit_sigma, "]]")
  bottom <- "PDF[aDist, x]"
  return(paste(top, bottom, sep = "\n"))
}

dLogitNormal_log_mathematica <- function(logit_mu, logit_sigma){
  top <- paste0("aDist=TransformedDistribution[LogisticSigmoid[x], x \\[Distributed] NormalDistribution[", logit_mu, ", ",  logit_sigma, "]]")
  bottom <- "Log@PDF[aDist, x]"
  return(paste(top, bottom, sep = "\n"))
}

rLogitNormal_mathematica <- function(logit_mu, logit_sigma){
  top <- paste0("aDist=TransformedDistribution[LogisticSigmoid[x], x \\[Distributed] NormalDistribution[", logit_mu, ", ",  logit_sigma, "]]")
  bottom <- "RandomVariate[aDist, n]"
  return(paste(top, bottom, sep = "\n"))
}

fLogitNormal_mathematica <- function(input){
  switch(input$property,
         pdf=dLogitNormal_mathematica(input$logitnormal_mu,input$logitnormal_sigma),
         log_pdf=dLogitNormal_log_mathematica(input$logitnormal_mu,input$logitnormal_sigma),
         random=rLogitNormal_mathematica(input$logitnormal_mu,input$logitnormal_sigma)
  )
}

dMultivariateNormal_mathematica <- function(mux, muy, sigmax, sigmay, rho){
  top <- paste0("aDist=MultinormalDistribution[{", mux, ", ", muy, "}, {{", sigmax^2, ", ", sigmax * sigmay * rho, "}, {", sigmax * sigmay * rho, ", ",  
                sigmay^2, "}}]")
}

fMultivariatenormal_mathematica <- function(input){
  top <- dMultivariateNormal_mathematica(input$multivariatenormal_mux,
                                         input$multivariatenormal_muy,
                                         input$multivariatenormal_sigmax,
                                         input$multivariatenormal_sigmay,
                                         input$multivariatenormal_rho)
  switch(input$property,
         pdf=paste(top, "PDF[aDist, x]", sep = "\n"),
         log_pdf=paste(top, "Log@PDF[aDist, x]", sep = "\n"),
         random=paste(top, "RandomVariate[aDist, n]", sep = "\n")
  )
}

fMathematicacode <- function(input){
  text <- 
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fMathematicaHelper("Normal",
                                       params = c(input$normal_mu, input$normal_sigma),
                                       input),
             Uniform=fMathematicaHelper("Uniform",
                                        params = c(input$uniform_a, input$uniform_b),
                                        input,
                                        vector_params = T,
                                        mathematica_vector = T),
             LogNormal=fMathematicaHelper("LogNormal",
                                          params = c(input$lognormal_mu, input$lognormal_sigma),
                                          input),
             Exponential=fMathematicaHelper("Exponential",
                                            params = input$exponential_rate, 
                                            input),
             Gamma=fMathematicaHelper("Gamma",
                                      params = c(input$gamma_shape, 1 / input$gamma_rate), 
                                      input),
             t=fMathematicaHelper("StudentT",
                                  params = c(input$t_mu,input$t_sigma, input$t_nu), 
                                  input),
             Beta=fMathematicaHelper("Beta",
                                     params = c(input$beta_a,input$beta_b), 
                                     input),
             Cauchy=fMathematicaHelper("Cauchy",
                                       params = c(input$cauchy_location,input$cauchy_scale), 
                                       input),
             HalfCauchy=fHalfCauchy_mathematica(input),
             InverseGamma=fMathematicaHelper("InverseGamma",
                                             params = c(input$inversegamma_shape,input$inversegamma_scale), 
                                             input),
             InverseChiSquared=fMathematicaHelper("InverseChiSquare",
                                                  params = input$inversechisquared_df, 
                                                  input),
             LogitNormal=fLogitNormal_mathematica(input)
      )
    }else if(input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=fMathematicaHelper("Bernoulli",
                                          params = input$bernoulli_prob, 
                                          input),
             Binomial=fMathematicaHelper("Binomial",
                                         params = c(input$binomial_size, input$binomial_prob), 
                                         input),
             Poisson=fMathematicaHelper("Poisson",
                                        params = input$poisson_lambda, 
                                        input),
             NegativeBinomial=fMathematicaHelper("NegativeBinomial",
                                                 params = c(input$negativebinomial_dispersion, input$negativebinomial_dispersion / (input$negativebinomial_dispersion + input$negativebinomial_mean)), 
                                                 input),
             BetaBinomial=fMathematicaHelper("BetaBinomial",
                                             params = c(input$betabinomial_shape1, input$betabinomial_shape2, input$betabinomial_size), 
                                             input)
             
      )
    }else if(input$distType=='Multivariate'){
      switch(input$dist2,
             MultivariateNormal=fMultivariatenormal_mathematica(input)
      )
    }
  return(prismCodeBlock(text))
}
