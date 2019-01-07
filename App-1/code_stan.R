
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

fStanHelper_discrete <- function(mainName, params, input, import=NULL,
                        named_arguments=NULL, vector_params=FALSE, end_brace=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste_stan(mainName=paste0("exp(", mainName, "_lpmf"),
                                     params=params, prefixparams="x",
                                     import=import, named_arguments=named_arguments,
                                     vector_params=vector_params, end_brace = T),
         log_pdf=fMakeFunctionPaste_stan(mainName=paste0(mainName, "_lpmf"),
                                         params=params, prefixparams="x",
                                         import=import, named_arguments=named_arguments,
                                         vector_params=vector_params),
         random=fMakeFunctionPaste_stan(mainName=paste0(mainName, "_rng"),
                                        params=params,
                                        import=import, named_arguments=named_arguments,
                                        vector_params=vector_params))
}

dhalfcauchy_stan <- function(input){
  switch(input$property,
         pdf=paste0("exp(", 
                   fMakeFunctionPaste_stan(mainName = "cauchy_lpdf",
                                     params=c(input$halfcauchy_location, input$halfcauchy_scale),
                                     prefixparams="x"),
                   " - ",
                   fMakeFunctionPaste_stan(mainName = "cauchy_lccdf",
                                           params=c(input$halfcauchy_location, input$halfcauchy_scale),
                                           prefixparams="0"),
                   ")"),
         log_pdf=paste(
                       fMakeFunctionPaste_stan(mainName = "cauchy_lpdf",
                                               params=c(input$halfcauchy_location, input$halfcauchy_scale),
                                               prefixparams="x"),
                       " - ",
                       fMakeFunctionPaste_stan(mainName = "cauchy_lccdf",
                                               params=c(input$halfcauchy_location, input$halfcauchy_scale),
                                               prefixparams="0")),
         random=paste(
           paste0("real x_rng = cauchy_rng(", input$halfcauchy_location, ", ", input$halfcauchy_scale, ");"),
           "while(x_rng < 0)",
           paste0("  real x_rng = cauchy_rng(", input$halfcauchy_location, ", ", input$halfcauchy_scale, ");"),
         sep = "\n")
                   
  )
}

dlogitnormal_stan <- paste(
  "functions{",
  "  real logitnormal_lpdf(real x, real mu, real sigma){",
  "    real temp = (logit(x) - mu)^2 / (2 * sigma^2);",
  "    if(sigma < 0)",
  "      return(log(0));",
  "    return(-log(sigma) - 0.5 * log(2 * pi()) - temp - log(x) - log(1.0 - x));",
  "  }",
  "}",
  "// calling function",
  sep="\n"
)

flogitnormal_stan <- function(input){
  switch(input$property,
         pdf=paste(dlogitnormal_stan,
                   fStanHelper("logitnormal", c(input$logitnormal_mu, input$logitnormal_sigma), input, end_brace = T),
                   sep = "\n"),
         log_pdf=paste(dlogitnormal_stan,
                       fStanHelper("logitnormal", c(input$logitnormal_mu, input$logitnormal_sigma), input),
                       sep = "\n"),
         random=paste0("inv_logit(normal_rng(", input$logitnormal_mu, ", ", input$logitnormal_sigma, "))")
  )
}

dCalculateChol <- function(sigmax, sigmay, rho){
  L <- t(chol(matrix(c(sigmax^2, sigmax*sigmay*rho, sigmax*sigmay*rho, sigmay^2), ncol = 2)))
  return(c(L[1, 1], L[2, 1], L[2, 2]))
}


fMultivariateNormal_stan <- function(input){
  comps <- dCalculateChol(input$multivariatenormal_sigmax,
                          input$multivariatenormal_sigmay,
                          input$multivariatenormal_rho)
  switch(input$property,
         pdf=paste("// non-Cholesky",
           paste0("exp(multi_normal_lpdf(x| [", input$multivariatenormal_mux, ", ", input$multivariatenormal_muy, "], ", "[[", input$multivariatenormal_sigmax^2, ", ", input$multivariatenormal_sigmax * input$multivariatenormal_sigmay * input$multivariatenormal_rho, "], [", 
                    input$multivariatenormal_sigmax * input$multivariatenormal_sigmay * input$multivariatenormal_rho, ", ", input$multivariatenormal_sigmay^2, "]]))"),
           "// Cholesky (faster)",
           paste0("exp(multi_normal_cholesky_lpdf(x| [", input$multivariatenormal_mux, ", ", input$multivariatenormal_muy, "], ", "[[", comps[1], ", ", 0, "], [", 
                  comps[2], ", ", comps[3], "]]))"),
           sep = "\n"),
         log_pdf=paste("// non-Cholesky",
                       paste0("multi_normal_lpdf(x| [", input$multivariatenormal_mux, ", ", input$multivariatenormal_muy, "], ", "[[", input$multivariatenormal_sigmax^2, ", ", input$multivariatenormal_sigmax * input$multivariatenormal_sigmay * input$multivariatenormal_rho, "], [", 
                              input$multivariatenormal_sigmax * input$multivariatenormal_sigmay * input$multivariatenormal_rho, ", ", input$multivariatenormal_sigmay^2, "]])"),
                       "// Cholesky (faster)",
                       paste0("multi_normal_cholesky_lpdf(x| [", input$multivariatenormal_mux, ", ", input$multivariatenormal_muy, "], ", "[[", comps[1], ", ", 0, "], [", 
                              comps[2], ", ", comps[3], "]])"),
                       sep = "\n"),
         random=paste("// non-Cholesky",
                      paste0("multi_normal_rng(to_vector([", input$multivariatenormal_mux, ", ", input$multivariatenormal_muy, "]), ","[[", input$multivariatenormal_sigmax^2, ", ", input$multivariatenormal_sigmax * input$multivariatenormal_sigmay * input$multivariatenormal_rho, "], [", 
                       input$multivariatenormal_sigmax * input$multivariatenormal_sigmay * input$multivariatenormal_rho, ", ", input$multivariatenormal_sigmay^2, "]])"),
                      "// Cholesky (faster)",
                      paste0("multi_normal_cholesky_rng(to_vector([", input$multivariatenormal_mux, ", ", input$multivariatenormal_muy, "]), ", "[[", comps[1], ", ", 0, "], [", 
                             comps[2], ", ", comps[3], "]])"),
                      sep = "\n")
  )
  
}

fMultivariateT_stan <- function(input){
  switch(input$property,
         pdf=paste0("exp(multi_student_t_lpdf(x| ", input$multivariatet_df, ", [", input$multivariatet_mux, ", ", input$multivariatet_muy, "], ", "[[", input$multivariatet_sigmax^2, ", ", input$multivariatet_sigmax * input$multivariatet_sigmay * input$multivariatet_rho, "], [", 
                          input$multivariatet_sigmax * input$multivariatet_sigmay * input$multivariatet_rho, ", ", input$multivariatet_sigmay^2, "]]))"),
         log_pdf=paste0("multi_student_t_lpdf(x| ", input$multivariatet_df, ", [", input$multivariatet_mux, ", ", input$multivariatet_muy, "], ", "[[", input$multivariatet_sigmax^2, ", ", input$multivariatet_sigmax * input$multivariatet_sigmay * input$multivariatet_rho, "], [", 
                        input$multivariatet_sigmax * input$multivariatet_sigmay * input$multivariatet_rho, ", ", input$multivariatet_sigmay^2, "]])"),
         random=paste0("multi_student_t_rng(", input$multivariatet_df, ", to_vector([", input$multivariatet_mux, ", ", input$multivariatet_muy, "]), ", "[[", input$multivariatet_sigmax^2, ", ", input$multivariatet_sigmax * input$multivariatet_sigmay * input$multivariatet_rho, "], [", 
                       input$multivariatet_sigmax * input$multivariatet_sigmay * input$multivariatet_rho, ", ", input$multivariatet_sigmay^2, "]])")
  )
  
}

fMultinomial_stan <- function(input){
  theta <- c(input$multinomial_prob1,
             input$multinomial_prob2,
             input$multinomial_prob3)
  theta <- theta / sum(theta)
  switch(input$property,
         pdf=paste0("exp(multinomial_lpmf(x| to_vector([", theta[1], ", ", theta[2], ", ", theta[3], "])))"),
         log_pdf=paste0("multinomial_lpmf(x| to_vector([", theta[1], ", ", theta[2], ", ", theta[3], "]))"),
         random=paste0("multinomial_rng(to_vector([", theta[1], ", ", theta[2], ", ", theta[3], "]), ", input$multinomial_size, ")")
  )
  
}


fStanCode <- function(input){
  text <-
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fStanHelper("normal", c(input$normal_mu, input$normal_sigma), input),
             Uniform=fStanHelper("uniform", c(input$uniform_a, input$uniform_b), input),
             LogNormal=fStanHelper("lognormal", c(input$lognormal_mu,input$lognormal_sigma), input),
             Exponential=fStanHelper("exponential", input$exponential_rate, input),
             Gamma=fStanHelper("gamma", c(input$gamma_shape, input$gamma_rate), input),
             t=fStanHelper("student_t", c(input$t_nu, input$t_mu,input$t_sigma), input),
             Beta=fStanHelper("beta", c(input$beta_a,input$beta_b), input),
             Cauchy=fStanHelper("cauchy", c(input$cauchy_location,input$cauchy_scale), input),
             HalfCauchy=dhalfcauchy_stan(input),
             InverseGamma=fStanHelper("inv_gamma", c(input$inversegamma_shape, 1.0 / input$inversegamma_scale), input),
             InverseChiSquared=fStanHelper("inv_chi_square", input$inversechisquared_df, input),
             LogitNormal=flogitnormal_stan(input))
    }else if(input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=fStanHelper_discrete("bernoulli", input$bernoulli_prob, input),
             Binomial=fStanHelper_discrete("binomial", c(input$binomial_size, input$binomial_prob), input),
             Poisson=fStanHelper_discrete("poisson", input$poisson_lambda, input),
             NegativeBinomial=fStanHelper_discrete("neg_binomial_2", c(input$negativebinomial_mean, input$negativebinomial_dispersion), input),
             BetaBinomial=fStanHelper_discrete("beta_binomial", c(input$betabinomial_size, input$betabinomial_shape1, input$betabinomial_shape2), input)
             )
    }else if(input$distType=='Multivariate'){
      switch(input$dist2,
             MultivariateNormal=fMultivariateNormal_stan(input),
             MultivariateT=fMultivariateT_stan(input),
             Multinomial=fMultinomial_stan(input),
             Wishart=fStanHelper("wishart", input$wishart_df, input),
             InverseWishart=dInverseWishartFull(input$inversewishart_df, input),
             LKJ=fLKJ_1(input$lkj_eta, input$lkj_dimension, input),
             Dirichlet=if_else(input$dirichlet_dimension==2, fStanHelper("dirichlet", c(input$dirichlet_alpha1, input$dirichlet_alpha2), input, vector_params = TRUE, import="library(LaplacesDemon)"),
                               if_else(input$dirichlet_dimension==3, fStanHelper("dirichlet", c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3), input, vector_params = TRUE, import="library(LaplacesDemon)"),
                                       if_else(input$dirichlet_dimension==4, fStanHelper("dirichlet", c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3, input$dirichlet_alpha4), input, vector_params = TRUE, import="library(LaplacesDemon)"), "test")))
      )
    }
           
  
  return(prismCodeBlock(text, language = "r"))
}
