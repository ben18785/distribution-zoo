
fRHelper <- function(mainName, params, input, import=NULL, named_arguments=NULL, vector_params=FALSE){
  switch(input$property,
        pdf=fMakeFunctionPaste(mainName=paste0("d", mainName),
                               params=params, prefixparams="x",
                               import=import, named_arguments=named_arguments,
                               vector_params=vector_params),
        log_pdf=fMakeFunctionPaste(mainName=paste0("d", mainName),
                                   params=params, prefixparams="x",
                                   postfixparams="log=TRUE",
                                   import=import, named_arguments=named_arguments,
                                   vector_params=vector_params),
        random=fMakeFunctionPaste(mainName=paste0("r", mainName),
                                  params=params, prefixparams="n",
                                  import=import, named_arguments=named_arguments,
                                  vector_params=vector_params))
}

dHalfCauchyCode <- paste(
  "dhalfcauchy <- function(x, location, scale, log=FALSE){",
  "  if(x >= 0)",
  "    val <- 1.0 / (pi * (1 + ((x - location) / scale)^2) * scale * (0.5 + atan(location / scale) / pi))",
  "  else",
  "    val <- 0.0",
  "  if(!log)",
  "    return(val)",
  "  else",
  "    return(log(val))",
  "}",
  "# calling function",
  sep = "\n")

rHalfCauchyCode <- paste(
  "rhalfcauchy <- function(n, location, scale){",
  "  r <- vector(length=n)",
  "  for(i in 1:n){",
  "    r_1 <- rcauchy(1, location, scale)",
  "    while(r_1 < 0)",
  "      r_1 <- rcauchy(1, location, scale)",
  "    r[i] <- r_1",
  "  }",
  "  return(r)
  }",
  "# calling function",
  sep = "\n")

dHalfCauchyFull <- function(location, scale, input){
  if(input$property!="random")
    paste(dHalfCauchyCode,
          fRHelper("halfcauchy", c(location, scale), input),
          sep="\n")
  else
    paste(rHalfCauchyCode,
          fRHelper("halfcauchy", c(location, scale), input),
          sep="\n")
}

dDiscreteUniformCode <- paste("# function definition",
                              "ddiscreteuniform <- function(x, min=0, max=1, log=FALSE){",
                              "if(x >= min & x <= max & round(x) == x)",
                              "  if(!log)",
                              "    return(1 / (max - min + 1))",
                              "  else",
                              "    return(-log(max - min + 1))",
                              "else",
                              "  if(!log)",
                              "    return(0.0)",
                              "  else",
                              "    return(-Inf)",
                              "}",
                              "# calling function",
                              sep = "\n")
rDiscreteUniformCode <- paste("# function definition",
                              "rdiscreteuniform <- function(n, min=0, max=1){",
                              "  return(sample(min:max, n, replace=T))",
                              "}",
                              sep = "\n")

dDiscreteUniformFull <- function(lower, upper, input){
  if(input$property!="random")
    paste(dDiscreteUniformCode,
          fRHelper("discreteuniform", c(lower, upper), input),
          sep="\n")
  else
    paste(rDiscreteUniformCode,
          fRHelper("discreteuniform", c(lower, upper), input),
          sep="\n")
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
    paste0("rmultinom(n, ", input$multinomial_size,
           ", prob=c(", input$multinomial_prob1, ", ", input$multinomial_prob2, ", ", input$multinomial_prob3, "))")
  }
}

dWishartFull <- function(df, input){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName="dwishart",
                                params=df, prefixparams="x",
                                import=paste("library(LaplacesDemon)",
                                             "# note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)",
                                             sep="\n"),
                                postfixparams = "S"),
         log_pdf=fMakeFunctionPaste(mainName="dwishart",
                                params=df, prefixparams="x",
                                import=paste("library(LaplacesDemon)",
                                             "# note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)",
                                             sep="\n"),
                                postfixparams=c("S", "log=TRUE")),
         random=fMakeFunctionPaste(mainName="rWishart",
                                params=df, prefixparams="n",
                                import=paste("# note S is symmetric positive-definite matrix",
                                             sep="\n"),
                                postfixparams = "S"))
}

dInverseWishartFull <- function(df, input){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName="dinvwishart",
                                params=df, prefixparams="x",
                                import=paste("library(LaplacesDemon)",
                                             "# note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)",
                                             sep="\n"),
                                postfixparams = "S"),
         log_pdf=fMakeFunctionPaste(mainName="dinvwishart",
                                    params=df, prefixparams="x",
                                    import=paste("library(LaplacesDemon)",
                                                 "# note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)",
                                                 sep="\n"),
                                    postfixparams=c("S", "log=TRUE")),
         random=fMakeFunctionPaste(mainName="lapply(seq(1, n, 1), function(i) rinvwishart",
                                   params=df,
                                   import=paste("library(LaplacesDemon)",
                                                "# note S is symmetric positive-definite matrix",
                                                sep="\n"),
                                   postfixparams = "S)"))
}


dLKJ_1 <- paste(
  "dlkj <- function(x, nu, log=FALSE){",
  "  d <- nrow(x)",
  "  if(sum(diag(x)) != d | x[upper.tri(x)] != x[lower.tri(x)])",
  "    return(ifelse(!log, 0.0, -Inf))",
  "  det_a <- det(x) ^ (nu - 1)",
  "  a_sum <- 0",
  "  b_sum <- 1",
  "  k <- 1",
  "  for(i in 1:(d - 1)){",
  "    a_sum <- a_sum + (2 * nu - 2 + d - k) * (d - k)",
  "    b_sum <- b_sum * (beta(nu + 0.5 * (d - k - 1), nu + 0.5 * (d - k - 1)) ^ (d - k))",
  "    k <- k + 1",
  "  }",
  "  a_sum <- 2 ^ a_sum",
  "  if(!log)",
  "    return(a_sum * b_sum * det_a)",
  "  else",
  "    return(log(a_sum) + log(b_sum) + log(det_a))",
  "}",
  "# calling function",
  sep="\n"
)

rLKJ_1 <- paste(
  "rlkj <- function(n, nu, d){",
  "  r_list <- vector(length = n, mode = 'list')",
  "  for(i in 1:n){",
  "    if(d==1)",
  "      r = as.array(1)",
  "    else if(d==2){",
  "      rho <- 2 * rbeta(1, nu, nu) - 1",
  "      r <- matrix(c(1, rho, rho, 1), ncol = 2)",
  "    }else{",
  "      beta <- nu + (d - 2) / 2",
  "      u <- rbeta(1, beta, beta)",
  "      r_12 <- 2 * u - 1",
  "      r <- matrix(c(1, r_12, r_12, 1), ncol = 2)",
  "      for(m in 2:(d - 1)){",
  "        beta <- beta - 0.5",
  "        y <- rbeta(1, m / 2, beta)",
  "        a <- rnorm(m)",
  "        anorm <- sqrt(sum(a^2))",
  "        u <- a / anorm",
  "        w <- sqrt(y) * u",
  "        A <- chol(r)",
  "        z <- w%*%A",
  "        r <-cbind(r, t(z))",
  "        r <- rbind(r, c(z, 1))",
  "      }",
  "    }",
  "    r_list[[i]] <- r",
  "  }",
  "  return(r_list)",
  "}",
  "# calling function",
  sep="\n"
)

fLKJ_1 <- function(eta, d, input){
  if(input$property!="random")
    paste(dLKJ_1,
          fRHelper("lkj", eta, input),
          sep="\n")
  else
    paste(rLKJ_1,
          fRHelper("lkj", c(eta, d), input),
          sep="\n")
}

fLogitNormal_R <- function(input){
  switch(input$property,
         pdf=fRHelper("logitnorm", c(input$logitnormal_mu, input$logitnormal_sigma), input, import="library(logitnorm)"),
         log_pdf=fRHelper("logitnorm", c(input$logitnormal_mu, input$logitnormal_sigma), input, import="library(logitnorm)"),
         random=fMakeFunctionPaste(mainName=paste0("r", "logitnorm"),
                                   params=c(input$logitnormal_mu, input$logitnormal_sigma), postfixparams="n",
                                   import="library(logitnorm)")
           )
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
             HalfCauchy=dHalfCauchyFull(input$halfcauchy_location, input$halfcauchy_scale, input),
             InverseGamma=fRHelper("invgamma", c(input$inversegamma_shape, 1 / input$inversegamma_scale), input, import="library(actuar)"),
             InverseChiSquared=fRHelper("invchisq", input$inversechisquared_df, input, import="library(LaplacesDemon)"),
             LogitNormal=fLogitNormal_R(input))
    }else if(input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=fRHelper("binom", c(1, input$bernoulli_prob), input),
             Binomial=fRHelper("binom", c(input$binomial_size, input$binomial_prob), input),
             DiscreteUniform=dDiscreteUniformFull(input$discreteuniform_lower, input$discreteuniform_upper, input),
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
             Multinomial=dMultinomialFull(input$multinomial_prob1, input$multinomial_prob2, input$multinomial_prob3, input),
             Wishart=dWishartFull(input$wishart_df, input),
             InverseWishart=dInverseWishartFull(input$inversewishart_df, input),
             LKJ=fLKJ_1(input$lkj_eta, input$lkj_dimension, input),
             Dirichlet=if_else(input$dirichlet_dimension==2, fRHelper("dirichlet", c(input$dirichlet_alpha1, input$dirichlet_alpha2), input, vector_params = TRUE, import="library(LaplacesDemon)"),
                               if_else(input$dirichlet_dimension==3, fRHelper("dirichlet", c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3), input, vector_params = TRUE, import="library(LaplacesDemon)"),
                                       if_else(input$dirichlet_dimension==4, fRHelper("dirichlet", c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3, input$dirichlet_alpha4), input, vector_params = TRUE, import="library(LaplacesDemon)"), "test")))
      )
    }
           
  
  return(prismCodeBlock(text, language = "r"))
}
