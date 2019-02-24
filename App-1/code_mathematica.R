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

dMultivariatet_mathematica <- function(mux, muy, sigmax, sigmay, rho, df){
  top <- paste0("aDist=MultivariateTDistribution[{", mux, ", ", muy, "}, {{", sigmax^2, ", ", sigmax * sigmay * rho, "}, {", sigmax * sigmay * rho, ", ",  
                sigmay^2, "}}", ", ", df, "]")
}

fMultivariatet_mathematica <- function(input){
  top <- dMultivariatet_mathematica(input$multivariatet_mux,
                                    input$multivariatet_muy,
                                    input$multivariatet_sigmax,
                                    input$multivariatet_sigmay,
                                    input$multivariatet_rho,
                                    input$multivariatet_df)
  switch(input$property,
         pdf=paste(top, "PDF[aDist, x]", sep = "\n"),
         log_pdf=paste(top, "Log@PDF[aDist, x]", sep = "\n"),
         random=paste(top, "RandomVariate[aDist, n]", sep = "\n")
  )
}

fMultinomial_mathematica <- function(input){
  comps <- c(input$multinomial_prob1, input$multinomial_prob2, input$multinomial_prob3)
  comps <- comps / sum(comps)
  top <- paste0("aDist=MultinomialDistribution[", input$multinomial_size, ", ", "{", comps[1], ", ",
                comps[2], ", ", comps[3], "}]")
  switch(input$property,
         pdf=paste(top, "PDF[aDist, x]", sep = "\n"),
         log_pdf=paste(top, "Log@PDF[aDist, x]", sep = "\n"),
         random=paste(top, "RandomVariate[aDist, n]", sep = "\n")
  )
}

fWishart_mathematica <- function(input){
  topper <- paste("(* S must be symmetric and positive definite *)",
                  "Needs[ \"MultivariateStatistics`\"];",
                  sep = "\n")
  top <- paste0("aDist=WishartDistribution[S, ", input$wishart_df, "]")
  switch(input$property,
         pdf=paste(topper, top, "PDF[aDist, x]", sep = "\n"),
         log_pdf=paste(topper, top, "Log@PDF[aDist, x]", sep = "\n"),
         random=paste(topper, top, "RandomVariate[aDist, n]", sep = "\n")
  )
}

dInverseWishart_mathematica <- paste(
  "multivariateGamma[p_, a_] := \\[Pi]^(p (p - 1)/4) Product[Gamma[a + (1 - j)/2], {j, 1, p}]",
  " ",
  "inverseWishartPDF[X_, \\[Nu]_ /; \\[Nu] > 0, S_] := Module[{d = Dimensions[X][[1]]},",
  "  Det[S]^(\\[Nu]/ 2) Det[X]^(-(\\[Nu] + d + 1)/2) Exp[-Tr[S.Inverse[X]]/2]",
  "  1 / (2^(\\[Nu] d/2) multivariateGamma[d, \\[Nu]/2])]",
  sep="\n"
)

fInverseWishart_mathematica <- function(input){
  topper <- paste0("(* S must be symmetric and positive definite *)")
  top <- paste0("aDist=InverseWishartMatrixDistribution[", input$inversewishart_df, ", S]")
  switch(input$property,
         pdf=paste(topper, dInverseWishart_mathematica, " ", paste0("inverseWishartPDF[x, ",input$inversewishart_df, ", S]"), sep = "\n"),
         log_pdf=paste(topper, dInverseWishart_mathematica, " ", paste0("Log@inverseWishartPDF[x, ",input$inversewishart_df, ", S]"), sep = "\n"),
         random=paste(topper, top, "RandomVariate[aDist, n]", sep = "\n")
  )
}

dLKJ_mathematica <- paste(
  "LKJPDF[X_, \\[Nu]_] := Module[{d = Dimensions[X][[1]], aSum, bProd},",
  "  If[Or[Or[Total[Diagonal[X]] != d, Total[Total[LowerTriangularize[X]]] != Total[Total[UpperTriangularize[X]]]], !PositiveDefiniteMatrixQ[X]], Return[0.0]];",
  "  aSum = 2^Sum[(2 \\[Nu] - 2 + d - k) (d - k), {k, 1, d - 1}];",
  "  bProd = Product[Beta[\\[Nu] + 0.5 (d - k - 1), \\[Nu] + 0.5 (d - k - 1)], {k, 1, d - 1}];",
  "  aSum bProd Det[X]^(\\[Nu] - 1)]",
  " ",
  "(*Calling function*)",
  sep="\n"
)

dLKJ_log_mathematica <- paste(
  "LKJLogPDF[X_, \\[Nu]_] := Module[{d = Dimensions[X][[1]], aSum, bProd},",
  "  If[Or[Or[Total[Diagonal[X]] != d, Total[Total[LowerTriangularize[X]]] != Total[Total[UpperTriangularize[X]]]], !PositiveDefiniteMatrixQ[X]], Return[-Infinity]];",
  "  aSum = 2^Sum[(2 \\[Nu] - 2 + d - k) (d - k), {k, 1, d - 1}];",
  "  bProd = Product[Beta[\\[Nu] + 0.5 (d - k - 1), \\[Nu] + 0.5 (d - k - 1)], {k, 1, d - 1}];",
  "  Log[aSum] + Log[bProd] + (\\[Nu] - 1) Log[Det[X]]]",
  " ",
  "(*Calling function*)",
  sep="\n"
)

rLKJ_mathematica <- paste(
  "LKJRandomOne[\\[Nu]_ /; \\[Nu] > 0, d_ /; And[d > 0, IntegerQ[d]]] :=",
  "  Module[{r, rho, beta, u, r12, y, a, anorm, w, A, z},",
  "    If[d == 1, r = {1}, If[d == 2, rho = 2 RandomVariate[BetaDistribution[\\[Nu], \\[Nu]], 1][[1]];",
  "      r = {{1, rho}, {rho, 1}}, beta = \\[Nu] + (d - 2)/2; ",
  "      u = RandomVariate[BetaDistribution[beta, beta], 1][[1]];",
  "      r12 = 2 u - 1;",
  "      r = {{1, r12}, {r12, 1}};",
  "      Table[beta = beta - 0.5; y = RandomVariate[BetaDistribution[m/2, beta], 1][[1]];",
  "        a = RandomVariate[NormalDistribution[], m];",
  "        anorm = Sqrt[Total[Flatten[a]^2]];",
  "        u = a / anorm;",
  "        w = Sqrt[y] u;",
  "        A = CholeskyDecomposition[r];",
  "        z = w.A;",
  "        r = ArrayFlatten[{{r, Transpose[{z}]}, {{z}, 1}}];, {m, 2, d - 1, 1}]]]; r]",
  "  ",
  "LKJRandom[n_Integer, \\[Nu]_ /; \\[Nu] > 0, d_ /; And[d > 0, IntegerQ[d]]] :=",
  "  Table[LKJRandomOne[\\[Nu], d], {n}]",
  "(*Calling function*)",
  sep="\n"
)


fLKJ_mathematica <- function(input){
  switch(input$property,
         pdf=paste(dLKJ_mathematica,
                   paste0("LKJPDF[x, ", input$lkj_eta, "]"),
                   sep = "\n"),
         log_pdf=paste(dLKJ_log_mathematica,
                       paste0("LKJLogPDF[x, ", input$lkj_eta, "]"),
                       sep = "\n"),
         random=paste(rLKJ_mathematica,
                      paste0("LKJRandom[n, ", input$lkj_eta, ", ", input$lkj_dimension, "]"),
                      sep = "\n")
  )
}

fDirichlet_mathematica <- function(input){
  if(input$dirichlet_dimension==2){
    alpha <- c(input$dirichlet_alpha1, input$dirichlet_alpha2)
  }else if(input$dirichlet_dimension==3){
    alpha <- c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3)
  }else if(input$dirichlet_dimension==4){
    alpha <- c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3, input$dirichlet_alpha4)
  }
  top <- paste0("DirichletDistribution[{", paste(alpha, collapse = ", "), "}]")
  switch(input$property,
         pdf=paste("(* Note that x is without last (redundant) dimension *)", paste0("PDF[", top, ", x]"), sep="\n"),
         log_pdf=paste("(* Note that x is without last (redundant) dimension *)", paste0("Log@PDF[", top, ", x]"), sep="\n"),
         random=paste("(* Note that random variates are without last (redundant) dimension *)", paste0("RandomVariate[", top, ", n]"), sep="\n")
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
             DiscreteUniform=fMathematicaHelper("DiscreteUniform",
                                         params = c(input$discreteuniform_lower, input$discreteuniform_upper), 
                                         input,
                                         vector_params = T,
                                         mathematica_vector = T),
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
             MultivariateNormal=fMultivariatenormal_mathematica(input),
             MultivariateT=fMultivariatet_mathematica(input),
             Multinomial=fMultinomial_mathematica(input),
             Wishart=fWishart_mathematica(input),
             InverseWishart=fInverseWishart_mathematica(input),
             LKJ=fLKJ_mathematica(input),
             Dirichlet=fDirichlet_mathematica(input)
      )
    }
  return(prismCodeBlock(text))
}
