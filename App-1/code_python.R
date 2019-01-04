fPythonHelper <- function(mainName, mainName1, params, input, import=NULL, import1=NULL, named_arguments=NULL, vector_params=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName, ".pdf"),
                                params=params, prefixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params),
         log_pdf=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName, ".logpdf"),
                                    params=params, prefixparams="x",
                                    import=import, named_arguments=named_arguments,
                                    vector_params=vector_params),
         random=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName1, ".rvs"),
                                   params=params, postfixparams="n",
                                   import=import1, named_arguments=named_arguments,
                                   vector_params=vector_params))
}

fLognormal <- function(input){
  mainName <- "lognorm"
  mainName1 <- "lognorm"
  import <- "import scipy.stats"
  import1 <- "import scipy.stats"
  named_arguments <- NULL
  vector_params <- NULL
  named_arguments <- c("scale", "s")
  params <- c(exp(input$lognormal_mu), input$lognormal_sigma)
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName, ".pdf"),
                                params=params, prefixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params),
         log_pdf=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName, ".logpdf"),
                                    params=params, prefixparams="x",
                                    import=import, named_arguments=named_arguments,
                                    vector_params=vector_params),
         random=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName1, ".rvs"),
                                   params=c(exp(input$lognormal_mu), input$lognormal_sigma),
                                   import=import1, named_arguments=c("scale", "s"),
                                   postfixparams="size=n",
                                   vector_params=vector_params)
  )
}


fExponential <- function(input){
  switch(input$property,
         pdf=fPythonHelper("expon", "expon",
                c(0, 1 / input$exponential_rate),
                input, import="import scipy.stats",
                import1="import scipy.stats",
                named_arguments = c("loc", "scale")),
         log_pdf=fPythonHelper("expon", "expon",
                               c(0, 1 / input$exponential_rate),
                               input, import="import scipy.stats",
                               import1="import scipy.stats",
                               named_arguments = c("loc", "scale")),
         random=fMakeFunctionPaste(mainName="numpy.random.exponential",
                                   params=1 / input$exponential_rate,
                                   import="import numpy",
                                   postfixparams="n")
  )
}

fGamma <- function(input){
  switch(input$property,
         pdf=fPythonHelper("gamma", "gamma",
                           c(input$gamma_shape, 0, 1.0 / input$gamma_rate),
                           input, import="import scipy.stats",
                           import1="import scipy.stats",
                           named_arguments = c("a", "loc", "scale")),
         log_pdf=fPythonHelper("gamma", "gamma",
                               c(input$gamma_shape, 0, 1.0 / input$gamma_rate),
                               input, import="import scipy.stats",
                               import1="import scipy.stats",
                               named_arguments = c("a", "loc", "scale")),
         random=fMakeFunctionPaste(mainName="numpy.random.gamma",
                                   params=c(input$gamma_shape, 1.0 / input$gamma_rate),
                                   import="import numpy",
                                   postfixparams="n")
  )
}

fInverseGamma <- function(input){
  switch(input$property,
         pdf=fPythonHelper("invgamma", "invgamma",
                           c(input$inversegamma_shape, 0, input$inversegamma_scale),
                           input, import="import scipy.stats",
                           import1="import scipy.stats",
                           named_arguments = c("a", "loc", "scale")),
         log_pdf=fPythonHelper("invgamma", "invgamma",
                               c(input$inversegamma_shape, 0, input$inversegamma_scale),
                               input, import="import scipy.stats",
                               import1="import scipy.stats",
                               named_arguments = c("a", "loc", "scale")),
         random=fMakeFunctionPaste(mainName="scipy.stats.invgamma.rvs",
                                   params=c(input$inversegamma_shape, 0, input$inversegamma_scale),
                                   import="import scipy.stats",
                                   postfixparams="size=n",
                                   named_arguments = c("a", "loc", "scale"))
  )
}


fBeta <- function(input){
  switch(input$property,
    pdf=fPythonHelper("beta", "beta",
                     c(input$beta_a, input$beta_b),
                     input, import="import scipy.stats",
                     import1="import scipy.stats"),
    log_pdf=fPythonHelper("beta", "beta",
                          c(input$beta_a, input$beta_b),
                          input, import="import scipy.stats",
                          import1="import scipy.stats"),
    random=fMakeFunctionPaste(mainName="numpy.random.beta",
                              params=c(input$beta_a, input$beta_b),
                              import="import numpy",
                              postfixparams="n")
  )
}

dInverseChiSquared <- paste(
  "import numpy",
  "import scipy.special",
  "def inversechisquared_pdf(x, df):",
  "    temp = df / 2.0",
  "    val = (2**(-temp) / scipy.special.gamma(temp)) * x**-(temp + 1) * numpy.exp(-1.0 / (2 * x))",
  "    return val",
  "# calling function",
  sep="\n"
)

dInverseChiSquared_log <- paste(
  "import numpy",
  "import scipy.special",
  "def inversechisquared_logpdf(x, df):",
  "    temp = df / 2.0",
  "    val = (2**(-temp) / scipy.special.gamma(temp)) * x**-(temp + 1) * numpy.exp(-1.0 / (2 * x))",
  "    return numpy.log(val)",
  "# calling function",
  sep="\n"
)

rInverseChiSquared <- paste(
  "import scipy.stats",
  "def inversechisquared_rvs(df, n=1):",
  "    r = scipy.stats.chi2.rvs(df, 0, 1, n)",
  "    return 1.0 / r",
  "# calling function",
  sep="\n"
)


fInverseChiSquaredFull <- function(input){
  switch(input$property,
         pdf=paste(dInverseChiSquared,
                   fMakeFunctionPaste(mainName="inversechisquared_pdf",
                                params=input$inversechisquared_df, prefixparams="x"),
                   sep="\n"),
         log_pdf=paste(dInverseChiSquared_log,
                       fMakeFunctionPaste(mainName="inversechisquared_logpdf",
                                          params=input$inversechisquared_df, prefixparams="x"),
                       sep="\n"),
         random=paste(rInverseChiSquared,
                     fMakeFunctionPaste(mainName="inversechisquared_rvs",
                                        params=input$inversechisquared_df, postfixparams="n"),
                     sep="\n")
  )
}

fPythoncode <- function(input){
  text <-
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fPythonHelper("norm", "norm",
                                  c(input$normal_mu, input$normal_sigma),
                                  input, import="import scipy.stats",
                                  import1="import scipy.stats"),
             Uniform=fPythonHelper("uniform", "uniform",
                                  c(input$uniform_a, input$uniform_b),
                                  input, import="import scipy.stats",
                                  import1="import scipy.stats"),
             LogNormal=fLognormal(input),
             Exponential=fExponential(input),
             Gamma=fGamma(input),
             t=fPythonHelper("t", "t",
                                   c(input$t_nu, input$t_mu, input$t_sigma),
                                   input, import="import scipy.stats",
                                   import1="import scipy.stats"),
             Beta=fBeta(input),
             Cauchy=fPythonHelper("cauchy", "cauchy",
                             c(input$cauchy_location, input$cauchy_scale),
                             input, import="import scipy.stats",
                             import1="import scipy.stats"),
             HalfCauchy=fPythonHelper("halfcauchy", "halfcauchy",
                                  c(input$halfcauchy_location, input$halfcauchy_scale),
                                  input, import="import scipy.stats",
                                  import1="import scipy.stats"),
             InverseGamma=fInverseGamma(input),
             InverseChiSquared=fInverseChiSquaredFull(input)
      )
    }
  return(prismCodeBlock(text, language = "python"))
}
