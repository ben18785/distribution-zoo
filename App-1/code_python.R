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

fPythonHelperDiscrete <- function(mainName, mainName1, params, params1, input, import=NULL, import1=NULL, named_arguments=NULL, vector_params=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName, ".pmf"),
                                params=params, prefixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params),
         log_pdf=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName, ".logpmf"),
                                    params=params, prefixparams="x",
                                    import=import, named_arguments=named_arguments,
                                    vector_params=vector_params),
         random=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName1, ".rvs"),
                                   params=params1, postfixparams="n",
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

dLogitNormal <- paste(
  "import numpy",
  "import scipy.special",
  "def logitnormal_pdf(x, mu, sigma):",
  "    temp = ((scipy.special.logit(x) - mu)**2 / (2 * sigma**2))",
  "    return (1.0 / sigma) * (1.0 / numpy.sqrt(2 * numpy.pi)) * np.exp(-temp) * (1.0 / (x * (1.0 - x)))",
  "# calling function",
  sep="\n"
)

dLogitNormal_log <- paste(
  "import numpy",
  "import scipy.special",
  "def logitnormal_logpdf(x, mu, sigma):",
  "    temp = ((scipy.special.logit(x) - mu)**2 / (2 * sigma**2))",
  "    return numpy.log((1.0 / sigma) * (1.0 / numpy.sqrt(2 * numpy.pi)) * np.exp(-temp) * (1.0 / (x * (1.0 - x))))",
  "# calling function",
  sep="\n"
)

rLogitNormal <- paste(
  "import scipy.stats",
  "import numpy",
  "def logistic(x):",
  "    return 1.0 / (1.0 + numpy.exp(-x))",
  "def logitnormal_rvs(mu, sigma, n=1):",
  "    x = scipy.stats.norm.rvs(mu, sigma, n)",
  "    p = [logistic(z) for z in x]",
  "    return p",
  "# calling function",
  sep="\n"
)

fLogitNormalFull <- function(input){
  switch(input$property,
         pdf=paste(dLogitNormal,
                   fMakeFunctionPaste(mainName="logitnormal_pdf",
                                      params=c(input$logitnormal_mu, input$logitnormal_sigma),
                                      prefixparams="x"),
                   sep="\n"),
         log_pdf=paste(dLogitNormal_log,
                       fMakeFunctionPaste(mainName="logitnormal_logpdf",
                                          params=c(input$logitnormal_mu, input$logitnormal_sigma),
                                          prefixparams="x"),
                       sep="\n"),
         random=paste(rLogitNormal,
                      fMakeFunctionPaste(mainName="logitnormal_rvs",
                                         params=c(input$logitnormal_mu, input$logitnormal_sigma),
                                         postfixparams="n"),
                      sep="\n")
  )
}

dNegativeBinomial <- paste(
  "import scipy.stats",
  "def negativebinomial_pmf(x, mu, kappa):",
  "    n = kappa",
  "    p = float(kappa) / (kappa + mu)",
  "    return scipy.stats.nbinom.pmf(x, n, p)",
  "# calling function",
  sep="\n"
)

dNegativeBinomial_log <- paste(
  "import scipy.stats",
  "def negativebinomial_logpmf(x, mu, kappa):",
  "    n = kappa",
  "    p = float(kappa) / (kappa + mu)",
  "    return scipy.stats.nbinom.logpmf(x, n, p)",
  "# calling function",
  sep="\n"
)

rNegativeBinomial <- paste(
  "import scipy.stats",
  "def negativebinomial_rvs(mu, kappa, n=1):",
  "    n1 = kappa",
  "    p = float(kappa) / (kappa + mu)",
  "    return scipy.stats.nbinom.rvs(n1, p, 0, n)",
  "# calling function",
  sep="\n"
)

fNegativeBinomialFull <- function(input){
  switch(input$property,
         pdf=paste(dNegativeBinomial,
                   fMakeFunctionPaste(mainName="negativebinomial_pmf",
                                      params=c(input$negativebinomial_mean, input$negativebinomial_dispersion),
                                      prefixparams="x"),
                   sep="\n"),
         log_pdf=paste(dNegativeBinomial_log,
                       fMakeFunctionPaste(mainName="negativebinomial_logpmf",
                                          params=c(input$negativebinomial_mean, input$negativebinomial_dispersion),
                                          prefixparams="x"),
                       sep="\n"),
         random=paste(rNegativeBinomial,
                      fMakeFunctionPaste(mainName="negativebinomial_rvs",
                                         params=c(input$negativebinomial_mean, input$negativebinomial_dispersion),
                                         postfixparams="n"),
                      sep="\n")
  )
}

dBetaBinomial <- paste(
  "import scipy.special",
  "def betabinomial_pmf(x, size, a, b):",
  "    return scipy.special.comb(size, x) * scipy.special.beta(x + a, size - x + b) / scipy.special.beta(a, b)",
  "# calling function",
  sep="\n"
)

dBetaBinomial_log <- paste(
  "import scipy.special",
  "def betabinomial_logpmf(x, size, a, b):",
  "    return (gammaln(size + 1) + gammaln(x + a) + gammaln(size - x + b) + gammaln(a + b) - \
        (gammaln(x + 1) + gammaln(size - x + 1) + gammaln(a) + gammaln(b) + gammaln(size + a + b)))",
  "# calling function",
  sep="\n"
)

rBetaBinomial <- paste(
  "import scipy.stats",
  "def betabinomial_rvs(size, a, b, n=1):",
  "    thetas = scipy.stats.beta.rvs(a, b, 0, 1, n)",
  "    x = [scipy.stats.binom.rvs(size, theta, 0, 1)[0] for theta in thetas]",
  "    return x",
  "# calling function",
  sep="\n"
)

fBetaBinomialFull <- function(input){
  switch(input$property,
         pdf=paste(dBetaBinomial,
                   fMakeFunctionPaste(mainName="betabinomial_pmf",
                                      params=c(input$betabinomial_size, input$betabinomial_shape1, input$betabinomial_shape2),
                                      prefixparams="x"),
                   sep="\n"),
         log_pdf=paste(dBetaBinomial_log,
                       fMakeFunctionPaste(mainName="betabinomial_logpmf",
                                          params=c(input$betabinomial_size, input$betabinomial_shape1, input$betabinomial_shape2),
                                          prefixparams="x"),
                       sep="\n"),
         random=paste(rBetaBinomial,
                      fMakeFunctionPaste(mainName="betabinomial_rvs",
                                         params=c(input$betabinomial_size, input$betabinomial_shape1, input$betabinomial_shape2),
                                         postfixparams="n"),
                      sep="\n")
  )
}


dMultivariateNormal <- paste(
  "import scipy.stats",
  "def normal2d_pdf(x, mux, muy, sigmax, sigmay, rho):",
  "    return scipy.stats.multivariate_normal.pdf(x, [mux, muy], [[sigmax**2, sigmax * sigmay * rho], [sigmax * sigmay * rho, sigmay**2]])",
  "# calling function",
  sep="\n"
)

dMultivariateNormal_log <- paste(
  "import scipy.stats",
  "def normal2d_logpdf(x, mux, muy, sigmax, sigmay, rho):",
  "    return scipy.stats.multivariate_normal.logpdf(x, [mux, muy], [[sigmax**2, sigmax * sigmay * rho], [sigmax * sigmay * rho, sigmay**2]])",
  "# calling function",
  sep="\n"
)

rMultivariateNormal <- paste(
  "import scipy.stats",
  "def normal2d_rvs(mux, muy, sigmax, sigmay, rho, n=1):",
  "    return scipy.stats.multivariate_normal.rvs([mux, muy], [[sigmax**2, sigmax * sigmay * rho], [sigmax * sigmay * rho, sigmay**2]], n)",
  "# calling function",
  sep="\n"
)

fMultivariateNormalFull <- function(input){
  switch(input$property,
         pdf=paste(dMultivariateNormal,
                   fMakeFunctionPaste(mainName="normal2d_pdf",
                                      params=c(input$multivariatenormal_mux,
                                               input$multivariatenormal_muy,
                                               input$multivariatenormal_sigmax,
                                               input$multivariatenormal_sigmay,
                                               input$multivariatenormal_rho),
                                      prefixparams="x"),
                   sep="\n"),
         log_pdf=paste(dMultivariateNormal_log,
                       fMakeFunctionPaste(mainName="normal2d_logpdf",
                                          params=c(input$multivariatenormal_mux,
                                                   input$multivariatenormal_muy,
                                                   input$multivariatenormal_sigmax,
                                                   input$multivariatenormal_sigmay,
                                                   input$multivariatenormal_rho),
                                          prefixparams="x"),
                       sep="\n"),
         random=paste(rMultivariateNormal,
                      fMakeFunctionPaste(mainName="normal2d_rvs",
                                         params=c(input$multivariatenormal_mux,
                                                  input$multivariatenormal_muy,
                                                  input$multivariatenormal_sigmax,
                                                  input$multivariatenormal_sigmay,
                                                  input$multivariatenormal_rho),
                                         postfixparams="n"),
                      sep="\n")
  )
}

dStudentt <- paste(
  "import scipy.special",
  "import numpy",
  "# general Student t",
  "def studentt_pdf(x, mu, sigma, nu):",
  "    p = len(mu)",
  "    first = scipy.special.gamma(0.5 * (nu + p)) / (scipy.special.gamma(nu / 2.0) * nu**(float(p) / 2) * numpy.pi**(float(p) / 2) * numpy.sqrt(numpy.linalg.det(sigma)))",
  "    x_minus_mu = numpy.array(x) - numpy.array(mu)",
  "    sigma_inv = numpy.linalg.inv(sigma)",
  "    second = (1 + (1.0 / float(nu)) * numpy.matmul(numpy.matmul(x_minus_mu, sigma_inv), numpy.transpose(x_minus_mu)))**(-0.5 * (nu + p))",
  "    return first * second",
  "# 2d Student t",
  "def studentt2d_pdf(x, mux, muy, sigmax, sigmay, rho, nu):",
  "    return studentt_pdf(x, [mux, muy], [[sigmax**2, sigmax * sigmay * rho], [sigmax * sigmay * rho, sigmay**2]], nu)",
  "# calling function",
  sep="\n"
)

dStudentt_log <- paste(
  "import scipy.special",
  "import numpy",
  "# general Student t",
  "def studentt_logpdf(x, mu, sigma, nu):",
  "    p = len(mu)",
  "    first = scipy.special.gamma(0.5 * (nu + p)) / (scipy.special.gamma(nu / 2.0) * nu**(float(p) / 2) * numpy.pi**(float(p) / 2) * numpy.sqrt(numpy.linalg.det(sigma)))",
  "    x_minus_mu = numpy.array(x) - numpy.array(mu)",
  "    sigma_inv = numpy.linalg.inv(sigma)",
  "    second = (1 + (1.0 / float(nu)) * numpy.matmul(numpy.matmul(x_minus_mu, sigma_inv), numpy.transpose(x_minus_mu)))**(-0.5 * (nu + p))",
  "    return numpy.log(first) + numpy.log(second)",
  "# 2d Student t",
  "def studentt2d_logpdf(x, mux, muy, sigmax, sigmay, rho, nu):",
  "    return studentt_logpdf(x, [mux, muy], [[sigmax**2, sigmax * sigmay * rho], [sigmax * sigmay * rho, sigmay**2]], nu)",
  "# calling function",
  sep="\n"
)

rStudentt <- paste(
  "import scipy.stats",
  "import numpy",
  "# general Student t",
  "def studentt_rvs(mu, sigma, nu, n=1):",
  "    y = scipy.stats.multivariate_normal.rvs(numpy.zeros(len(mu)), sigma, n)",
  "    u = scipy.stats.chi2.rvs(nu, size=n)",
  "    x = numpy.zeros((n, len(mu)))",
  "    for i in range(n):",
  "        x[i, :] = mu + y[i, :] / numpy.sqrt(u[i] / nu)",
  "    return x",
  "# 2d Student t",
  "def studentt2d_rvs(mux, muy, sigmax, sigmay, rho, nu, n=1):",
  "    return studentt_rvs([mux, muy], [[sigmax**2, sigmax * sigmay * rho], [sigmax * sigmay * rho, sigmay**2]], nu, n)",
  "# calling function",
  sep="\n"
)

fStudenttFull <- function(input){
  switch(input$property,
         pdf=paste(dStudentt,
                   fMakeFunctionPaste(mainName="studentt2d_pdf",
                                      params=c(input$multivariatet_mux,
                                               input$multivariatet_muy,
                                               input$multivariatet_sigmax,
                                               input$multivariatet_sigmay,
                                               input$multivariatet_rho,
                                               input$multivariatet_df),
                                      prefixparams="x"),
                   sep="\n"),
         log_pdf=paste(dStudentt_log,
                       fMakeFunctionPaste(mainName="studentt2d_logpdf",
                                          params=c(input$multivariatet_mux,
                                                   input$multivariatet_muy,
                                                   input$multivariatet_sigmax,
                                                   input$multivariatet_sigmay,
                                                   input$multivariatet_rho,
                                                   input$multivariatet_df),
                                          prefixparams="x"),
                       sep="\n"),
         random=paste(rStudentt,
                      fMakeFunctionPaste(mainName="studentt2d_rvs",
                                         params=c(input$multivariatet_mux,
                                                  input$multivariatet_muy,
                                                  input$multivariatet_sigmax,
                                                  input$multivariatet_sigmay,
                                                  input$multivariatet_rho,
                                                  input$multivariatet_df),
                                         postfixparams="n"),
                      sep="\n")
  )
}

fMultinomial <- function(input){
  a_sum <- sum(c(input$multinomial_prob1,
             input$multinomial_prob2,
             input$multinomial_prob3))
  lparams <- c(input$multinomial_prob1,
               input$multinomial_prob2,
               input$multinomial_prob3) / a_sum
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName="scipy.stats.multinomial.pmf",
                                params=lparams,
                                prefixparams="x",
                                vector_params = T,
                                python_vector=T,
                                import="import scipy.stats",
                                other_params=input$multinomial_size),
         log_pdf=fMakeFunctionPaste(mainName="float(scipy.stats.multinomial.logpmf",
                                    params=lparams,
                                    prefixparams="x",
                                    vector_params = T,
                                    python_vector=T,
                                    import="import scipy.stats",
                                    other_params=input$multinomial_size,
                                    end_brace = T),
         random=fMakeFunctionPaste(mainName="scipy.stats.multinomial.rvs",
                                   params=lparams,
                                   postfixparams="n",
                                   vector_params = T,
                                   python_vector=T,
                                   import="import scipy.stats",
                                   other_params=input$multinomial_size)
  )
}

fWishart <- function(input){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName="scipy.stats.wishart.pdf",
                                params=input$wishart_df,
                                prefixparams="x",
                                postfixparams = "S",
                                import="import scipy.stats\n# note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)"),
         log_pdf=fMakeFunctionPaste(mainName="scipy.stats.wishart.logpdf",
                                params=input$wishart_df,
                                prefixparams="x",
                                postfixparams = "S",
                                import="import scipy.stats\n# note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)"),
         random=fMakeFunctionPaste(mainName="scipy.stats.wishart.rvs",
                                   params=input$wishart_df,
                                   postfixparams = c("S", "n"),
                                   import="import scipy.stats\n# note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)")
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
             InverseChiSquared=fInverseChiSquaredFull(input),
             LogitNormal=fLogitNormalFull(input)
      )
    }else if(input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=fPythonHelperDiscrete("bernoulli", "bernoulli",
                                     params=input$bernoulli_prob,
                                     params1=c(input$bernoulli_prob, 0),
                                     input, import="import scipy.stats",
                                     import1="import scipy.stats"),
             Binomial=fPythonHelperDiscrete("binom", "binom",
                                            params=c(input$binomial_size, input$binomial_prob),
                                            params1=c(input$binomial_size, input$binomial_prob, 0),
                                            input, import="import scipy.stats",
                                            import1="import scipy.stats"),
             Poisson=fPythonHelperDiscrete("poisson", "poisson",
                                           params=input$poisson_lambda,
                                           params1=c(input$poisson_lambda, 0),
                                           input, import="import scipy.stats",
                                           import1="import scipy.stats"),
             NegativeBinomial=fNegativeBinomialFull(input),
             BetaBinomial=fBetaBinomialFull(input)
      )
    } else if(input$distType=="Multivariate"){
      switch(input$dist2,
             MultivariateNormal=fMultivariateNormalFull(input),
             MultivariateT=fStudenttFull(input),
             Multinomial=fMultinomial(input),
             Wishart=fWishart(input)
      )
    }
  return(prismCodeBlock(text, language = "python"))
}
