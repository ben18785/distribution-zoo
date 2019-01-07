fMatlabHelper <- function(mainName, params, input, import=NULL, named_arguments=NULL, vector_params=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0(mainName, "pdf"),
                                params=params, prefixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params),
         log_pdf=paste0(fMakeFunctionPaste(mainName=paste0("log(", mainName, "pdf"),
                                    params=params, prefixparams="x",
                                    import=import, named_arguments=named_arguments,
                                    vector_params=vector_params),
                        ")"),
         random=fMakeFunctionPaste(mainName=paste0(mainName, "rnd"),
                                   params=params, postfixparams="[n, 1]",
                                   import=import, named_arguments=named_arguments,
                                   vector_params=vector_params))
}

dStudentt_matlab <- paste(
  "function f = studenttpdf(x, mu, sigma, nu)",
  "    numer = (nu / (nu + ((x - mu) / sigma)^2))^((nu + 1) / 2);",
  "    f = numer / (sqrt(nu) * sigma * beta(nu / 2, 1 / 2));",
  "end",
  "% calling function",
  sep = "\n"
)

rStudentt_matlab <- paste(
  "function x = studenttrnd(mu, sigma, nu, M)",
  "    y = trnd(nu, M);",
  "    x = sigma * y + mu;",
  "end",
  "% calling function",
  sep = "\n"
)

fStudentt_matlab <- function(input){
  switch(input$property,
         pdf=paste(dStudentt_matlab,
                   fMatlabHelper("studentt",
                           params = c(input$t_mu,input$t_sigma, input$t_nu),
                           input),
           sep = "\n"),
         log_pdf=paste(dStudentt_matlab,
                       fMatlabHelper("studentt",
                               params = c(input$t_mu,input$t_sigma, input$t_nu),
                               input),
                       sep = "\n"),
         random=paste(rStudentt_matlab,
                      fMatlabHelper("studentt",
                              params = c(input$t_mu,input$t_sigma, input$t_nu),
                              input),
                      sep = "\n")
  )
}

dCauchy_matlab <- paste(
  "function f = cauchypdf(x, a, b)",
  "    f = b ./ (pi * (b.^2 + (x - a).^2));",
  "end",
  "% calling function",
  sep = "\n"
)

rCauchy_matlab <- paste(
  "function x = cauchyrnd(a, b, M)",
  "    y = trnd(1, M);",
  "    x = b * y + a;",
  "end",
  "% calling function",
  sep = "\n"
)

fCauchy_matlab <- function(input){
  lparams <- c(input$cauchy_location, input$cauchy_scale)
  switch(input$property,
         pdf=paste(dCauchy_matlab,
                   fMatlabHelper("cauchy",
                                 params = lparams,
                                 input),
                   sep = "\n"),
         log_pdf=paste(dCauchy_matlab,
                       fMatlabHelper("cauchy",
                                     params = lparams,
                                     input),
                       sep = "\n"),
         random=paste(rCauchy_matlab,
                      fMatlabHelper("cauchy",
                                    params = lparams,
                                    input),
                      sep = "\n")
  )
}

dHalfCauchy_matlab <- paste(
  "function f = halfcauchypdf(x, a, b)",
  "    if x < 0",
  "        f = 0",
  "    else",
  "        fun = @(y) b ./ (pi * (b.^2 + (y - a).^2));",
  "        c = integral(fun, 0, Inf)",
  "        f = (1 / c) * b ./ (pi * (b.^2 + (x - a).^2));",
  "    end",
  "end",
  "% calling function",
  sep = "\n"
)

rHalfCauchy_matlab <- paste(
  "function x = cauchyrnd(a, b, M)",
  "    y = trnd(1, M);",
  "    x = b * y + a;",
  "end",
  " ",
  "function x = halfcauchysinglernd(a, b)",
  "    x = cauchyrnd(a, b, 1);",
  "    while x < 0",
  "        x = cauchyrnd(a, b, 1);",
  "    end",
  "end",
  " ",
  "function x = halfcauchyrnd(a, b, M)",
  "    x = zeros(M);",
  "    n = numel(x);",
  "    y = zeros([n, 1]);",
  "    for i = 1:n",
  "        y(i) = halfcauchysinglernd(a, b);",
  "    end",
  "    x = reshape(y, M);",
  "end",
  "% calling function",
  sep = "\n"
)

fHalfCauchy_matlab <- function(input){
  lparams <- c(input$halfcauchy_location, input$halfcauchy_scale)
  switch(input$property,
         pdf=paste(dHalfCauchy_matlab,
                   fMatlabHelper("halfcauchy",
                                 params = lparams,
                                 input),
                   sep = "\n"),
         log_pdf=paste(dHalfCauchy_matlab,
                       fMatlabHelper("halfcauchy",
                                     params = lparams,
                                     input),
                       sep = "\n"),
         random=paste(rHalfCauchy_matlab,
                      fMatlabHelper("halfcauchy",
                                    params = lparams,
                                    input),
                      sep = "\n")
  )
}

dInverseGamma_matlab <- paste(
  "function f = inversegammapdf(x, alpha, beta)",
  "    if x < 0",
  "        f = 0;",
  "    else",
  "        f = (beta^alpha) / gamma(alpha) * x^(-alpha-1) * exp(-beta / x);",
  "    end",
  "end",
  "% calling function",
  sep = "\n"
)

rInverseGamma_matlab <- paste(
  "function x = inversegammarnd(alpha, beta, M)",
  "    y = gamrnd(alpha, 1 / beta, M);",
  "    x = 1 ./ y;",
  "end",
  "% calling function",
  sep = "\n"
)

fInverseGamma_matlab <- function(input){
  lparams <- c(input$inversegamma_shape, input$inversegamma_scale)
  switch(input$property,
         pdf=paste(dInverseGamma_matlab,
                   fMatlabHelper("inversegamma",
                                 params = lparams,
                                 input),
                   sep = "\n"),
         log_pdf=paste(dInverseGamma_matlab,
                       fMatlabHelper("inversegamma",
                                     params = lparams,
                                     input),
                       sep = "\n"),
         random=paste(rInverseGamma_matlab,
                      fMatlabHelper("inversegamma",
                                    params = lparams,
                                    input),
                      sep = "\n")
  )
}

dInverseChiSquared_matlab <- paste(
  "function f = inversechisquaredpdf(x, nu)",
  "    f = 2^(-nu/2) / gamma(nu / 2) * x^(-nu / 2 - 1) * exp(-1 / (2 * x));",
  "end",
  "% calling function",
  sep = "\n"
)

rInverseChiSquared_matlab <- paste(
  "function x = inversechisquaredrnd(nu, M)",
  "    y = chi2rnd(nu, M);",
  "    x = 1 ./ y;",
  "end",
  "% calling function",
  sep = "\n"
)

fInverseChiSquared_matlab <- function(input){
  lparams <- input$inversechisquared_df
  switch(input$property,
         pdf=paste(dInverseChiSquared_matlab,
                   fMatlabHelper("inversechisquared",
                                 params = lparams,
                                 input),
                   sep = "\n"),
         log_pdf=paste(dInverseChiSquared_matlab,
                       fMatlabHelper("inversechisquared",
                                     params = lparams,
                                     input),
                       sep = "\n"),
         random=paste(rInverseChiSquared_matlab,
                      fMatlabHelper("inversechisquared",
                                    params = lparams,
                                    input),
                      sep = "\n")
  )
}

dBetaBinomial_matlab <- paste(
  "function f = betabinomialpdf(x, n, alpha, beta1)",
  "    if abs(x - round(x)) > 0",
  "        f = 0;",
  "    elseif x < 0 || x > n",
  "        f = 0;",
  "    else",
  "        f = nchoosek(n, x) * beta(x + alpha, n - x + beta1) / beta(alpha, beta1);",
  "    end",
  "end",
  "% calling function",
  sep = "\n"
)

rBetaBinomial_matlab <- paste(
  "function x = betabinomialrnd(n, alpha, beta1, M)",
  "    theta = betarnd(alpha, beta1, M);",
  "    x = binornd(n, theta);",
  "end",
  "% calling function",
  sep = "\n"
)


fBetaBinomial_matlab <- function(input){
  lparams <- c(input$betabinomial_size, input$betabinomial_shape1, input$betabinomial_shape2)
  switch(input$property,
         pdf=paste(dBetaBinomial_matlab,
                   fMatlabHelper("betabinomial",
                                 params = lparams,
                                 input),
                   sep = "\n"),
         log_pdf=paste(dBetaBinomial_matlab,
                       fMatlabHelper("betabinomial",
                                     params = lparams,
                                     input),
                       sep = "\n"),
         random=paste(rBetaBinomial_matlab,
                      fMatlabHelper("betabinomial",
                                    params = lparams,
                                    input),
                      sep = "\n")
  )
}

fMultivariatenormal_matlab <- function(input){
  mux <- input$multivariatenormal_mux
  muy <- input$multivariatenormal_muy
  sigmax <- input$multivariatenormal_sigmax
  sigmay <- input$multivariatenormal_sigmay
  rho <- input$multivariatenormal_rho
  
  switch(input$property,
         pdf=paste0("mvnpdf(x, [", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]])"),
         log_pdf=paste0("log(mvnpdf(x, [", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]]))"),
         random=paste0("mvnrnd([", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]], n)")
  )
}



dMultivariatet_matlab <- paste(
  "function f = multivariatetpdf(x, mu, Sigma, nu)",
  "    d = length(mu);",
  "    x_minus_mu = reshape(x - mu, d, 1);",
  "    f = gamma((nu + d) / 2) / (gamma(nu / 2) * nu^(d / 2) * pi^(d / 2) * det(Sigma)^0.5) * (1 + (1 / nu) * x_minus_mu' * inv(Sigma) * x_minus_mu)^(-(nu + d) / 2);",
  "end",
  "% calling function",
  sep = "\n"
)

rMultivariatet_matlab <- paste(
  "function x = multivariatetrnd(mu, Sigma, nu, n)",
  "    d = length(mu);",
  "    y = mvnrnd(zeros([d, 1]), Sigma, n);",
  "    u = chi2rnd(nu, n);",
  "    x = zeros([n, d]);",
  "    for i = 1:n",
  "        x(i, :) = mu + y(i, :) / sqrt(u(i) / nu);",
  "    end",
  "end",
  "% calling function",
  sep = "\n"
)

fMultivariatet_matlab <- function(input){
  
  mux <- input$multivariatet_mux
  muy <- input$multivariatet_muy
  sigmax <- input$multivariatet_sigmax
  sigmay <- input$multivariatet_sigmay
  rho <- input$multivariatet_rho
  df <- input$multivariatet_df
  
  switch(input$property,
         pdf=paste(dMultivariatet_matlab,
                   paste0("multivariatetpdf(x, [", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]], ", df, ")"),
                   sep = "\n"),
         log_pdf=paste(dMultivariatet_matlab,
                       paste0("log(multivariatetpdf(x, [", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]], ", df, "))"),
                       sep = "\n"),
         random=paste(rMultivariatet_matlab,
           paste0("multivariatetrnd([", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]], ", df, ", n)"),
           sep = "\n")
         )
}

fMatlabcode <- function(input){
  text <- 
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fMatlabHelper("norm",
                                  params=c(input$normal_mu,input$normal_sigma),
                                  input),
             Uniform=fMatlabHelper("unif",
                                   params = c(input$uniform_a, input$uniform_b),
                                   input),
             LogNormal=fMatlabHelper("logn",
                                     params = c(input$lognormal_mu, input$lognormal_sigma),
                                     input),
             Exponential=fMatlabHelper("exp",
                                       params = input$exponential_rate, 
                                       input),
             Gamma=fMatlabHelper("gam",
                                  params = c(input$gamma_shape, 1 / input$gamma_rate),
                                  input),
             t=fStudentt_matlab(input),
             Cauchy=fCauchy_matlab(input),
             HalfCauchy=fHalfCauchy_matlab(input),
             Beta=fMatlabHelper("beta",
                                params = c(input$beta_a, input$beta_b),
                                input),
             InverseGamma=fInverseGamma_matlab(input),
             InverseChiSquared=fInverseChiSquared_matlab(input)
      )
    }else if(input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=fMatlabHelper("bino",
                                     params = c(1, input$bernoulli_prob),
                                     input),
             Binomial=fMatlabHelper("bino",
                                    params = c(input$binomial_size, input$binomial_prob),
                                    input),
             Poisson=fMatlabHelper("poiss",
                                   params = input$poisson_lambda,
                                   input),
             NegativeBinomial=fMatlabHelper("nbin",
                                            params = c(input$negativebinomial_dispersion,
                                                       input$negativebinomial_dispersion / (input$negativebinomial_dispersion + input$negativebinomial_mean)),
                                            input),
             BetaBinomial=fBetaBinomial_matlab(input)
      )
    }else if(input$distType=='Multivariate'){
      switch(input$dist2,
             MultivariateNormal=fMultivariatenormal_matlab(input),
             MultivariateT=fMultivariatet_matlab(input)
      )
    }
  return(prismCodeBlock(text, language = "matlab"))
}
