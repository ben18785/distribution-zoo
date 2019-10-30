fJuliaHelper <- function(name, input, params){
    if(input$property=="pdf")
      fMakeFunctionPaste(mainName=paste0("pdf(", input$dist),
                         params=paste(params, sep = ", "),
                         postfixparams="x",
                         julia=TRUE)
    else if(input$property=="log_pdf")
      fMakeFunctionPaste(mainName=paste0("logpdf(", input$dist),
                         params=paste(params, sep = ", "),
                         postfixparams="x",
                         julia=TRUE)
    else if(input$property=="random")
      fMakeFunctionPaste(mainName=paste0("rand(", input$dist),
                         params=paste(params, sep = ", "),
                         postfixparams="n",
                         julia=TRUE)
}

fJuliaHelperDiscrete <- function(name, input, params){
  if(input$property=="pdf")
    fMakeFunctionPaste(mainName=paste0("pdf(", input$dist1),
                       params=paste(params, sep = ", "),
                       postfixparams="x",
                       julia=TRUE)
  else if(input$property=="log_pdf")
    fMakeFunctionPaste(mainName=paste0("logpdf(", input$dist1),
                       params=paste(params, sep = ", "),
                       postfixparams="x",
                       julia=TRUE)
  else if(input$property=="random")
    fMakeFunctionPaste(mainName=paste0("rand(", input$dist1),
                       params=paste(params, sep = ", "),
                       postfixparams="n",
                       julia=TRUE)
}

fTCustomJulia <- function(input){
  if(input$property=="pdf")
    paste0("pdf(TDist(", input$t_nu, "), ", "(x - ", input$t_mu, ") / ", input$t_sigma, ") / ", input$t_sigma)
  else if(input$property=="log_pdf")
    paste0("logpdf(TDist(", input$t_nu, "), ", "(x - ", input$t_mu, ") / ", input$t_sigma, ") - log(", input$t_sigma, ")")
  else if(input$property=="random")
    paste0(input$t_mu, " .+ ", input$t_sigma, " .* ", "rand(TDist(", input$t_nu, ")", ", n)")
}

fTruncatedCauchyJulia <- function(input){
  preamble <- paste0("d=Truncated(Cauchy(", input$halfcauchy_location, ", ", input$halfcauchy_scale, ")", ", 0, Inf)")
  switch(input$property,
         pdf=paste(preamble, "pdf(d, x)", sep = "\n"),
         log_pdf=paste(preamble, "logpdf(d, x)", sep = "\n"),
         random=paste(preamble, "rand(d, n)", sep = "\n"))
}

fInverseChiSquaredJulia <- function(input){
  switch(input$property,
         pdf=paste0("pdf(InverseGamma(", input$inversechisquared_df, " / 2, 1 / 2), x)"),
         log_pdf=paste0("logpdf(InverseGamma(", input$inversechisquared_df, " / 2, 1 / 2), x)"),
         random=paste0("rand(InverseGamma(", input$inversechisquared_df, " / 2, 1 / 2), n)"))
}


fJuliacode <- function(input){
  text <- 
    if(input$distType=='Continuous'){
      switch(input$dist,
              Normal=fJuliaHelper("Normal", input, c(input$normal_mu, input$normal_sigma)),
              Uniform=fJuliaHelper("Uniform", input, c(input$uniform_a, input$uniform_b)),
              LogNormal=fJuliaHelper("LogNormal", input, c(input$lognormal_mu, input$lognormal_sigma)),
              Exponential=fJuliaHelper("Exponential", input, c(1 / input$exponential_rate)),
              Gamma=fJuliaHelper("Gamma", input, c(input$gamma_shape, 1 / input$gamma_rate)),
              t=fTCustomJulia(input),
              Beta=fJuliaHelper("Beta", input, c(input$beta_a, input$beta_b)),
              Cauchy=fJuliaHelper("Cauchy", input, c(input$cauchy_location,input$cauchy_scale)),
              HalfCauchy=fTruncatedCauchyJulia(input),
              InverseGamma=fJuliaHelper("InverseGamma", input, c(input$inversegamma_shape, input$inversegamma_scale)),
              InverseChiSquared=fInverseChiSquaredJulia(input),
              LogitNormal=fJuliaHelper("LogitNormal", input, c(input$logitnormal_mu, input$logitnormal_sigma)),
              "Coming soon.")
    }else if(input$distType=='Discrete'){
       switch(input$dist1,
              Bernoulli=fJuliaHelperDiscrete("Bernoulli", input, c(input$bernoulli_prob)),
              Binomial=fJuliaHelperDiscrete("Binomial", input, c(input$binomial_size, input$binomial_prob)),
              "Coming soon.")
    }
  
  if(text!="Coming soon."){
  tagList(prismCodeBlock(paste0("using Random, Distributions\n",
                                text), language = "julia"),
          h3("Note that code assumes that 'Compat' and 'Distributions' packages are installed by typing:"),
          prismCodeBlock(paste("Pkg.add(\"Compat\")",
                               "Pkg.add(\"Distributions\")",
                               sep = "\n"), language = "julia"),
          h3("at Julia command line."))
  }else{
      tagList(h3(text))
    }
}
