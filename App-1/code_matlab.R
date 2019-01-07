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
             Beta=fMatlabHelper("beta",
                                params = c(input$beta_a, input$beta_b),
                                input)
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
        
      )
    }else if(input$distType=='Multivariate'){
      switch(input$dist2,
             MultivariateNormal=
      )
    }
  return(prismCodeBlock(text, language = "matlab"))
}
