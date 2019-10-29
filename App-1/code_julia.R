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


fJuliacode <- function(input){
  text <- switch (input$dist,
    Normal=fJuliaHelper("Normal", input, c(input$normal_mu, input$normal_sigma)),
    Uniform=fJuliaHelper("Uniform", input, c(input$uniform_a, input$uniform_b)),
    LogNormal=fJuliaHelper("LogNormal", input, c(input$lognormal_mu, input$lognormal_sigma)),
    Exponential=fJuliaHelper("Exponential", input, c(1 / input$exponential_rate)),
    Gamma=fJuliaHelper("Gamma", input, c(input$gamma_shape, 1 / input$gamma_rate)),
    "To be completed."
  )
      
  
  if(text!="To be completed."){
  tagList(prismCodeBlock("using Random, Distributions", language = "julia"),
          prismCodeBlock(text, language = "julia"),
          h3("Note that code assumes that 'Compat' and 'Distributions' packages are installed by typing:"),
          prismCodeBlock(paste("Pkg.add(\"Compat\")",
                               "Pkg.add(\"Distributions\")",
                               sep = "\n"), language = "julia"),
          h3("at Julia command line."))
  }else{
      tagList(h3(text))
    }
}
