fJuliaHelperTwoParam <- function(name, input, param1, param2){
    if(input$property=="pdf")
      fMakeFunctionPaste(mainName=paste0("pdf(", input$dist),
                         params=c(param1, param2),
                         postfixparams="x",
                         julia=TRUE)
    else if(input$property=="log_pdf")
      fMakeFunctionPaste(mainName=paste0("logpdf(", input$dist),
                         params=c(param1, param2),
                         postfixparams="x",
                         julia=TRUE)
    else if(input$property=="random")
      fMakeFunctionPaste(mainName=paste0("rand(", input$dist),
                         params=c(param1, param2),
                         postfixparams="n",
                         julia=TRUE)
}


fJuliacode <- function(input){
  text <- 
    if(input$dist=="Normal"){
      fJuliaHelperTwoParam("Normal", input, input$normal_mu, input$normal_sigma)
    }else{
      "To be completed."
    }
  
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
