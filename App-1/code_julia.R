fJuliacode <- function(input){
  text <- 
    if(input$dist=="Normal"){
      if(input$property=="pdf")
        fMakeFunctionPaste(mainName="pdf",
                               params=c(input$normal_mu,input$normal_sigma),
                               postfixparams="x",
                               julia=TRUE)
      else if(input$property=="log_pdf")
        fMakeFunctionPaste(mainName="logpdf",
                               params=c(input$normal_mu,input$normal_sigma),
                               postfixparams="x",
                               julia=TRUE)
      else if(input$property=="random")
        fMakeFunctionPaste(mainName="rand(Normal",
                               params=c(input$normal_mu,input$normal_sigma),
                               postfixparams="n",
                               julia=TRUE)
    }
  
  tagList(prismCodeBlock(text, language = "julia"),
          h3("Note that code assumes that 'Compat' and 'Distributions' packages are installed by typing:"),
          prismCodeBlock(paste("Pkg.add(\"Compat\")",
                               "Pkg.add(\"Distributions\")",
                               sep = "\n"), language = "julia"),
          h3("at Julia command line."))
}
