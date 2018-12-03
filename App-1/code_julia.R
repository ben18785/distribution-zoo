fJuliacode <- function(input){
  if(input$dist=="Normal"){
    if(input$property=="pdf")
      text <- fMakeFunctionPaste(mainName="pdf",
                             params=c(input$mu,input$sigma),
                             postfixparams="x",
                             julia=TRUE)
    else if(input$property=="log_pdf")
      text <- fMakeFunctionPaste(mainName="logpdf",
                             params=c(input$mu,input$sigma),
                             postfixparams="x",
                             julia=TRUE)
    else if(input$property=="random")
      text <- fMakeFunctionPaste(mainName="rand(Normal",
                             params=c(input$mu,input$sigma),
                             postfixparams="n",
                             julia=TRUE)
  }
  
  tagList(prismCodeBlock(text, language = "julia"),
          h3("Note that code assumes that 'Compat' and 'Distributions' packages are installed by typing:"),
          h3("Pkg.add(\"Compat\")", style="color:#d95f02"),
          h3("Pkg.add(\"Distributions\")", style="color:#d95f02"),
          h3("at Julia command line."))
}