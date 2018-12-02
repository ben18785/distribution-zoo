fJuliacode <- function(input){
  if(input$dist=="Normal"){
    if(input$property=="pdf")
      short <- fMakeFunction(mainName="pdf",
                             params=c(input$mu,input$sigma),
                             postfixparams="x",
                             julia=TRUE)
    else if(input$property=="log_pdf")
      short <- fMakeFunction(mainName="logpdf",
                             params=c(input$mu,input$sigma),
                             postfixparams="x",
                             julia=TRUE)
    else if(input$property=="random")
      short <- fMakeFunction(mainName="rand(Normal",
                             params=c(input$mu,input$sigma),
                             postfixparams="n",
                             julia=TRUE)
  }
  
  tagList(short,
          h3("Note that code assumes that 'Compat' and 'Distributions' packages are installed by typing:"),
          h3("Pkg.add(\"Compat\")", style="color:#d95f02"),
          h3("Pkg.add(\"Distributions\")", style="color:#d95f02"),
          h3("at Julia command line."))
}