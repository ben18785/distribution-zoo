
fRcode <- function(input){
  text <-
    if(input$dist=="Normal"){
      if(input$property=="pdf")
        fMakeFunctionPaste(mainName="dnorm",
                                          params=c(input$mu,input$sigma),
                                          prefixparams="x")
      else if(input$property=="log_pdf")
        fMakeFunctionPaste(mainName="dnorm",
                      params=c(input$mu,input$sigma),
                      prefixparams="x",
                      postfixparams="log=TRUE")
      else if(input$property=="random")
        fMakeFunctionPaste(mainName="rnorm",
                      params=c(input$mu,input$sigma),
                      prefixparams="n")
    }else if(input$dist=="Uniform"){
        if(input$property=="pdf")
          fMakeFunctionPaste(mainName="dunif",
                             params=c(input$a,input$b),
                             prefixparams="x")
        else if(input$property=="log_pdf")
          fMakeFunctionPaste(mainName="dunif",
                           params=c(input$a,input$b),
                           prefixparams="x",
                           postfixparams="log=TRUE")
      else if(input$property=="random")
        fMakeFunctionPaste(mainName="runif",
                           params=c(input$a,input$b),
                           prefixparams="n")
    }
  return(prismCodeBlock(text, language = "r"))
}