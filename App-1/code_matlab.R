
fMatlabcode <- function(input){
  text <-
    if(input$dist=="Normal"){
      if(input$property=="pdf")
          fMakeFunctionPaste(mainName="normpdf",
                      params=c(input$mu,input$sigma),
                      prefixparams="x")
      else if(input$property=="log_pdf")
          fMakeFunctionPaste(mainName=paste0("-0.5 * log(2 * pi) - log(", eval(parse(text=input$sigma)),
                                      ") - (x - ",
                                      eval(parse(text=input$mu)), ")^2 / (2 * ",
                                      eval(parse(text=input$sigma)), "^2)"),
                      params=c(input$mu,input$sigma),
                      freeform=TRUE)
      else if(input$property=="random")
        fMakeFunctionPaste(mainName="normrnd",
                      params=c(input$mu,input$sigma),
                      postfixparams="[n, 1]")
    }
  return(prismCodeBlock(text, language = "matlab"))
}