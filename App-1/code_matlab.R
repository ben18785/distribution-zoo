
fMatlabcode <- function(input){
  text <-
    if(input$dist=="Normal"){
      if(input$property=="pdf")
          fMakeFunctionPaste(mainName="normpdf",
                      params=c(input$normal_mu,input$normal_sigma),
                      prefixparams="x")
      else if(input$property=="log_pdf")
          fMakeFunctionPaste(mainName=paste0("-0.5 * log(2 * pi) - log(", eval(parse(text=input$normal_sigma)),
                                      ") - (x - ",
                                      eval(parse(text=input$normal_mu)), ")^2 / (2 * ",
                                      eval(parse(text=input$normal_sigma)), "^2)"),
                      params=c(input$normal_mu,input$normal_sigma),
                      freeform=TRUE)
      else if(input$property=="random")
        fMakeFunctionPaste(mainName="normrnd",
                      params=c(input$normal_mu,input$normal_sigma),
                      postfixparams="[n, 1]")
    }
  return(prismCodeBlock(text, language = "matlab"))
}
