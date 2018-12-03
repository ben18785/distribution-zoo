

fMathematicacode <- function(input){
  if(input$dist=="Normal"){
    if(input$property=="pdf")
      text <- 
        fMakeFunctionPaste(mainName="PDF[NormalDistribution",
                    params=c(input$mu,input$sigma),
                    postfixparams="x",
                    mathematica=TRUE)
    else if(input$property=="log_pdf")
      text <-
        fMakeFunctionPaste(mainName=paste0("-0.5 Log[2 Pi] - Log[", eval(parse(text=input$sigma)),
                                    "] - (x - ",
                                    eval(parse(text=input$mu)), ")^2 / (2 ",
                                    eval(parse(text=input$sigma)), "^2)"),
                    freeform=TRUE)
    else if(input$property=="random")
      text <- 
        fMakeFunctionPaste(mainName="RandomVariate[NormalDistribution",
                    params=c(input$mu,input$sigma),
                    postfixparams="n",
                    mathematica=TRUE)
  }
  return(prismCodeBlock(text))
}