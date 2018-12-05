

fMathematicacode <- function(input){
  text <- 
    if(input$dist=="Normal"){
      if(input$property=="pdf")
          fMakeFunctionPaste(mainName="PDF[NormalDistribution",
                      params=c(input$normal_mu,input$normal_sigma),
                      postfixparams="x",
                      mathematica=TRUE)
      else if(input$property=="log_pdf")
          fMakeFunctionPaste(mainName=paste0("-0.5 Log[2 Pi] - Log[", eval(parse(text=input$normal_sigma)),
                                      "] - (x - ",
                                      eval(parse(text=input$normal_mu)), ")^2 / (2 ",
                                      eval(parse(text=input$normal_sigma)), "^2)"),
                      freeform=TRUE)
      else if(input$property=="random")
          fMakeFunctionPaste(mainName="RandomVariate[NormalDistribution",
                      params=c(input$normal_mu,input$normal_sigma),
                      postfixparams="n",
                      mathematica=TRUE)
    }
  return(prismCodeBlock(text))
}
