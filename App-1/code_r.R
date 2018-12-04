
fRHelper <- function(mainName, params, input){
  if(input$property=="pdf")
    fMakeFunctionPaste(mainName=paste0("d", mainName),
                     params=params,
                     prefixparams="x")
  else if(input$property=="log_pdf")
    fMakeFunctionPaste(mainName=paste0("d", mainName),
                       params=params,
                       prefixparams="x",
                       postfixparams="log=TRUE")
  else if(input$property=="random")
    fMakeFunctionPaste(mainName=paste0("r", mainName),
                       params=params,
                       prefixparams="n")
}


fRcode <- function(input){
  text <-
    if(input$dist=="Normal"){
      fRHelper("norm", c(input$mu, input$sigma), input)
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
    }else if(input$dist=="LogNormal"){
      if(input$property=="pdf")
        fMakeFunctionPaste(mainName="dlnnorm",
                           params=c(input$meanlog,input$sdlog),
                           prefixparams="x")
      else if(input$property=="log_pdf")
        fMakeFunctionPaste(mainName="dlnorm",
                           params=c(input$meanlog,input$sdlog),
                           prefixparams="x",
                           postfixparams="log=TRUE")
      else if(input$property=="random")
        fMakeFunctionPaste(mainName="rlnorm",
                           params=c(input$meanlog,input$sdlog),
                           prefixparams="n")
    }
  return(prismCodeBlock(text, language = "r"))
}