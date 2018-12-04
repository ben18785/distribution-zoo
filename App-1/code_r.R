
fRHelper <- function(mainName, params, input, import=NULL){
  switch(input$property,
        pdf=fMakeFunctionPaste(mainName=paste0("d", mainName),
                               params=params, prefixparams="x",
                               import=import),
        log_pdf=fMakeFunctionPaste(mainName=paste0("d", mainName),
                                   params=params, prefixparams="x",
                                   postfixparams="log=TRUE",
                                   import=import),
        random=fMakeFunctionPaste(mainName=paste0("r", mainName),
                                  params=params, prefixparams="n",
                                  import=import))
}


fRcode <- function(input){
  text <-
    switch(input$dist,
           Normal=fRHelper("norm", c(input$mu, input$sigma), input),
           Uniform=fRHelper("unif", c(input$a, input$b), input),
           LogNormal=fRHelper("lnorm", c(input$meanlog,input$sdlog), input),
           Exponential=fRHelper("exp", input$rate, input),
           Gamma=fRHelper("gamma", c(input$shape, input$rateGam), input),
           t=fRHelper("st", c(input$muT,input$sigmaT, input$nuT), input, import="library(LaplacesDemon)"),
           Beta=fRHelper("beta", c(input$alpha,input$beta), input),
           Cauchy=fRHelper("cauchy", c(input$locationC,input$scaleC), input))
           
  
  return(prismCodeBlock(text, language = "r"))
}