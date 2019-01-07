fMathematicaHelper <- function(mainName, params, input, import=NULL, named_arguments=NULL, vector_params=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0("PDF[", mainName, "Distribution"),
                                params=params, postfixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params,
                                mathematica = T),
         log_pdf=fMakeFunctionPaste(mainName=paste0("Log@PDF[", mainName, "Distribution"),
                                    params=params, postfixparams="x",
                                    import=import, named_arguments=named_arguments,
                                    vector_params=vector_params,
                                    mathematica = T),
         random=fMakeFunctionPaste(mainName=paste0("RandomVariate[", mainName, "Distribution"),
                                   params=params, postfixparams="n",
                                   import=import, named_arguments=named_arguments,
                                   vector_params=vector_params,
                                   mathematica = T))
}
fMathematicacode <- function(input){
  text <- 
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fMathematicaHelper("Normal",
                                       params = c(input$normal_mu, input$normal_sigma),
                                       input)
      )
    }
  return(prismCodeBlock(text))
}
