fMathematicaHelper <- function(mainName, params, input, import=NULL, named_arguments=NULL, vector_params=FALSE,
                               mathematica_vector=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0("PDF[", mainName, "Distribution"),
                                params=params, postfixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params,
                                mathematica = T,
                                mathematica_vector=mathematica_vector),
         log_pdf=fMakeFunctionPaste(mainName=paste0("Log@PDF[", mainName, "Distribution"),
                                    params=params, postfixparams="x",
                                    import=import, named_arguments=named_arguments,
                                    vector_params=vector_params,
                                    mathematica = T,
                                    mathematica_vector=mathematica_vector),
         random=fMakeFunctionPaste(mainName=paste0("RandomVariate[", mainName, "Distribution"),
                                   params=params, postfixparams="n",
                                   import=import, named_arguments=named_arguments,
                                   vector_params=vector_params,
                                   mathematica = T,
                                   mathematica_vector=mathematica_vector))
}
fMathematicacode <- function(input){
  text <- 
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fMathematicaHelper("Normal",
                                       params = c(input$normal_mu, input$normal_sigma),
                                       input),
             Uniform=fMathematicaHelper("Uniform",
                                        params = c(input$uniform_a, input$uniform_b),
                                        input,
                                        vector_params = T,
                                        mathematica_vector = T)
      )
    }
  return(prismCodeBlock(text))
}
