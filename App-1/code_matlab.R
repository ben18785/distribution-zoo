fMatlabHelper <- function(mainName, params, input, import=NULL, named_arguments=NULL, vector_params=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0(mainName, "pdf"),
                                params=params, prefixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params),
         log_pdf=paste0(fMakeFunctionPaste(mainName=paste0("log(", mainName, "pdf"),
                                    params=params, prefixparams="x",
                                    import=import, named_arguments=named_arguments,
                                    vector_params=vector_params),
                        ")"),
         random=fMakeFunctionPaste(mainName=paste0(mainName, "rnd"),
                                   params=params, postfixparams="[n, 1]",
                                   import=import, named_arguments=named_arguments,
                                   vector_params=vector_params))
}


fMatlabcode <- function(input){
  text <- 
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fMatlabHelper("norm",
                                  params=c(input$normal_mu,input$normal_sigma),
                                  input),
             Uniform=fMatlabHelper("unif",
                                   params = c(input$uniform_a, input$uniform_b),
                                   input)
      )
            
    }
  return(prismCodeBlock(text, language = "matlab"))
}
