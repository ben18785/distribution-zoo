fPythonHelper <- function(mainName, mainName1, params, input, import=NULL, import1=NULL, named_arguments=NULL, vector_params=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName, ".pdf"),
                                params=params, prefixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params),
         log_pdf=fMakeFunctionPaste(mainName=paste0("scipy.stats.", mainName, ".logpdf"),
                                    params=params, prefixparams="x",
                                    import=import, named_arguments=named_arguments,
                                    vector_params=vector_params),
         random=fMakeFunctionPaste(mainName=paste0("numpy.random.", mainName1),
                                   params=params, postfixparams="n",
                                   import=import1, named_arguments=named_arguments,
                                   vector_params=vector_params))
}



fPythoncode <- function(input){
  text <-
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fPythonHelper("norm", "normal",
                                  c(input$normal_mu, input$normal_sigma),
                                  input, import="import scipy.stats",
                                  import1="import numpy"),
             Uniform=fPythonHelper("uniform", "uniform",
                                  c(input$uniform_a, input$uniform_b),
                                  input, import="import scipy.stats",
                                  import1="import numpy")
      )
    }
  return(prismCodeBlock(text, language = "python"))
}
