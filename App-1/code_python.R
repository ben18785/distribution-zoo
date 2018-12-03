fPrismPython <- function(text){
  return(prismCodeBlock(text, language = "python"))
}

fPythoncode <- function(input){
  if(input$dist=="Normal"){
    if(input$property=="pdf")
      fPrismPython(fMakeFunctionPaste(mainName="scipy.stats.norm.pdf",
                    params=c(input$mu,input$sigma),
                    prefixparams="x"))
    else if(input$property=="log_pdf")
      fPrismPython(fMakeFunctionPaste(mainName="scipy.stats.norm.logpdf",
                    params=c(input$mu,input$sigma),
                    prefixparams="x"))
    else if(input$property=="random")
      fPrismPython(fMakeFunctionPaste(mainName="numpy.random.normal",
                    import="import numpy",
                    params=c(input$mu,input$sigma),
                    postfixparams="n"))
  }else if(input$dist=="Uniform"){
    if(input$property=="pdf")
      HTML(markdown::markdownToHTML(text="```{python}
                                    scipy.stats.norm.logpdf(1,2,3)", options=c("highlight_code")))
  }
}