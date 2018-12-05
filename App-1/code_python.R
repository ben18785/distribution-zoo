
fPythoncode <- function(input){
  text <-
    if(input$dist=="Normal"){
      if(input$property=="pdf")
        fMakeFunctionPaste(mainName="scipy.stats.norm.pdf",
                      params=c(input$normal_mu,input$normal_sigma),
                      prefixparams="x")
      else if(input$property=="log_pdf")
        fMakeFunctionPaste(mainName="scipy.stats.norm.logpdf",
                      params=c(input$normal_mu,input$normal_sigma),
                      prefixparams="x")
      else if(input$property=="random")
        fMakeFunctionPaste(mainName="numpy.random.normal",
                      import="import numpy",
                      params=c(input$normal_mu,input$normal_sigma),
                      postfixparams="n")
    }else if(input$dist=="Uniform"){
      if(input$property=="pdf")
        HTML(markdown::markdownToHTML(text="```{python}
                                      scipy.stats.norm.logpdf(1,2,3)", options=c("highlight_code")))
    }
  return(prismCodeBlock(text, language = "python"))
}
