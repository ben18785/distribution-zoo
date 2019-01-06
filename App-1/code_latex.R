fPrismLatex <- function(text){
  return(prismCodeBlock(text, language = "latex"))
}

fLatexHelper <- function(lMoments, lPDF, lCDF){
  moments_latex <- lapply(lMoments, fPrismLatex)
  pdf_latex <- lapply(lPDF, fPrismLatex)
  cdf_latex <- lapply(lCDF, fPrismLatex)
  return(tagList(h2("Moments"),
                 moments_latex,
                 h2("Probability density function (PDF)"),
                 pdf_latex,
                 h2("Cumulative distribution function (CDF)"),
                 cdf_latex))
}


fLatex <- function(input){
  if (input$distType=='Continuous'){
    switch(input$dist,
           Normal=fLatexHelper(c("\\mathrm{E}(X) = \\mu",
                                 "var(X) = \\sigma^2"),
                               c("f(x|\\mu,\\sigma) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}}
                                 \\text{exp}\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right)"),
                               c("F(x|\\mu,\\sigma) = \\frac{1}{2}\\left[1+\\text{erf}
                          \\left(\\frac{x-\\mu}{\\sigma\\sqrt{2}}\\right)\\right]")),
           Uniform=fLatexHelper(c("\\mathrm{E}(X) = \\frac{1}{2}(a + b)",
                                  "var(X) = \\frac{1}{12}(b - a)^2"), 
                                  c("f(x|a,b)=\\begin{cases}
  0,  & \\text{if }x \\not\\in [a,b] \\\\
  \\frac{1}{b-a}, & \\text{if } x \\in [a,b]
  \\end{cases}}"), c("F(x|a,b)=\\begin{cases}
  0,  & \\text{if }x < a \\\\
  \\frac{x-a}{b-a}, & \\text{if } x\\in [a,b]\\\\
  1, & \\text{if } x > b
  \\end{cases}"))
    )
  }
}