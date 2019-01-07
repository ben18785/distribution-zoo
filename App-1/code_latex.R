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
                          \\left(\\frac{x-\\mu}{\\sigma\\sqrt{2}}\\right)\\right]",
                                 "\\text{where }\\text{erf}(x) = \\frac{2}{\\sqrt{\\pi}}\\int_{0}^{x} e^{-t^2}\\mathrm{d}t \\text{ is the error function}}")),
           Uniform=fLatexHelper(c("\\mathrm{E}(X) = \\frac{1}{2}(a + b)",
                                  "var(X) = \\frac{1}{12}(b - a)^2"), 
                                  c("f(x|a,b)=\\begin{cases}
  0,  & \\text{if }x \\not\\in [a,b] \\\\
  \\frac{1}{b-a}, & \\text{if } x \\in [a,b]
  \\end{cases}}"), c("F(x|a,b)=\\begin{cases}
  0,  & \\text{if }x < a \\\\
  \\frac{x-a}{b-a}, & \\text{if } x\\in [a,b]\\\\
  1, & \\text{if } x > b
  \\end{cases}")),
           LogNormal=fLatexHelper(c("\\mathrm{E}(X) = \\text{exp}(\\mu + \\frac{\\sigma^2}{2})",
                                    "var(X) = \\left[\\text{exp}(\\sigma^2) - 1\\right] \\text{exp}(2\\mu + \\sigma^2)"),
                                  c("f(x|\\mu,\\sigma) = \\frac{1}{x \\sigma \\sqrt{2 \\pi}} \\text{exp}\\left(-\\frac{(\\text{log } x - \\mu)^2}{2\\sigma^2}\\right)"),
                                  c("F(x|\\mu,\\sigma) = \\frac{1}{2} + \\frac{1}{2} \\text{erf}\\left(\\frac{\\text{log } x - \\mu}{\\sqrt{2} \\sigma}\\right)",
                                    "\\text{where }\\text{erf}(x) = \\frac{2}{\\sqrt{\\pi}}\\int_{0}^{x} e^{-t^2}\\mathrm{d}t \\text{ is the error function}}")),
           Exponential=fLatexHelper(c("\\mathrm{E}(X) = \\frac{1}{\\lambda}",
                                      "var(X) = \\frac{1}{\\lambda^2}"),
                                    c("f(x|\\lambda) = \\lambda e^{-\\lambda x}"),
                                    c("F(x|\\lambda) = 1 - e^{-\\lambda x}")),
           Gamma=fLatexHelper(c("\\mathrm{E}(X) = \\frac{\\alpha}{\\beta}",
                                "var(X) = \\frac{\\alpha}{\\beta^2}"),
                              c("f(x|\\alpha, \\beta) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x}",
                                "\\text{where }\\Gamma(w)=\\int_{0}^{\\infty}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the gamma function}}"),
                              c("F(x|\\alpha, \\beta) = \\frac{1}{\\Gamma(\\alpha)} \\gamma(\\alpha, \\beta x)",
                                "\\text{where }\\gamma(w,v)=\\int_{0}^{v}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the incomplete lower gamma function}}"))
    )
  }
}