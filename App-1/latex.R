fLatex <- function(input){
  if (input$dist=='Normal'){
    tagList(h2("Moments"),
            h2("\\mathrm{E}(X) = \\mu"),
            h2("var(X) = \\sigma^2"),
            h2("PDF"),
            h2("f(x|\\mu,\\sigma) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}}\\text{exp}\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right)"),
            h2("CDF"),
            h2("F(x|\\mu,\\sigma) = \\frac{1}{2}\\left[1+\\text{erf}\\left(\\frac{x-\\mu}{\\sigma\\sqrt{2}}\\right)\\right]")
    )
  }
}