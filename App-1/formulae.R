
fFormulae <- function(input){
  if (input$distType=='Continuous'){
    if (input$dist=='Normal'){
      withMathJax(h2("Parameters"),
                  h2(withMathJax(
                    helpText(HTML('$$\\color{black}{\\text{mean: }\\mu\\in\\mathbb{R}}$$')))),
                  h2(withMathJax(
                    helpText(HTML('$$\\color{black}{\\text{standard deviation: }\\sigma\\in\\mathbb{R}^+}$$')))),
                  h2("Support"),
                  h2("$$x\\in\\mathbb{R}$$"),
                  h2("Moments"),
                  h2("$$\\mathrm{E}(X) = \\mu$$"),
                  h2("$$var(X) = \\sigma^2$$"),
                  h2("PDF"),
                  h2("$$f(x|\\mu,\\sigma) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}} 
                     \\text{exp}\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right)$$"),
                  h2("CDF"),
                  h2("$$F(x|\\mu,\\sigma) = \\frac{1}{2}\\left[1+\\text{erf}\\left(\\frac{x-\\mu}{\\sigma\\sqrt{2}}\\right)\\right]$$"),
                  h2(withMathJax(
                    helpText(HTML('$$\\color{black}{\\text{where }\\text{erf}(x) = \\frac{2}{\\sqrt{\\pi}}\\int_{0}^{x} e^{-t^2}\\mathrm{d}t \\text{ is the error function}}$$')))))
    }else if(input$dist=='Uniform'){
      withMathJax(h2("Parameters"),
                  h2("$$-\\infty<a<b<+\\infty$$"),
                  h2("Support"),
                  h2("$$x\\in[a,b]$$"),
                  h2("Moments"),
                  h2("$$\\mathrm{E}(X) = \\frac{1}{2}(a + b)$$"),
                  h2("$$var(X) = \\frac{1}{12}(b - a)$$"),
                  h2("PDF"),
                  h2(withMathJax(
                    helpText(HTML('$$\\color{black}{f(x|a,b)=\\begin{cases}
                                  0,  & \\text{if }x \\not\\in [a,b] \\\\
                                  \\frac{1}{b-a}, & \\text{if } x \\in [a,b]
                                  \\end{cases}\\!}$$')))),
                  h2("CDF"),
                  h2(withMathJax(
                    helpText(HTML('$$\\color{black}{F(x|a,b)=\\begin{cases}
                                  0,  & \\text{if }x < a \\\\
                                  \\frac{x-a}{b-a}, & \\text{if } x\\in [a,b]\\\\
                                  1, & \\text{if } x > b
                                  \\end{cases}\\!}$$')))
                    ))
      }else if(input$dist=='LogNormal'){
        withMathJax(h2("Parameters"),
                    h2(withMathJax(
                      helpText(HTML('$$\\color{black}{\\text{log mean: }\\mu\\in\\mathbb{R}}$$')))),
                    h2(withMathJax(
                      helpText(HTML('$$\\color{black}{\\text{scale: }\\sigma\\in\\mathbb{R}^+}$$')))),
                    h2("Support"),
                    h2("$$x\\in\\mathbb{R}^+$$"),
                    h2("Moments"),
                    h2("$$\\mathrm{E}(X) = \\text{exp}(\\mu + \\frac{\\sigma^2}{2})$$"),
                    h2("$$var(X) = \\left[\\text{exp}(\\sigma^2) - 1\\right] \\text{exp}(2\\mu + \\sigma^2)$$"),
                    h2("PDF"),
                    h2("$$f(x|\\mu,\\sigma) = \\frac{1}{x \\sigma \\sqrt{2 \\pi}} \\text{exp}\\left(-\\frac{(\\text{log } x - \\mu)^2}{2\\sigma^2}\\right)$$"),
                    h2("CDF"),
                    h2("$$F(x|\\mu,\\sigma) = \\frac{1}{2} + \\frac{1}{2} \\text{erf}\\left(\\frac{\\text{log } x - \\mu}{\\sqrt{2} \\sigma}\\right)$$"))
        }else if(input$dist=='Exponential'){
          withMathJax(h2("Parameters"),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{rate: }\\lambda\\in\\mathbb{R}^+}$$')))),
                      h2("Support"),
                      h2("$$x\\in\\mathbb{R}^+$$"),
                      h2("Moments"),
                      h2("$$\\mathrm{E}(X) = \\frac{1}{\\lambda}$$"),
                      h2("$$var(X) = \\frac{1}{\\lambda^2}$$"),
                      h2("PDF"),
                      h2("$$f(x|\\lambda) = \\lambda e^{-\\lambda x}$$"),
                      h2("CDF"),
                      h2("$$F(x|\\lambda) = 1 - e^{-\\lambda x}$$")
          )
        }else if(input$dist=='Gamma'){
          withMathJax(h2("Parameters"),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{shape: }\\alpha\\in\\mathbb{R}^+}$$')))),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{rate: }\\beta\\in\\mathbb{R}^+}$$')))),
                      h2("Support"),
                      h2("$$x\\in\\mathbb{R}^+$$"),
                      h2("Moments"),
                      h2("$$\\mathrm{E}(X) = \\frac{\\alpha}{\\beta}$$"),
                      h2("$$var(X) = \\frac{\\alpha}{\\beta^2}$$"),
                      h2("PDF"),
                      h2("$$f(x|\\alpha, \\beta) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x}$$"),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{where }\\Gamma(w) \\text{ is the gamma function}}$$')))),
                      h2("CDF"),
                      h2("$$F(x|\\alpha, \\beta) = \\frac{1}{\\Gamma(\\alpha)} \\gamma(\\alpha, \\beta x)$$"),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{where }\\gamma(w,v) \\text{ is the incomplete lower gamma function}}$$'))))
          )
        }else if(input$dist=='t'){
          withMathJax(h2("Parameters"),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{location or median or mode: }\\mu\\in\\mathbb{R}}$$')))),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{scale: }\\sigma\\in\\mathbb{R}^+}$$')))),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{degrees of freedom: }\\nu\\in\\mathbb{R}^+}$$')))),
                      h2("Support"),
                      h2("$$x\\in\\mathbb{R}$$"),
                      h2("Moments"),
                      h2("$$\\mathrm{E}(X) = \\mu, \\text{ if }\\nu>1 \\text{ otherwise undefined}$$"),
                      h2("$$var(X) = \\frac{\\nu \\sigma^2}{\\nu-2}$$"),
                      h2("PDF"),
                      h2("$$f(x|\\mu, \\sigma, \\nu) = \\frac{\\left(\\frac{\\nu }{\\nu +\\frac{(x-\\mu )^2}{\\sigma ^2}}\\right)^{\\frac{\\nu
                         +1}{2}}}{\\sqrt{\\nu } \\sigma  B\\left(\\frac{\\nu }{2},\\frac{1}{2}\\right)}$$"),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{where } B(u,v) \\text{ is the beta function}}$$')))),
                      h2("CDF"),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{F(\\mu, \\sigma, \\nu) = \\begin{cases}
                                      \\frac{1}{2} I_{\\frac{\\nu  \\sigma ^2}{(x-\\mu )^2+\\nu  \\sigma \
                                      ^2}}\\left(\\frac{\\nu }{2},\\frac{1}{2}\\right), & x\\leq \\mu  \\\\
                                      \\frac{1}{2} \\left(I_{\\frac{(x-\\mu )^2}{(x-\\mu )^2+\\nu  \\sigma \
                                      ^2}}\\left(\\frac{1}{2},\\frac{\\nu }{2}\\right)+1\\right), & \
                                      \\text{Otherwise}
                                      \\end{cases}}$$')))),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{where } I_w(u,v) \\text{ is the regularised beta function: }}$$')))),
                      h2('$$I_w(u,v) = \\frac{B(w; u, v)}{B(u,v)}$$'),
                      h2(withMathJax(
                        helpText(HTML('$$\\color{black}{\\text{where } B(w; u,v) \\text{ is the incomplete beta function and } B(u,v) \\text{ is the complete beta function}}$$'))))
                        )
          }else if(input$dist=='Beta'){
            withMathJax(h2("Parameters"),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\text{shape 1: }\\alpha\\in\\mathbb{R}^+}$$')))),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\text{shape 2: }\\beta\\in\\mathbb{R}^+}$$')))),
                        h2("Support"),
                        h2("$$x\\in (0, 1)$$"),
                        h2("Moments"),
                        h2("$$\\mathrm{E}(X) = \\frac{\\alpha}{\\alpha + \\beta}$$"),
                        h2("$$var(X) = \\frac{\\alpha\\beta}{(\\alpha + \\beta)^2 (\\alpha + \\beta + 1)}$$"),
                        h2("PDF"),
                        h2("$$f(x|\\alpha, \\beta) = \\frac{x^{\\alpha-1} (1-x)^{\\beta-1}}{B(\\alpha,\\beta)}$$"),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\text{where } B(u,v) \\text{ is the beta function}}$$')))),
                        h2("CDF"),
                        h2("$$F(x|\\alpha,\\beta) = I_x(\\alpha,\\beta)$$"),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\text{where } I_w(u,v) \\text{ is the regularised beta function: }}$$')))),
                        h2('$$I_w(u,v) = \\frac{B(w; u, v)}{B(u,v)}$$'),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\text{where } B(w; u,v) \\text{ is the incomplete beta function and } B(u,v) \\text{ is the complete beta function}}$$'))))
            )
          }else if(input$dist=='Cauchy'){
            withMathJax(h2("Parameters"),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\text{location or median or mode: }\\mu\\in\\mathbb{R}}$$')))),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\text{scale: }\\sigma\\in\\mathbb{R}^+}$$')))),
                        h2("Support"),
                        h2("$$x\\in \\mathbb{R}$$"),
                        h2("Moments"),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\mathrm{E}(X) = \\text{ undefined}}$$')))),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{var(X) = \\text{ undefined}}$$')))),
                        h2("PDF"),
                        h2("$$f(x|\\mu, \\sigma) = \\frac{1}{\\pi\\sigma\\left[1 + \\left(\\frac{x-\\mu}{\\sigma}\\right)^2\\right]}$$"),
                        h2("CDF"),
                        h2("$$F(x|\\mu, \\sigma) = \\frac{1}{2} + \\frac{1}{\\pi}\\text{arctan}\\left(\\frac{x-\\mu}{\\sigma}\\right)$$")
            )
          }else if(input$dist=='HalfCauchy'){
            withMathJax(h2("Parameters"),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\text{location: }\\mu\\in\\mathbb{R}}$$')))),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\text{scale: }\\sigma\\in\\mathbb{R}^+}$$')))),
                        h2("Support"),
                        h2("$$x\\in \\mathbb{R}^+$$"),
                        h2("Moments"),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{\\mathrm{E}(X) = \\text{ undefined}}$$')))),
                        h2(withMathJax(
                          helpText(HTML('$$\\color{black}{var(X) = \\text{ undefined}}$$')))),
                        h2("PDF"),
                        h2("$$f(x|\\mu, \\sigma) = \\begin{cases}
                           \\frac{1}{\\pi  \\sigma  \\left(\\frac{\\text{arctan}\\left(\\frac{\\mu \
                           }{\\sigma }\\right)}{\\pi }+\\frac{1}{2}\\right) \
                           \\left(\\frac{(x-\\mu )^2}{\\sigma ^2}+1\\right)}, & x>0 \\\\
                           0, & \\text{Otherwise}
                           \\end{cases}$$"),
                        h2("CDF"),
                        h2("$$F(x|\\mu, \\sigma) = \\begin{cases}
                           \\frac{\\frac{\\text{arctan}\\left(\\frac{\\mu }{\\sigma \
                           }\\right)}{\\pi }+\\frac{\\text{arctan}\\left(\\frac{x-\\mu }{\\sigma \
                           }\\right)}{\\pi }}{\\frac{\\text{arctan}\\left(\\frac{\\mu }{\\sigma \
                           }\\right)}{\\pi }+\\frac{1}{2}}, & x>0 \\\\
                           0, & \\text{Otherwise}
                           \\end{cases}$$")
                        )
            }else if(input$dist=='InverseGamma'){
              withMathJax(h2("Parameters"),
                          h2(withMathJax(
                            helpText(HTML('$$\\color{black}{\\text{shape: }\\alpha\\in\\mathbb{R}^+}$$')))),
                          h2(withMathJax(
                            helpText(HTML('$$\\color{black}{\\text{scale: }\\beta\\in\\mathbb{R}^+}$$')))),
                          h2("Support"),
                          h2("$$x\\in\\mathbb{R}^+$$"),
                          h2("Moments"),
                          h2("$$\\mathrm{E}(X) = \\begin{cases}
                             \\frac{\\beta }{\\alpha -1}, & \\alpha >1 \\\\
                             \\text{Indeterminate}, & \\text{Otherwise}
                             \\end{cases}$$"),
                          h2("$$var(X) = \\begin{cases}
                             \\frac{\\beta ^2}{(\\alpha -2) (\\alpha -1)^2}, & \\alpha >2 \\\\
                             \\text{Indeterminate}, & \\text{Otherwise}
                             \\end{cases}$$"),
                          h2("PDF"),
                          h2("$$f(x|\\alpha, \\beta) = \\begin{cases}
                             \\frac{e^{-\\frac{\\beta }{x}} \\left(\\frac{\\beta \
                             }{x}\\right)^{\\alpha }}{x \\Gamma (\\alpha )}, & x>0 \\\\
                             0, & \\text{Otherwise}
                             \\end{cases}$$"),
                          h2(withMathJax(
                            helpText(HTML('$$\\color{black}{\\text{where }\\Gamma(w) \\text{ is the gamma function}}$$')))),
                          h2("CDF"),
                          h2("$$F(x|\\alpha, \\beta) = \\begin{cases}
                             Q\\left(\\alpha ,\\frac{\\beta }{x}\\right), & x>0 \\\\
                             0, & \\text{Otherwise}
                             \\end{cases}$$"),
                          h2(withMathJax(
                            helpText(HTML('$$\\color{black}{\\text{where }Q(w,v) \\text{ is the regularised gamma function}}:$$')))),
                          h2("$$Q(w,v) = \\frac{\\Gamma(u,v)}{\\Gamma(u)}$$"),
                          h2(withMathJax(
                            helpText(HTML('$$\\color{black}{\\text{where }\\Gamma(w,v) \\text{ is the incomplete gamma function}}$$'))))
                          )
              }else if(input$dist=='LogitNormal'){
                withMathJax(h2("Parameters"),
                            h2(withMathJax(
                              helpText(HTML('$$\\color{black}{\\text{logit mean: }\\mu\\in\\mathbb{R}}$$')))),
                            h2(withMathJax(
                              helpText(HTML('$$\\color{black}{\\text{scale: }\\sigma\\in\\mathbb{R}^+}$$')))),
                            h2("Support"),
                            h2("$$x\\in\\mathbb{R}^+$$"),
                            h2("Moments"),
                            h2("$$\\mathrm{E}(X) = \\text{ No simple analytic expression}$$"),
                            h2("$$var(X) = \\text{ No simple analytic expression}$$"),
                            h2("PDF"),
                            h2("$$f(x|\\mu,\\sigma) = \\frac{1}{\\sigma\\sqrt{2\\pi}} \\text{exp}\\left(-\\frac{(\\text{logit } x - \\mu)^2}{2\\sigma^2}\\right) \\frac{1}{x(1-x)}$$"),
                            h2(withMathJax(
                              helpText(HTML('$$\\color{black}{\\text{where }\\text{logit } x = \\text{log}\\left(\\frac{x}{1-x}\\right)}$$')))),
                            h2("CDF"),
                            h2("$$F(x|\\mu,\\sigma) = \\frac{1}{2} + \\frac{1}{2} \\text{erf}\\left(\\frac{\\text{logit } x - \\mu}{\\sqrt{2} \\sigma}\\right)$$"),
                            h2(withMathJax(
                              helpText(HTML('$$\\color{black}{\\text{where }\\text{erf}(x) = \\frac{2}{\\sqrt{\\pi}}\\int_{0}^{x} e^{-t^2}\\mathrm{d}t \\text{ is the error function}}$$'))))
                )
              }
  }
}
