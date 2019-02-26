
fFormulae <- function(input){
  if (input$distType=='Continuous'){
    switch(input$dist,
           Normal=withMathJax(h2("Parameters"),
                              h2(withMathJax(
                                helpText(HTML('$$\\color{black}{\\text{mean: }\\mu\\in\\mathbb{R}}$$')))),
                              h2(withMathJax(
                                helpText(HTML('$$\\color{black}{\\text{standard deviation: }\\sigma\\in\\mathbb{R}^+}$$')))),
                              h2("Support"),
                              h2("$$x\\in\\mathbb{R}$$"),
                              h2("Moments"),
                              h2("$$\\mathrm{E}(X) = \\mu$$"),
                              h2("$$var(X) = \\sigma^2$$"),
                              h2("Probability density function (PDF)"),
                              h2("$$f(x|\\mu,\\sigma) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}} 
                                 \\text{exp}\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right)$$"),
                              h2("Cumulative distribution function (CDF)"),
                              h2("$$F(x|\\mu,\\sigma) = \\frac{1}{2}\\left[1+\\text{erf}\\left(\\frac{x-\\mu}{\\sigma\\sqrt{2}}\\right)\\right]$$"),
                              h2(withMathJax(
                                helpText(HTML('$$\\color{black}{\\text{where }\\text{erf}(x) = \\frac{2}{\\sqrt{\\pi}}\\int_{0}^{x} e^{-t^2}\\mathrm{d}t \\text{ is the error function}}$$'))))),
           Uniform=withMathJax(h2("Parameters"),
                               h2("$$-\\infty<a<b<+\\infty$$"),
                               h2("Support"),
                               h2("$$x\\in[a,b]$$"),
                               h2("Moments"),
                               h2("$$\\mathrm{E}(X) = \\frac{1}{2}(a + b)$$"),
                               h2("$$var(X) = \\frac{1}{12}(b - a)^2$$"),
                               h2("Probability density function (PDF)"),
                               h2(withMathJax(
                                 helpText(HTML('$$\\color{black}{f(x|a,b)=\\begin{cases}
                                               0,  & \\text{if }x \\not\\in [a,b] \\\\
                                               \\frac{1}{b-a}, & \\text{if } x \\in [a,b]
                                               \\end{cases}\\!}$$')))),
                               h2("Cumulative distribution function (CDF)"),
                               h2(withMathJax(
                                 helpText(HTML('$$\\color{black}{F(x|a,b)=\\begin{cases}
                                               0,  & \\text{if }x < a \\\\
                                               \\frac{x-a}{b-a}, & \\text{if } x\\in [a,b]\\\\
                                               1, & \\text{if } x > b
                                               \\end{cases}\\!}$$')))
                                 )),
           LogNormal=withMathJax(h2("Parameters"),
                                 h2(withMathJax(
                                   helpText(HTML('$$\\color{black}{\\text{log mean: }\\mu\\in\\mathbb{R}}$$')))),
                                 h2(withMathJax(
                                   helpText(HTML('$$\\color{black}{\\text{scale: }\\sigma\\in\\mathbb{R}^+}$$')))),
                                 h2("Support"),
                                 h2("$$x\\in\\mathbb{R}^+$$"),
                                 h2("Moments"),
                                 h2("$$\\mathrm{E}(X) = \\text{exp}(\\mu + \\frac{\\sigma^2}{2})$$"),
                                 h2("$$var(X) = \\left[\\text{exp}(\\sigma^2) - 1\\right] \\text{exp}(2\\mu + \\sigma^2)$$"),
                                 h2("Probability density function (PDF)"),
                                 h2("$$f(x|\\mu,\\sigma) = \\frac{1}{x \\sigma \\sqrt{2 \\pi}} \\text{exp}\\left(-\\frac{(\\text{log } x - \\mu)^2}{2\\sigma^2}\\right)$$"),
                                 h2("Cumulative distribution function (CDF)"),
                                 h2("$$F(x|\\mu,\\sigma) = \\frac{1}{2} + \\frac{1}{2} \\text{erf}\\left(\\frac{\\text{log } x - \\mu}{\\sqrt{2} \\sigma}\\right)$$"),
                                 h2(withMathJax(
                                   helpText(HTML('$$\\color{black}{\\text{where }\\text{erf}(x) = \\frac{2}{\\sqrt{\\pi}}\\int_{0}^{x} e^{-t^2}\\mathrm{d}t \\text{ is the error function}}$$'))))),
           Exponential=withMathJax(h2("Parameters"),
                                   h2(withMathJax(
                                     helpText(HTML('$$\\color{black}{\\text{rate: }\\lambda\\in\\mathbb{R}^+}$$')))),
                                   h2("Support"),
                                   h2("$$x\\in[0,\\mathbb{R}^+]$$"),
                                   h2("Moments"),
                                   h2("$$\\mathrm{E}(X) = \\frac{1}{\\lambda}$$"),
                                   h2("$$var(X) = \\frac{1}{\\lambda^2}$$"),
                                   h2("Probability density function (PDF)"),
                                   h2("$$f(x|\\lambda) = \\lambda e^{-\\lambda x}$$"),
                                   h2("Cumulative distribution function (CDF)"),
                                   h2("$$F(x|\\lambda) = 1 - e^{-\\lambda x}$$")),
           Gamma=withMathJax(h2("Parameters"),
                             h2(withMathJax(
                               helpText(HTML('$$\\color{black}{\\text{shape: }\\alpha\\in\\mathbb{R}^+}$$')))),
                             h2(withMathJax(
                               helpText(HTML('$$\\color{black}{\\text{rate: }\\beta\\in\\mathbb{R}^+}$$')))),
                             h2("Support"),
                             h2("$$x\\in\\mathbb{R}^+$$"),
                             h2("Moments"),
                             h2("$$\\mathrm{E}(X) = \\frac{\\alpha}{\\beta}$$"),
                             h2("$$var(X) = \\frac{\\alpha}{\\beta^2}$$"),
                             h2("Probability density function (PDF)"),
                             h2("$$f(x|\\alpha, \\beta) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x}$$"),
                             h2(withMathJax(
                               helpText(HTML('$$\\color{black}{\\text{where }\\Gamma(w)=\\int_{0}^{\\infty}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the gamma function}}$$')))),
                             h2("Cumulative distribution function (CDF)"),
                             h2("$$F(x|\\alpha, \\beta) = \\frac{1}{\\Gamma(\\alpha)} \\gamma(\\alpha, \\beta x)$$"),
                             h2(withMathJax(
                               helpText(HTML('$$\\color{black}{\\text{where }\\gamma(w,v)=\\int_{0}^{v}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the incomplete lower gamma function}}$$'))))),
           t=withMathJax(h2("Parameters"),
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
                         h2("Probability density function (PDF)"),
                         h2("$$f(x|\\mu, \\sigma, \\nu) = \\frac{\\left(\\frac{\\nu }{\\nu +\\frac{(x-\\mu )^2}{\\sigma ^2}}\\right)^{\\frac{\\nu
                            +1}{2}}}{\\sqrt{\\nu } \\sigma  B\\left(\\frac{\\nu }{2},\\frac{1}{2}\\right)}$$"),
                         h2(withMathJax(
                           helpText(HTML('$$\\color{black}{\\text{where } B(u,v)=\\int_{0}^{1}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the beta function}}$$')))),
                         h2("Cumulative distribution function (CDF)"),
                         h2(withMathJax(
                           helpText(HTML('$$\\color{black}{F(\\mu, \\sigma, \\nu) = \\begin{cases}
                                         \\frac{1}{2} I_{\\frac{\\nu  \\sigma ^2}{(x-\\mu )^2+\\nu  \\sigma \
                                         ^2}}\\left(\\frac{\\nu }{2},\\frac{1}{2}\\right), & x\\leq \\mu  \\\\
                                         \\frac{1}{2} \\left(I_{\\frac{(x-\\mu )^2}{(x-\\mu )^2+\\nu  \\sigma \
                                         ^2}}\\left(\\frac{1}{2},\\frac{\\nu }{2}\\right)+1\\right), & \
                                         \\text{Otherwise}
                                         \\end{cases}}$$')))),
                         h2(withMathJax(
                           helpText(HTML('$$\\color{black}{\\text{where } I_w(u,v) \\text{ is the regularised incomplete beta function: }}$$')))),
                         h2('$$I_w(u,v) = \\frac{B(w; u, v)}{B(u,v)}$$'),
                         h2(withMathJax(
                           helpText(HTML('$$\\color{black}{\\text{where } B(w; u,v)=\\int_{0}^{w}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the incomplete beta function and } B(u,v) \\text{ is the (complete) beta function}}$$'))))
                           ),
           Beta=withMathJax(h2("Parameters"),
                            h2(withMathJax(
                              helpText(HTML('$$\\color{black}{\\text{shape 1: }\\alpha\\in\\mathbb{R}^+}$$')))),
                            h2(withMathJax(
                              helpText(HTML('$$\\color{black}{\\text{shape 2: }\\beta\\in\\mathbb{R}^+}$$')))),
                            h2("Support"),
                            h2("$$x\\in (0, 1)$$"),
                            h2("Moments"),
                            h2("$$\\mathrm{E}(X) = \\frac{\\alpha}{\\alpha + \\beta}$$"),
                            h2("$$var(X) = \\frac{\\alpha\\beta}{(\\alpha + \\beta)^2 (\\alpha + \\beta + 1)}$$"),
                            h2("Probability density function (PDF)"),
                            h2("$$f(x|\\alpha, \\beta) = \\frac{x^{\\alpha-1} (1-x)^{\\beta-1}}{B(\\alpha,\\beta)}$$"),
                            h2(withMathJax(
                              helpText(HTML('$$\\color{black}{\\text{where } B(u,v)=\\int_{0}^{1}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the beta function}}$$')))),
                            h2("Cumulative distribution function (CDF)"),
                            h2("$$F(x|\\alpha,\\beta) = I_x(\\alpha,\\beta)$$"),
                            h2(withMathJax(
                              helpText(HTML('$$\\color{black}{\\text{where } I_w(u,v) \\text{ is the regularised beta function: }}$$')))),
                            h2('$$I_w(u,v) = \\frac{B(w; u, v)}{B(u,v)}$$'),
                            h2(withMathJax(
                              helpText(HTML('$$\\color{black}{\\text{where } B(w; u,v)=\\int_{0}^{w}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the incomplete beta function and } B(u,v) \\text{ is the (complete) beta function}}$$'))))),
           Cauchy=withMathJax(h2("Parameters"),
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
                              h2("Probability density function (PDF)"),
                              h2("$$f(x|\\mu, \\sigma) = \\frac{1}{\\pi\\sigma\\left[1 + \\left(\\frac{x-\\mu}{\\sigma}\\right)^2\\right]}$$"),
                              h2("Cumulative distribution function (CDF)"),
                              h2("$$F(x|\\mu, \\sigma) = \\frac{1}{2} + \\frac{1}{\\pi}\\text{arctan}\\left(\\frac{x-\\mu}{\\sigma}\\right)$$")),
           HalfCauchy=withMathJax(h2("Parameters"),
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
                                  h2("Probability density function (PDF)"),
                                  h2("$$f(x|\\mu, \\sigma) = \\begin{cases}
                                     \\frac{1}{\\pi  \\sigma  \\left(\\frac{1}{\\pi}\\text{arctan}\\left(\\frac{\\mu \
                                     }{\\sigma }\\right)+\\frac{1}{2}\\right) \
                                     \\left(\\frac{(x-\\mu )^2}{\\sigma ^2}+1\\right)}, & x>0 \\\\
                                     0, & \\text{Otherwise}
                                     \\end{cases}$$"),
                                  h2("Cumulative distribution function (CDF)"),
                                  h2("$$F(x|\\mu, \\sigma) = \\begin{cases}
                                     \\frac{1}{\\pi}\\frac{\\text{arctan}\\left(\\frac{\\mu }{\\sigma \
                                     }\\right)+\\text{arctan}\\left(\\frac{x-\\mu }{\\sigma \
                                     }\\right)}{\\frac{1}{\\pi}\\text{arctan}\\left(\\frac{\\mu }{\\sigma \
                                     }\\right)+\\frac{1}{2}}, & x>0 \\\\
                                     0, & \\text{Otherwise}
                                     \\end{cases}$$")
                                  ),
           InverseGamma=withMathJax(h2("Parameters"),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{shape: }\\alpha\\in\\mathbb{R}^+}$$')))),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{scale: }\\beta\\in\\mathbb{R}^+}$$')))),
                                    h2("Support"),
                                    h2("$$x\\in\\mathbb{R}^+$$"),
                                    h2("Moments"),
                                    h2("$$\\mathrm{E}(X) = \\begin{cases}
                                       \\frac{\\beta }{\\alpha -1}, & \\alpha >1 \\\\
                                       \\text{undefined}, & \\text{Otherwise}
                                       \\end{cases}$$"),
                                    h2("$$var(X) = \\begin{cases}
                                       \\frac{\\beta ^2}{(\\alpha -2) (\\alpha -1)^2}, & \\alpha >2 \\\\
                                       \\text{undefined}, & \\text{Otherwise}
                                       \\end{cases}$$"),
                                    h2("Probability density function (PDF)"),
                                    h2("$$f(x|\\alpha, \\beta) = \\begin{cases}
                                       \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)}x^{-\\alpha-1}\\text{exp}\\left(-\\frac{\\beta}{x}\\right), & x>0 \\\\
                                       0, & \\text{Otherwise}
                                       \\end{cases}$$"),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{where }\\Gamma(w)=\\int_{0}^{\\infty}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the gamma function}}$$')))),
                                    h2("Cumulative distribution function (CDF)"),
                                    h2("$$F(x|\\alpha, \\beta) = \\begin{cases}
                                       Q\\left(\\alpha ,\\frac{\\beta }{x}\\right), & x>0 \\\\
                                       0, & \\text{Otherwise}
                                       \\end{cases}$$"),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{where }Q(w,v) \\text{ is the regularised gamma function:}}$$')))),
                                    h2("$$Q(w,v) = \\frac{\\Gamma(u,v)}{\\Gamma(u)}$$"),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{where }\\Gamma(w,v)=\\int_{v}^{\\infty} t^{w-1} e^{-t}\\mathrm{d}t \\text{ is the upper incomplete gamma function}}$$'))))),
           LogitNormal=withMathJax(h2("Parameters"),
                                   h2(withMathJax(
                                     helpText(HTML('$$\\color{black}{\\text{logit mean: }\\mu\\in\\mathbb{R}}$$')))),
                                   h2(withMathJax(
                                     helpText(HTML('$$\\color{black}{\\text{scale: }\\sigma\\in\\mathbb{R}^+}$$')))),
                                   h2("Support"),
                                   h2("$$x\\in\\mathbb{R}^+$$"),
                                   h2("Moments"),
                                   h2("$$\\mathrm{E}(X) = \\text{ No simple analytic expression}$$"),
                                   h2("$$var(X) = \\text{ No simple analytic expression}$$"),
                                   h2("Probability density function (PDF)"),
                                   h2("$$f(x|\\mu,\\sigma) = \\frac{1}{\\sigma\\sqrt{2\\pi}} \\text{exp}\\left(-\\frac{(\\text{logit } x - \\mu)^2}{2\\sigma^2}\\right) \\frac{1}{x(1-x)}$$"),
                                   h2(withMathJax(
                                     helpText(HTML('$$\\color{black}{\\text{where }\\text{logit } x = \\text{log}\\left(\\frac{x}{1-x}\\right)}$$')))),
                                   h2("Cumulative distribution function (CDF)"),
                                   h2("$$F(x|\\mu,\\sigma) = \\frac{1}{2} + \\frac{1}{2} \\text{erf}\\left(\\frac{\\text{logit } x - \\mu}{\\sqrt{2} \\sigma}\\right)$$"),
                                   h2(withMathJax(
                                     helpText(HTML('$$\\color{black}{\\text{where }\\text{erf}(x) = \\frac{2}{\\sqrt{\\pi}}\\int_{0}^{x} e^{-t^2}\\mathrm{d}t \\text{ is the error function}}$$'))))),
           InverseChiSquared=withMathJax(h2("Parameters"),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{degrees of freedom: }\\nu\\in\\mathbb{R}^+}$$')))),
                                    h2("Support"),
                                    h2("$$x\\in\\mathbb{R}^+$$"),
                                    h2("Moments"),
                                    h2("$$\\mathrm{E}(X) = \\begin{cases}
                                       \\frac{1}{\\nu -2}, & \\nu >2 \\\\
                                       \\text{undefined}, & \\text{Otherwise}
                                       \\end{cases}$$"),
                                    h2("$$var(X) = \\begin{cases}
                                       \\frac{2}{(\\nu - 2)^2 (\\nu - 4)}, & \\nu >4 \\\\
                                       \\text{undefined}, & \\text{Otherwise}
                                       \\end{cases}$$"),
                                    h2("Probability density function (PDF)"),
                                    h2("$$f(x|\\nu) = \\begin{cases}
                                       \\frac{2^{-\\nu/2}}{\\Gamma(\\nu/2)} x^{-\\nu/2 - 1} e^{-1/(2x)}, & x>0 \\\\
                                       0, & \\text{Otherwise}
                                       \\end{cases}$$"),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{where }\\Gamma(w)=\\int_{0}^{\\infty}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the gamma function}}$$')))),
                                    h2("Cumulative distribution function (CDF)"),
                                    h2("$$F(x|\\nu) = \\begin{cases}
                                       \\frac{\\Gamma\\left(\\frac{\\nu}{2},\\frac{1}{2x}\\right)}{\\Gamma(\\frac{\\nu}{2})}, & x>0 \\\\
                                       0, & \\text{Otherwise}
                                       \\end{cases}$$"),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{where }\\Gamma(w,v)=\\int_{v}^{\\infty} t^{w-1} e^{-t}\\mathrm{d}t \\text{ is the incomplete upper gamma function}}$$')))))
           )
  }else if(input$distType=='Discrete'){
    switch(input$dist1,
           Bernoulli=withMathJax(h2("Parameters"),
                                 h2(withMathJax(
                                   helpText(HTML('$$\\color{black}{\\text{probability: }p\\in[0,1]}$$')))),
                                 h2("Support"),
                                 h2("$$x\\in\\{0,1\\}$$"),
                                 h2("Moments"),
                                 h2("$$\\mathrm{E}(X) = p$$"),
                                 h2("$$var(X) = p(1-p)$$"),
                                 h2("Probability mass function (PMF)"),
                                 h2("$$f(x|p) = p^x(1-p)^{1-x}$$"),
                                 h2("Cumulative distribution function (CDF)"),
                                 h2("$$F(x|p) = \\begin{cases}
                                    0, & x<0 \\\\
                                    1 - p, & 0\\leq x \\leq 1 \\\\
                                    1, & x>1
                                    \\end{cases}$$")),
           Binomial=withMathJax(h2("Parameters"),
                                h2(withMathJax(
                                  helpText(HTML('$$\\color{black}{\\text{number of trials: }n\\in\\{0,1,2,3,...\\}}$$')))),
                                h2(withMathJax(
                                  helpText(HTML('$$\\color{black}{\\text{probability: }p\\in[0,1]}$$')))),
                                h2("Support"),
                                h2("$$x\\in\\{0,1,2,3,...,n\\}$$"),
                                h2("Moments"),
                                h2("$$\\mathrm{E}(X) = np$$"),
                                h2("$$var(X) = np(1-p)$$"),
                                h2("Probability mass function (PMF)"),
                                h2("$$f(x|n,p) = \\binom{n}{x} p^x(1-p)^{n-x}$$"),
                                h2("Cumulative distribution function (CDF)"),
                                h2("$$F(x|n,p) = I_{1-p}(n-x,1+x)$$"),
                                h2(withMathJax(
                                  helpText(HTML('$$\\color{black}{\\text{where }I_{1-p}(n-x,x+1)=(n-x)\\binom{n}{x}\\int_{0}^{1-p}t^{n-x-1}(1-t)^x \\mathrm{d}t}$$')))),
                                h2(withMathJax(
                                  helpText(HTML('$$\\color{black}{\\text{is the regularised incomplete beta function}}$$'))))),
           DiscreteUniform=withMathJax(h2("Parameters"),
                               h2("$$a\\in \\mathbb{Z}$$"),
                               h2("$$b\\in \\mathbb{Z}$$"),
                               h2(withMathJax(
                                 helpText(HTML('$$\\color{black}{\\text{where } a\\leq b}$$')))),
                               h2("Support"),
                               h2("$$x\\in\\{a,a+1,...,b-1,b\\}$$"),
                               h2("Moments"),
                               h2("$$\\mathrm{E}(X) = \\frac{a + b}{2}$$"),
                               h2("$$var(X) = \\frac{(b - a + 1)^2 - 1}{12}$$"),
                               h2("Probability mass function (PMF)"),
                               h2("$$f(x|a, b) = \\frac{1}{b - a + 1}$$"),
                               h2("Cumulative distribution function (CDF)"),
                               h2("$$F(x|a, b) = \\frac{\\lfloor x \\rfloor - a - 1}{b - a + 1}$$"),
                               h2(withMathJax(
                                 helpText(HTML('$$\\color{black}{\\text{and } \\lfloor x \\rfloor \\text{ is the floor function (rounds down reals to nearest smaller integer)}}$$'))))),
           Poisson=withMathJax(h2("Parameters"),
                               h2(withMathJax(
                                 helpText(HTML('$$\\color{black}{\\text{rate: }\\lambda\\in\\mathbb{R}^+}$$')))),
                               h2("Support"),
                               h2("$$x\\in\\{0,1,2,3,...\\}$$"),
                               h2("Moments"),
                               h2("$$\\mathrm{E}(X) = \\lambda$$"),
                               h2("$$var(X) = \\lambda$$"),
                               h2("Probability mass function (PMF)"),
                               h2("$$f(x|\\lambda) = \\frac{\\lambda^x e^{-\\lambda}}{x!}$$"),
                               h2("Cumulative distribution function (CDF)"),
                               h2("$$F(x|\\lambda) = \\frac{\\Gamma(\\lfloor x+1\\rfloor,\\lambda)}{\\lfloor x \\rfloor !}$$"),
                               h2(withMathJax(
                                 helpText(HTML('$$\\color{black}{\\text{where }\\Gamma(u,v)=\\int_{v}^{\\infty}t^{u-1}e^{-t} \\mathrm{d}t \\text{ is the upper incomplete gamma function}}$$')))),
                               h2(withMathJax(
                                 helpText(HTML('$$\\color{black}{\\text{and } \\lfloor x \\rfloor \\text{ is the floor function (rounds down reals to nearest smaller integer)}}$$'))))),
           NegativeBinomial=withMathJax(h2("Parameters"),
                                        h2(withMathJax(
                                          helpText(HTML('$$\\color{black}{\\text{rate: }\\lambda\\in\\mathbb{R}^+}$$')))),
                                        h2(withMathJax(
                                          helpText(HTML('$$\\color{black}{\\text{inverse-dispersion: }\\kappa\\in\\mathbb{R}^+}$$')))),
                                        h2("Support"),
                                        h2("$$x\\in\\{0,1,2,3,...\\}$$"),
                                        h2("Moments"),
                                        h2("$$\\mathrm{E}(X) = \\lambda$$"),
                                        h2("$$var(X) = \\lambda + \\frac{\\lambda^2}{\\kappa}$$"),
                                        h2("Probability mass function (PMF)"),
                                        h2("$$f(x|\\lambda,\\kappa) = \\frac{\\Gamma(x+\\kappa)}{x!\\Gamma(\\kappa+1)}\\left(\\frac{\\lambda}{\\lambda+\\kappa}\\right)^x \\left(\\frac{\\kappa}{\\lambda+\\kappa}\\right)^\\kappa$$"),
                                        h2("Cumulative distribution function (CDF)"),
                                        h2("$$F(x|\\lambda,\\kappa) = \\begin{cases}
                                       I_{\\frac{\\kappa}{\\kappa+\\lambda}}(\\kappa,1+\\lfloor x \\rfloor), & x\\geq 0 \\\\
                                           0, & \\text{Otherwise}
                                           \\end{cases}$$"),
                                        h2(withMathJax(
                                          helpText(HTML('$$\\color{black}{\\text{where } I_w(u,v) \\text{ is the regularised incomplete beta function: }}$$')))),
                                        h2('$$I_w(u,v) = \\frac{B(w; u, v)}{B(u,v)}$$'),
                                        h2(withMathJax(
                                          helpText(HTML('$$\\color{black}{\\text{where } B(w; u,v)=\\int_{0}^{w}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the incomplete beta function and } B(u,v)=\\int_{0}^{1}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the complete beta function}}$$'))))),
           BetaBinomial=withMathJax(h2("Parameters"),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{number of trials: }n\\in\\{0,1,2,3,...\\}}$$')))),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{shape 1: }\\alpha\\in\\mathbb{R}^+}$$')))),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{shape 2: }\\beta\\in\\mathbb{R}^+}$$')))),
                                    h2("Support"),
                                    h2("$$x\\in\\{0,1,2,3,...,n\\}$$"),
                                    h2("Moments"),
                                    h2("$$\\mathrm{E}(X) = \\frac{n\\alpha}{\\alpha+\\beta}$$"),
                                    h2("$$var(X) = \\frac{n\\alpha\\beta(\\alpha+\\beta+n)}{(\\alpha+\\beta)^2(\\alpha+\\beta+1)}$$"),
                                    h2("Probability mass function (PMF)"),
                                    h2("$$f(x|n,\\alpha,\\beta) = \\binom{n}{x}\\frac{B(x+\\alpha,n-x+\\beta)}{B(\\alpha,\\beta)}$$"),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{where } B(u,v)=\\int_{0}^{1}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the (complete) beta function }}$$')))),
                                    h2("Cumulative distribution function (CDF)"),
                                    h2("$$F(x|n,\\alpha,\\beta) = \\begin{cases}
                                    0, & x<0 \\\\
                                       \\binom{n}{x}\\frac{B(x+\\alpha,n-x+\\beta)}{B(\\alpha,\\beta)} {}_{3}F_2(1,-x,n-x+\\beta;n-x-1,1-x-\\alpha;1), & 0\\leq x \\leq n \\\\
                                       1, & x>n
                                       \\end{cases}$$"),
                                    h2(withMathJax(
                                      helpText(HTML('$$\\color{black}{\\text{where } {}_{3}F_2(a,b,x) \\text{ is the generalised hypergeometric function}}$$')))))
                                        
    )
    }else if(input$distType=='Multivariate'){
      switch(input$dist2,
             MultivariateNormal=withMathJax(h2("Parameters"),
                                            h2(withMathJax(
                                              helpText(HTML('$$\\color{black}{\\text{mean vector: }\\mu\\in\\mathbb{R}^d}$$')))),
                                            h2(withMathJax(
                                              helpText(HTML('$$\\color{black}{\\text{covariance matrix (positive semidefinite): }\\Sigma\\in\\mathbb{R}^{d\\times d}}$$')))),
                                            h2("Support"),
                                            h2("$$x\\in\\mathbb{R}^d$$"),
                                            h2("Moments"),
                                            h2("$$\\mathrm{E}(X) = \\mu$$"),
                                            h2("$$var(X) = \\Sigma$$"),
                                            h2("Probability density function (PDF)"),
                                            h2("$$f(x|\\mu,\\Sigma) = \\frac{1}{(2\\pi)^{d/2}|\\Sigma|^{1/2}} \\text{exp}\\left(-\\frac{1}{2}(x-\\mu)'\\Sigma^{-1}(x-\\mu)\\right)$$"),
                                            h2("Cumulative distribution function (CDF)"),
                                            h2("$$\\text{No analytic expression}$$")),
             MultivariateT=withMathJax(h2("Parameters"),
                                       h2(withMathJax(
                                         helpText(HTML('$$\\color{black}{\\text{mode vector: }\\mu\\in\\mathbb{R}^d}$$')))),
                                       h2(withMathJax(
                                         helpText(HTML('$$\\color{black}{\\text{shape matrix (positive semidefinite): }\\Sigma\\in\\mathbb{R}^{d\\times d}}$$')))),
                                       h2(withMathJax(
                                         helpText(HTML('$$\\color{black}{\\text{degrees of freedom: }\\nu\\in\\mathbb{R}^+}$$')))),
                                       h2("Support"),
                                       h2("$$x\\in\\mathbb{R}^d$$"),
                                       h2("Moments"),
                                       h2("$$\\mathrm{E}(X) = \\begin{cases}
                                          \\mu, & \\nu>1 \\\\
                                          \\text{undefined}, & \\text{otherwise}
                                          \\end{cases}$$"),
                                       h2("$$var(X) = \\begin{cases}
                                          \\frac{\\nu}{\\nu-2}\\Sigma, & \\nu>2 \\\\
                                          \\text{undefined}, & \\text{otherwise}
                                          \\end{cases}$$"),
                                       h2("Probability density function (PDF)"),
                                       h2("$$f(x|\\mu,\\Sigma, \\nu) = \\frac{\\Gamma((\\nu+d)/2)}{\\Gamma(\\nu/2)\\nu^{d/2}\\pi^{d/2}|\\Sigma|^{1/2}}\\left[1+\\frac{1}{\\nu}(x-\\mu)'\\Sigma^{-1}(x-\\mu)\\right]^{-(\\nu+d)/2}$$"),
                                       h2(withMathJax(
                                         helpText(HTML('$$\\color{black}{\\text{where }\\Gamma(w)=\\int_{0}^{\\infty}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the gamma function}}$$')))),
                                       h2("Cumulative distribution function (CDF)"),
                                       h2("$$\\text{No analytic expression}$$")),
             Multinomial=withMathJax(h2("Parameters"),
                                     h2(withMathJax(
                                       helpText(HTML('$$\\color{black}{\\text{number of trials: }n\\in\\{0,1,2,3,...\\}}$$')))),
                                     h2(withMathJax(
                                       helpText(HTML('$$\\color{black}{\\text{event probabilities: } p_1, p_2,...,p_d \\text{ such that } \\sum_{i=1}^{d}p_i=1}$$')))),
                                     h2("Support"),
                                     h2("$$x_i\\in\\{0,1,2,3,...,n\\} \\text{, }\\forall i$$"),
                                     h2("$$\\text{such that } \\sum_{i=1}^{d}x_i=n$$"),
                                     h2("Moments"),
                                     h2("$$\\mathrm{E}(X_i) = n p_i \\text{, }\\forall i$$"),
                                     h2("$$var(X_i) = n p_i (1-p_i) \\text{, }\\forall i$$"),
                                     h2("$$cov(X_i,X_j) = -n p_i p_j \\text{, }\\forall i\\neq j$$"),
                                     h2("Probability mass function (PMF)"),
                                     h2("$$f(x_1,x_2,...,x_d|n,p_1,p_2,...,p_d) = \\frac{n!}{x_1 ! x_2 ! ... x_d !} p_1^{x_1} p_2^{x_2}...p_d^{x_d} $$"),
                                     h2("Cumulative distribution function (CDF)"),
                                     h2("$$\\text{No analytic expression}$$")),
             Wishart=withMathJax(h2("Parameters"),
                                 h2(withMathJax(
                                   helpText(HTML('$$\\color{black}{\\text{degrees of freedom: }\\nu>d-1 \\text{ where } d \\text{ is dimensions of scale matrix}}$$')))),
                                 h2(withMathJax(
                                   helpText(HTML('$$\\color{black}{\\text{scale matrix (positive definite): } \\Psi\\in\\mathbb{R}^{d\\times d}}$$')))),
                                 h2("Support"),
                                 h2("$$X\\in\\mathbb{R}^{d\\times d} \\text{ (also positive definite) }$$"),
                                 h2("Moments"),
                                 h2("$$\\mathrm{E}(X) = \\nu \\Psi$$"),
                                 h2("$$var(X_{i,j}) = \\nu(\\Psi_{i,j}^2+\\Psi_{i,i}\\Psi_{j,j})$$"),
                                 h2("Probability density function (PDF)"),
                                 h2("$$f(X|\\nu,\\Psi) = |X|^{(\\nu-d-1)/2} \\text{exp}(-\\text{tr}(\\Psi^{-1} X)/2)\\frac{1}{2^{\\nu d/2} |\\Psi|^{\\nu/2} \\Gamma_d(\\nu/2)}$$"),
                                 h2(withMathJax(
                                   helpText(HTML('$$\\color{black}{\\text{where }  \\Gamma_p(a)=\\pi^{p(p-1)/4}\\prod_{j=1}^{p}\\Gamma(a+(1-j)/2) \\text{ is the multivariate gamma function}}$$')))),
                                 h2("Cumulative distribution function (CDF)"),
                                 h2("$$\\text{No analytic expression}$$")),
             InverseWishart=withMathJax(h2("Parameters"),
                                 h2(withMathJax(
                                   helpText(HTML('$$\\color{black}{\\text{degrees of freedom: }\\nu>d-1 \\text{ where } d \\text{ is dimensions of scale matrix}}$$')))),
                                 h2(withMathJax(
                                   helpText(HTML('$$\\color{black}{\\text{scale matrix (positive definite): } \\Psi\\in\\mathbb{R}^{d\\times d}}$$')))),
                                 h2("Support"),
                                 h2("$$X\\in\\mathbb{R}^{d\\times d} \\text{ (also positive definite) }$$"),
                                 h2("Moments"),
                                 h2("$$\\mathrm{E}(X) = \\begin{cases}
                                          \\frac{\\Psi}{\\nu-d-1}, & \\nu>d+1 \\\\
                                    \\text{undefined}, & \\text{otherwise}
                                    \\end{cases}$$"),
                                 h2("$$var(X_{i,j}) = \\begin{cases}
                                          \\frac{(\\nu-d+1)\\Psi_{i,j}^2 + (\\nu-d-1)\\Psi_{i,i}\\Psi_{i,j}}{(\\nu-d)(\\nu-d-1)^2(\\nu-d-3)}, & \\nu>d+3 \\\\
                                    \\text{undefined}, & \\text{otherwise}
                                    \\end{cases}$$"),
                                 h2("Probability density function (PDF)"),
                                 h2("$$f(X|\\nu,\\Psi) = |\\Psi|^{\\nu/2} |X|^{-(\\nu+d+1)/2} \\text{exp}(-\\text{tr}(\\Psi X^{-1})/2)\\frac{1}{2^{\\nu d/2}  \\Gamma_d(\\nu/2)}$$"),
                                 h2(withMathJax(
                                   helpText(HTML('$$\\color{black}{\\text{where }  \\Gamma_p(a)=\\pi^{p(p-1)/4}\\prod_{j=1}^{p}\\Gamma(a+(1-j)/2) \\text{ is the multivariate gamma function}}$$')))),
                                 h2("Cumulative distribution function (CDF)"),
                                 h2("$$\\text{No analytic expression}$$")),
             LKJ=withMathJax(h2("Parameters"),
                             h2(withMathJax(
                               helpText(HTML('$$\\color{black}{\\text{degrees of freedom: }\\nu \\in \\mathbb{R}^+}$$')))),
                             h2(withMathJax(
                               helpText(HTML('$$\\color{black}{\\text{dimensions: } d}$$')))),
                             h2("Support"),
                             h2("$$X\\in\\mathbb{R}^{d\\times d} \\text{ (with unit diagonals and positive definite) }$$"),
                             h2("Moments"),
                             h2("$$\\mathrm{E}(X) = \\mathcal{I}_d$$"),
                             h2("$$var(X_{i,j}) = \\frac{4\\left(\\frac{d}{2}+\\nu -1\\right)^2}{(d+2 \\nu -2)^2 (d+2 \\nu -1)}$$"),
                             h2("Probability density function (PDF)"),
                             h2("$$f(X|\\nu,d) = \\left(2^{\\sum_{k=1}^{d-1}(2\\nu-2+d-k)(d-k)}\\prod_{k=1}^{d-1}\\left[B(\\nu+0.5(d-k-1),\\nu+0.5(d-k-1))\\right]^{d-k}\\right) |X|^{\\nu-1}$$"),
                             h2(withMathJax(
                               helpText(HTML('$$\\color{black}{\\text{where }  B(u,v)=\\int_{0}^{1}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is beta function}}$$')))),
                             h2("Cumulative distribution function (CDF)"),
                             h2("$$\\text{No analytic expression}$$"),
                             h2("Marginal density"),
                             h2("$$X_{i,j}\\sim \\text{beta}(\\nu - 1 + d / 2, \\nu - 1 + d / 2), i\\neq j, \\text{ on } (-1,1).$$")),
             Dirichlet=withMathJax(h2("Parameters"),
                                   h2(withMathJax(
                                     helpText(HTML('$$\\color{black}{\\text{category concentrations: }\\alpha_1,\\alpha_2,...,\\alpha_d \\text{ where } d\\geq 2}$$')))),
                                   h2("Support"),
                                   h2("$$x_i\\in(0,1) \\text{, }\\forall i \\text{ such that } \\sum_{i=1}^{d}x_i=1$$"),
                                   h2("Moments"),
                                   h2("$$\\mathrm{E}(X_i) = \\frac{\\alpha_i}{\\sum_{k=1}^{d}\\alpha_k}$$"),
                                   h2("$$var(X_i) = \\frac{\\alpha_i(\\alpha_0-\\alpha_i)}{\\alpha_0^2(\\alpha_0+1)}\\text{, where }\\alpha_0=\\sum_{i=1}^{d}\\alpha_i$$"),
                                   h2("$$$$"),
                                   h2("$$cov(X_i,X_j) = -\\frac{\\alpha_i \\alpha_j}{\\alpha_0^2(\\alpha_0+1)} \\text{, where }i\\neq j$$"),
                                   h2("Probability density function (PDF)"),
                                   h2("$$f(x_1,x_2,...,x_d|\\alpha_1,\\alpha_2,...,\\alpha_d) = \\frac{1}{B(\\alpha_1,\\alpha_2,...,\\alpha_d)}\\prod_{i=1}^{d}x_i^{\\alpha_i-1}$$"),
                                   h2("$$\\text{where }B(\\alpha_1,\\alpha_2,...,\\alpha_d)=\\frac{\\prod_{i=1}^{d}\\Gamma(\\alpha_i)}{\\Gamma(\\sum_{i=1}^{d}\\alpha_i)}$$"),
                                   h2("Cumulative distribution function (CDF)"),
                                   h2("$$\\text{No analytic expression}$$"))
      )
  }
}
