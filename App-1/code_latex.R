fPrismLatex <- function(text){
  return(prismCodeBlock(text, language = "latex"))
}

fLatexHelper <- function(lMoments, lPDF, lCDF=NULL){
  moments_latex <- lapply(lMoments, fPrismLatex)
  pdf_latex <- lapply(lPDF, fPrismLatex)
  cdf_latex <- lapply(lCDF, fPrismLatex)
  if(!is.null(lCDF))
    return(tagList(h2("Moments"),
                 moments_latex,
                 h2("Probability density function (PDF)"),
                 pdf_latex,
                 h2("Cumulative distribution function (CDF)"),
                 cdf_latex))
  else
    return(tagList(h2("Moments"),
                   moments_latex,
                   h2("Probability density function (PDF)"),
                   pdf_latex))
}

fLatexHelper_discrete <- function(lMoments, lPDF, lCDF=NULL){
  moments_latex <- lapply(lMoments, fPrismLatex)
  pdf_latex <- lapply(lPDF, fPrismLatex)
  cdf_latex <- lapply(lCDF, fPrismLatex)
  if(!is.null(lCDF))
    return(tagList(h2("Moments"),
                 moments_latex,
                 h2("Probability mass function (PMF)"),
                 pdf_latex,
                 h2("Cumulative distribution function (CDF)"),
                 cdf_latex))
  else
    return(tagList(h2("Moments"),
                   moments_latex,
                   h2("Probability mass function (PMF)"),
                   pdf_latex))
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
                                "\\text{where }\\gamma(w,v)=\\int_{0}^{v}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the incomplete lower gamma function}}")),
           t=fLatexHelper(c("\\mathrm{E}(X) = \\mu, \\text{ if }\\nu>1 \\text{ otherwise undefined}",
                            "var(X) = \\frac{\\nu \\sigma^2}{\\nu-2}"),
                          c("f(x|\\mu, \\sigma, \\nu) = \\frac{\\left(\\frac{\\nu }{\\nu +\\frac{(x-\\mu )^2}{\\sigma ^2}}\\right)^{\\frac{\\nu
                            +1}{2}}}{\\sqrt{\\nu } \\sigma  B\\left(\\frac{\\nu }{2},\\frac{1}{2}\\right)}",
                            "\\text{where } B(u,v)=\\int_{0}^{1}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the beta function}}"),
                          c("F(\\mu, \\sigma, \\nu) = \\begin{cases}
                                         \\frac{1}{2} I_{\\frac{\\nu  \\sigma ^2}{(x-\\mu )^2+\\nu  \\sigma \
                            ^2}}\\left(\\frac{\\nu }{2},\\frac{1}{2}\\right), & x\\leq \\mu  \\\\
                            \\frac{1}{2} \\left(I_{\\frac{(x-\\mu )^2}{(x-\\mu )^2+\\nu  \\sigma \
                            ^2}}\\left(\\frac{1}{2},\\frac{\\nu }{2}\\right)+1\\right), & \
                            \\text{Otherwise}
                            \\end{cases}}",
                            "\\text{where } I_w(u,v) \\text{ is the regularised incomplete beta function: }}",
                            "I_w(u,v) = \\frac{B(w; u, v)}{B(u,v)}",
                            "\\text{where } B(w; u,v)=\\int_{0}^{w}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the incomplete beta function and } B(u,v) \\text{ is the (complete) beta function}}")),
           Beta=fLatexHelper(c("\\mathrm{E}(X) = \\frac{\\alpha}{\\alpha + \\beta}",
                               "var(X) = \\frac{\\alpha\\beta}{(\\alpha + \\beta)^2 (\\alpha + \\beta + 1)}"),
                             c("f(x|\\alpha, \\beta) = \\frac{x^{\\alpha-1} (1-x)^{\\beta-1}}{B(\\alpha,\\beta)}",
                               "\\text{where } B(u,v)=\\int_{0}^{1}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the beta function}}"),
                             c("F(x|\\alpha,\\beta) = I_x(\\alpha,\\beta)",
                               "\\text{where } I_w(u,v) \\text{ is the regularised beta function: }}",
                               "I_w(u,v) = \\frac{B(w; u, v)}{B(u,v)}",
                               "\\text{where } B(w; u,v)=\\int_{0}^{w}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the incomplete beta function and } B(u,v) \\text{ is the (complete) beta function}}")),
           Cauchy=fLatexHelper(c("\\mathrm{E}(X) = \\text{ undefined}",
                                 "var(X) = \\text{ undefined}"),
                               c("f(x|\\mu, \\sigma) = \\frac{1}{\\pi\\sigma\\left[1 + \\left(\\frac{x-\\mu}{\\sigma}\\right)^2\\right]}"),
                               c("F(x|\\mu, \\sigma) = \\frac{1}{2} + \\frac{1}{\\pi}\\text{arctan}\\left(\\frac{x-\\mu}{\\sigma}\\right)")),
           HalfCauchy=fLatexHelper(c("\\mathrm{E}(X) = \\text{ undefined}",
                                     "var(X) = \\text{ undefined}"),
                                   c("f(x|\\mu, \\sigma) = \\begin{cases}
\\frac{1}{\\pi  \\sigma  \\left(\\frac{1}{\\pi}\\text{arctan}\\left(\\frac{\\mu}{\\sigma }\\right)+\\frac{1}{2}\\right)
                                     \\left(\\frac{(x-\\mu )^2}{\\sigma ^2}+1\\right)}, & x>0 \\\\
                                     0, & \\text{Otherwise}
                                     \\end{cases}"),
                                   c("F(x|\\mu, \\sigma) = \\begin{cases}
\\frac{1}{\\pi}\\frac{\\text{arctan}\\left(\\frac{\\mu}{\\sigma}\\right)+
\\text{arctan}\\left(\\frac{x-\\mu }{\\sigma}\\right)}{\\frac{1}{\\pi}\\text{arctan}\\left(\\frac{\\mu }{\\sigma}\\right)
+\\frac{1}{2}}, & x>0 \\\\
                                     0, & \\text{Otherwise}
                                     \\end{cases}")),
           InverseGamma=fLatexHelper(c("\\mathrm{E}(X) = \\begin{cases}
\\frac{\\beta }{\\alpha -1}, & \\alpha >1 \\\\
\\text{undefined}, & \\text{Otherwise}
\\end{cases}",
"var(X) = \\begin{cases}
\\frac{\\beta ^2}{(\\alpha -2) (\\alpha -1)^2}, & \\alpha >2 \\\\
\\text{undefined}, & \\text{Otherwise}
\\end{cases}"),
                                     c("f(x|\\alpha, \\beta) = \\begin{cases}
\\frac{\\beta^\\alpha}{\\Gamma(\\alpha)}x^{-\\alpha-1}\\text{exp}\\left(-\\frac{\\beta}{x}\\right), & x>0 \\\\
0, & \\text{Otherwise}
\\end{cases}",
                                       "\\text{where }\\Gamma(w)=\\int_{0}^{\\infty}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the gamma function}"),
                                     c("F(x|\\alpha, \\beta) = \\begin{cases}
Q\\left(\\alpha ,\\frac{\\beta }{x}\\right), & x>0 \\\\
0, & \\text{Otherwise}
\\end{cases}",
                                       "\\text{where }Q(w,v) \\text{ is the regularised gamma function:}",
                                       "Q(w,v) = \\frac{\\Gamma(u,v)}{\\Gamma(u)}",
                                       "\\text{where }\\Gamma(w,v)=\\int_{v}^{\\infty} t^{w-1} e^{-t}\\mathrm{d}t \\text{ is the upper incomplete gamma function}")),
          InverseChiSquared=fLatexHelper(c("\\mathrm{E}(X) = \\begin{cases}
\\frac{1}{\\nu -2}, & \\nu >2 \\\\
\\text{undefined}, & \\text{Otherwise}
\\end{cases}",
                                           "var(X) = \\begin{cases}
\\frac{2}{(\\nu - 2)^2 (\\nu - 4)}, & \\nu >4 \\\\
\\text{undefined}, & \\text{Otherwise}
\\end{cases}"),
                                         c("f(x|\\nu) = \\begin{cases}
\\frac{2^{-\\nu/2}}{\\Gamma(\\nu/2)} x^{-\\nu/2 - 1} e^{-1/(2x)}, & x>0 \\\\
0, & \\text{Otherwise}
\\end{cases}",
                                           "\\text{where }\\Gamma(w)=\\int_{0}^{\\infty}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the gamma function}"),
                                         c("F(x|\\nu) = \\begin{cases}
\\frac{\\Gamma\\left(\\frac{\\nu}{2},\\frac{1}{2x}\\right)}{\\Gamma(\\frac{\\nu}{2})}, & x>0 \\\\
0, & \\text{Otherwise}
\\end{cases}",
                                           "\\text{where }\\Gamma(w,v)=\\int_{v}^{\\infty} t^{w-1} e^{-t}\\mathrm{d}t \\text{ is the incomplete upper gamma function}")),
          LogitNormal=fLatexHelper(c("\\mathrm{E}(X) = \\text{ No simple analytic expression}",
                                     "var(X) = \\text{ No simple analytic expression}"),
                                   c("f(x|\\mu,\\sigma) = \\frac{1}{\\sigma\\sqrt{2\\pi}} \\text{exp}\\left(-\\frac{(\\text{logit } x - \\mu)^2}{2\\sigma^2}\\right) \\frac{1}{x(1-x)}",
                                     "\\text{where }\\text{logit } x = \\text{log}\\left(\\frac{x}{1-x}\\right)"),
                                   c("F(x|\\mu,\\sigma) = \\frac{1}{2} + \\frac{1}{2} \\text{erf}\\left(\\frac{\\text{logit } x - \\mu}{\\sqrt{2} \\sigma}\\right)",
                                     "\\text{where }\\text{erf}(x) = \\frac{2}{\\sqrt{\\pi}}\\int_{0}^{x} e^{-t^2}\\mathrm{d}t \\text{ is the error function}"))
    )
  }else if(input$distType=='Discrete'){
    switch(input$dist1,
           Bernoulli=fLatexHelper_discrete(c("\\mathrm{E}(X) = p",
                                             "var(X) = p(1-p)"),
                                           c("f(x|p) = p^x(1-p)^{1-x}"),
                                           c("F(x|p) = \\begin{cases}
0, & x<0 \\\\
1 - p, & 0\\leq x \\leq 1 \\\\
1, & x>1
\\end{cases}")),
           Binomial=fLatexHelper_discrete(c("\\mathrm{E}(X) = np",
                                            "var(X) = np(1-p)"),
                                          c("f(x|n,p) = \\binom{n}{x} p^x(1-p)^{n-x}"),
                                          c("F(x|n,p) = I_{1-p}(n-x,1+x)",
                                            "\\text{where }I_{1-p}(n-x,x+1)=(n-x)\\binom{n}{x}\\int_{0}^{1-p}t^{n-x-1}(1-t)^x \\mathrm{d}t \\text{ is the regularised incomplete beta function}")),
           DiscreteUniform=fLatexHelper_discrete(c("\\mathrm{E}(X) = \\frac{a + b}{2}",
                                           "var(X) = \\frac{(b - a + 1)^2 - 1}{12}"),
                                         c("f(x|a, b) = \\frac{1}{b - a + 1}"),
                                         c("F(x|a, b) = \\frac{\\lfloor x \\rfloor - a - 1}{b - a + 1}",
                                           "\\text{where} \\lfloor x \\rfloor \\text{ is the floor function (rounds down reals to nearest smaller integer)}")),
           Poisson=fLatexHelper_discrete(c("\\mathrm{E}(X) = \\lambda",
                                           "var(X) = \\lambda"),
                                         c("f(x|\\lambda) = \\frac{\\lambda^x e^{-\\lambda}}{x!}"),
                                         c("F(x|\\lambda) = \\frac{\\Gamma(\\lfloor x+1\\rfloor,\\lambda)}{\\lfloor x \\rfloor !}",
                                           "\\text{where }\\Gamma(u,v)=\\int_{v}^{\\infty}t^{u-1}e^{-t} \\mathrm{d}t \\text{ is the upper incomplete gamma function}",
                                           "\\text{and } \\lfloor x \\rfloor \\text{ is the floor function (rounds down reals to nearest smaller integer)}")),
           NegativeBinomial=fLatexHelper_discrete(c("\\mathrm{E}(X) = \\lambda",
                                                    "var(X) = \\lambda + \\frac{\\lambda^2}{\\kappa}"),
                                                  c("f(x|\\lambda,\\kappa) = \\frac{\\Gamma(x+\\kappa)}{x!\\Gamma(\\kappa+1)}\\left(\\frac{\\lambda}{\\lambda+\\kappa}\\right)^x \\left(\\frac{\\kappa}{\\lambda+\\kappa}\\right)^\\kappa"),
                                                  c("F(x|\\lambda,\\kappa) = \\begin{cases}
I_{\\frac{\\kappa}{\\kappa+\\lambda}}(\\kappa,1+\\lfloor x \\rfloor), & x\\geq 0 \\\\
0, & \\text{Otherwise}
\\end{cases}",
                                                    "\\text{where } I_w(u,v) \\text{ is the regularised incomplete beta function: }",
                                                    "I_w(u,v) = \\frac{B(w; u, v)}{B(u,v)}",
                                                    "\\text{where } B(w; u,v)=\\int_{0}^{w}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the incomplete beta function and } B(u,v)=\\int_{0}^{1}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the complete beta function}")),
           BetaBinomial=fLatexHelper_discrete(c("\\mathrm{E}(X) = \\frac{n\\alpha}{\\alpha+\\beta}",
                                                "var(X) = \\frac{n\\alpha\\beta(\\alpha+\\beta+n)}{(\\alpha+\\beta)^2(\\alpha+\\beta+1)}"),
                                              c("f(x|n,\\alpha,\\beta) = \\binom{n}{x}\\frac{B(x+\\alpha,n-x+\\beta)}{B(\\alpha,\\beta)}",
                                                "\\text{where } B(u,v)=\\int_{0}^{1}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is the (complete) beta function }"),
                                              c("F(x|n,\\alpha,\\beta) = \\begin{cases}
0, & x<0 \\\\
\\binom{n}{x}\\frac{B(x+\\alpha,n-x+\\beta)}{B(\\alpha,\\beta)} {}_{3}F_2(1,-x,n-x+\\beta;n-x-1,1-x-\\alpha;1), & 0\\leq x \\leq n \\\\
1, & x>n
\\end{cases}",
                                                "\\text{where } {}_{3}F_2(a,b,x) \\text{ is the generalised hypergeometric function}"))
    )
  }else if(input$distType=='Multivariate'){
    switch(input$dist2,
           MultivariateNormal=fLatexHelper(c("\\mathrm{E}(X) = \\mu",
                                             "var(X) = \\Sigma"),
                                           c("f(x|\\mu,\\Sigma) = \\frac{1}{(2\\pi)^{d/2}|\\Sigma|^{1/2}} \\text{exp}\\left(-\\frac{1}{2}(x-\\mu)'\\Sigma^{-1}(x-\\mu)\\right)")),
           MultivariateT=fLatexHelper(c("\\mathrm{E}(X) = \\begin{cases}
\\mu, & \\nu>1 \\\\
\\text{undefined}, & \\text{otherwise}
\\end{cases}",
                                        "var(X) = \\begin{cases}
\\frac{\\nu}{\\nu-2}\\Sigma, & \\nu>2 \\\\
\\text{undefined}, & \\text{otherwise}
\\end{cases}"),
                                      c("f(x|\\mu,\\Sigma, \\nu) = \\frac{\\Gamma((\\nu+d)/2)}{\\Gamma(\\nu/2)\\nu^{d/2}\\pi^{d/2}|\\Sigma|^{1/2}}\\left[1+\\frac{1}{\\nu}(x-\\mu)'\\Sigma^{-1}(x-\\mu)\\right]^{-(\\nu+d)/2}",
                                        "\\text{where }\\Gamma(w)=\\int_{0}^{\\infty}t^{w-1}e^{-t}\\mathrm{d}t \\text{ is the gamma function}")),
           Multinomial=fLatexHelper_discrete(c("\\mathrm{E}(X_i) = n p_i \\text{, }\\forall i",
                                               "var(X_i) = n p_i (1-p_i) \\text{, }\\forall i",
                                               "cov(X_i,X_j) = -n p_i p_j \\text{, }\\forall i\\neq j"),
                                             c("f(x_1,x_2,...,x_d|n,p_1,p_2,...,p_d) = \\frac{n!}{x_1 ! x_2 ! ... x_d !} p_1^{x_1} p_2^{x_2}...p_d^{x_d} ")),
           Wishart=fLatexHelper(c("\\mathrm{E}(X) = \\nu \\Psi",
                                  "var(X_{i,j}) = \\nu(\\Psi_{i,j}^2+\\Psi_{i,i}\\Psi_{j,j})"),
                                c("f(X|\\nu,\\Psi) = |X|^{(\\nu-d-1)/2} \\text{exp}(-\\text{tr}(\\Psi^{-1} X)/2)\\frac{1}{2^{\\nu d/2} |\\Psi|^{d/2} \\Gamma_d(\\nu/2)}",
                                  "\\text{where }  \\Gamma_p(a)=\\pi^{p(p-1)/4}\\prod_{j=1}^{p}\\Gamma(a+(1-j)/2) \\text{ is the multivariate gamma function}")),
           InverseWishart=fLatexHelper(c("\\mathrm{E}(X) = \\begin{cases}
\\frac{\\Psi}{\\nu-d-1}, & \\nu>d+1 \\\\
\\text{undefined}, & \\text{otherwise}
\\end{cases}",
                                         "var(X_{i,j}) = \\begin{cases}
\\frac{(\\nu-d+1)\\Psi_{i,j}^2 + (\\nu-d-1)\\Psi_{i,i}\\Psi_{i,j}}{(\\nu-d)(\\nu-d-1)^2(\\nu-d-3)}, & \\nu>d+3 \\\\
\\text{undefined}, & \\text{otherwise}
\\end{cases}"),
                                       c("f(X|\\nu,\\Psi) = |\\Psi|^{d/2} |X|^{-(\\nu+d+1)/2} \\text{exp}(-\\text{tr}(\\Psi X^{-1})/2)\\frac{1}{2^{\\nu d/2}  \\Gamma_d(\\nu/2)}",
                                         "\\text{where }  \\Gamma_p(a)=\\pi^{p(p-1)/4}\\prod_{j=1}^{p}\\Gamma(a+(1-j)/2) \\text{ is the multivariate gamma function}")),
           LKJ=fLatexHelper(c("\\mathrm{E}(X) = \\mathcal{I}_d",
                              "var(X_{i,j}) = \\frac{4\\left(\\frac{d}{2}+\\nu -1\\right)^2}{(d+2 \\nu -2)^2 (d+2 \\nu -1)}"),
                            c("f(X|\\nu,d) = \\left(2^{\\sum_{k=1}^{d-1}(2\\eta-2+d-k)(d-k)}\\prod_{k=1}^{d-1}\\left[B(\\eta+0.5(d-k-1),\\eta+0.5(d-k-1))\\right]^{d-k}\\right) |X|^{\\eta-1}",
                              "\\text{where }  B(u,v)=\\int_{0}^{1}t^{u-1}(1-t)^{v-1}\\mathrm{d}t \\text{ is beta function}")),
           Dirichlet=fLatexHelper(c("\\mathrm{E}(X_i) = \\frac{\\alpha_i}{\\sum_{k=1}^{d}\\alpha_k}",
                                    "var(X_i) = \\frac{\\alpha_i(\\alpha_0-\\alpha_i)}{\\alpha_0^2(\\alpha_0+1)}\\text{, where }\\alpha_0=\\sum_{i=1}^{d}\\alpha_i",
                                    "cov(X_i,X_j) = -\\frac{\\alpha_i \\alpha_j}{\\alpha_0^2(\\alpha_0+1)} \\text{, where }i\\neq j"),
                                  c("f(x_1,x_2,...,x_d|\\alpha_1,\\alpha_2,...,\\alpha_d) = \\frac{1}{B(\\alpha_1,\\alpha_2,...,\\alpha_d)}\\prod_{i=1}^{d}x_i^{\\alpha_i-1}",
                                    "\\text{where }B(\\alpha_1,\\alpha_2,...,\\alpha_d)=\\frac{\\prod_{i=1}^{d}\\Gamma(\\alpha_i)}{\\Gamma(\\sum_{i=1}^{d}\\alpha_i)}"))
    )
    }
}