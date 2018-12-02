# Load necessary packages ----------
library(shiny)
library(LaplacesDemon)
library(ggplot2)
library(logitnorm)
library(actuar)
# library(VGAM)
library(reshape)
library(mvtnorm)
library(ggExtra)
library(grid)
library(gridExtra)
library(DirichletReg)
library(scatterplot3d)
library(markdown)

source("functions.R")
source("formulae.R")
source("CDF.R")
source("PDF.R")
source("latex.R")
source("r_code.R")

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  data <- reactive({
    
    dist<-if(input$distType=='Continuous'){
        switch(input$dist,
           Normal = dnorm,
           Uniform = dunif,
           LogNormal = dlnorm,
           Exponential = dexp,
           Gamma=dgamma,
           t = dst,
           Beta=dbeta,
           Cauchy=dcauchy,
           HalfCauchy=dCustomHalfCauchy,
           InverseGamma=dinvgamma,
           InverseChiSquared=dCustomInverseChiSquared,
           LogitNormal=dlogitnorm,
           dnorm)
    } else if (input$distType=='Discrete'){
        switch(input$dist1,
           Bernoulli=dbern,
           Binomial=dbinom,
           Poisson=dpois,
           NegativeBinomial=dnbinom,
           BetaBinomial=dCustomBetaBinomial,
           dbern)
    } else if (input$distType=='Multivariate'){
      switch(input$dist2,
             MultivariateNormal=dmvnorm,
             MultivarateT=dmvt,
             dmvnorm)
    }
  })
  
  dataCDF <- reactive({
    dist <- if (input$distType=='Continuous') {
                switch(input$dist,
                   Normal = pnorm,
                   Uniform = punif,
                   LogNormal = plnorm,
                   Exponential = pexp,
                   Gamma=pgamma,
                   t = pst,
                   Beta=pbeta,
                   Cauchy=pcauchy,
                   HalfCauchy=pCustomHalfCauchy,
                   InverseGamma=pinvgamma,
                   InverseChiSquared=pCustomInverseChiSquared,
                   LogitNormal=plogitnorm,
                   pnorm)
    } else if (input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=pbern,
             Binomial=pbinom,
             Poisson=ppois,
             NegativeBinomial=pnbinom,
             BetaBinomial=pCustomBetaBinomial,
             dbern)
    } 
  })
  
  fScale <- reactive({
    
    lAllReal <- c('Normal','Uniform','t','Cauchy')
    lUpperReal <- c('Exponential','LogNormal','Gamma','HalfCauchy',
                    'InverseGamma','InverseChiSquared')
    
    if (input$dist%in% lAllReal){
      lScale <- seq(-input$n,input$n,2*input$n/200)
    } else if (input$dist%in% lUpperReal){
      lScale <- seq(0,input$n,input$n/200)
    } else{
      lScale <- seq(0,1,1/200)
    }
                   
  })
  
  fScale1 <- reactive({
    lScale <- switch(input$dist1,
                     Bernoulli=c(0,1),
                     Binomial=seq(0,input$sizeBin,1),
                     Poisson=seq(0,input$rangePois,1),
                     NegativeBinomial=seq(0,input$rangeNB,1),
                     BetaBinomial=seq(0,input$sizeBetaBin,1)
                    )
  })
  
  fExtraFunctionInputs <- reactive({
    lExtra <- switch(input$dist,
                     Normal=paste("mean=",input$mu,",sd=",input$sigma),
                     Uniform = paste("min=",input$a,",max=",input$b),
                     LogNormal = paste("meanlog=",input$meanlog,",sdlog=",input$sdlog),
                     Exponential = paste("rate=",input$rate),
                     Gamma=paste("shape=",input$shape,",rate=",input$rateGam),
                     t = paste("mu=",input$muT,",sigma=",input$sigmaT,",nu=",input$nuT),
                     Beta=paste("shape1=",input$alpha,",shape2=",input$beta),
                     Cauchy=paste("location=",input$locationC,",scale=",input$scaleC),
                     HalfCauchy=paste("location=",input$locationHC,",scale=",input$scaleHC),
                     InverseGamma=paste("shape=",input$shapeIG,",scale=",input$scaleIG),
                     InverseChiSquared=paste("df=",input$dfIC),
                     LogitNormal=paste("mu=",input$muLogitN,",sigma=",input$sigmaLogitN),
                     paste("mean=1,sd=1"))
    return(lExtra)
  })
  
  fExtra1FunctionInputs <- reactive({
    lExtra <- switch(input$dist1,
                     Bernoulli=paste("prob=",input$probBer),
                     Binomial=paste("size=",input$sizeBin,",prob=",input$probBin),
                     Poisson=paste("lambda=",input$lambdaPois),
                     NegativeBinomial=paste("mu=",input$meanNB,",size=",input$dispersionNB),
                     BetaBinomial=paste("n=",input$sizeBetaBin,",alpha=",input$shapeBetaBin1,",beta=",input$shapeBetaBin2),
                     paste("mean=1,sd=1"))
    return(lExtra)
  })
  
  fScaleMVR <- reactive({
    lSeq <- seq(-input$rangeN,input$rangeN,2*input$rangeN/100)
    return(lSeq)
  })
  
  fCalculateMean <- reactive({
    lExtra <- fCalculateMeanFull(input)
    
  })
  
  fCalculateVariance <- reactive({
    aVar <- fCalculateVarianceFull(input)
    return(aVar)
  })
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    aDist <- data()
    aMean <- fCalculateMean()
    if (input$distType == 'Continuous'){
      aVar <- fCalculateVariance()
      lExtra <- fExtraFunctionInputs()
      lScale <- fScale()
    }else if (input$distType=='Discrete') {
      aVar <- fCalculateVariance()
      lExtra <- fExtra1FunctionInputs()
      lScale <- fScale1()
    }else if (input$dist2=='MultivariateNormal'){
      aVar <- -99
      lScale <- fScaleMVR()
    }else if (input$dist2=='MultivariateT'){
      aVar <- -99
      lScale <- fScaleMVR()
    }else{
      aVar <- -99
      lScale <- vector()
      lExtra <- vector()
    }
    fPlotPDF(input, aDist, aMean, aVar, lScale, lExtra)
  })
  
  output$plotCDF <- renderPlot({
    aDist <- dataCDF()
    aMean <- fCalculateMean()
    if (input$distType=='Continuous'){
      aVar <- fCalculateVariance()
      lScale <- fScale()
      lExtra <- fExtraFunctionInputs()
    }else if(input$distType=='Discrete'){
      lExtra <- fExtra1FunctionInputs()
      aVar <- fCalculateVariance()
      lScale <- fScale1()
    }else if (input$dist2=='MultivariateNormal'){
      lScale <- fScaleMVR()
      lExtra <- vector()
      aVar <- -99
    }else if (input$dist2=='MultivariateT'){
      lScale <- fScaleMVR()
      lExtra <- vector()
      aVar <- -99
    }
    fPlotCDF(input, aDist, aMean, aVar, lScale, lExtra)
  })
  
  output$formulae <- renderUI({
   fFormulae(input)
  })
  
  output$latex <- renderUI({
    fLatex(input)
    })
  
  output$rcode <- renderUI({
    fRcode(input)
  })
  
  output$pythoncode <- renderUI({
    if(input$dist=="Normal"){
      if(input$property=="pdf")
        fMakeFunction(mainName="scipy.stats.norm.pdf",
                      params=c(input$mu,input$sigma),
                      prefixparams="x")
      else if(input$property=="log_pdf")
        fMakeFunction(mainName="scipy.stats.norm.logpdf",
                      params=c(input$mu,input$sigma),
                      prefixparams="x")
      else if(input$property=="random")
        fMakeFunction(mainName="numpy.random.normal",
                      import="import numpy",
                      params=c(input$mu,input$sigma),
                      postfixparams="n")
    }else if(input$dist=="Uniform"){
      if(input$property=="pdf")
        HTML(markdown::markdownToHTML(text="```{python}
scipy.stats.norm.logpdf(1,2,3)", options=c("highlight_code")))
    }
  })
  
  output$matlabcode <- renderUI({
    if(input$dist=="Normal"){
      if(input$property=="pdf")
        fMakeFunction(mainName="normpdf",
                      params=c(input$mu,input$sigma),
                      prefixparams="x")
      else if(input$property=="log_pdf")
        fMakeFunction(mainName=paste0("-0.5 * log(2 * pi) - log(", eval(parse(text=input$sigma)),
                                      ") - (x - ",
                                      eval(parse(text=input$mu)), ")^2 / (2 * ",
                                      eval(parse(text=input$sigma)), "^2)"),
                      params=c(input$mu,input$sigma),
                      freeform=TRUE)
      else if(input$property=="random")
        fMakeFunction(mainName="normrnd",
                      params=c(input$mu,input$sigma),
                      postfixparams="[n, 1]")
    }
  })
  
  output$mathematicacode <- renderUI({
    if(input$dist=="Normal"){
      if(input$property=="pdf")
        fMakeFunction(mainName="PDF[NormalDistribution",
                      params=c(input$mu,input$sigma),
                      postfixparams="x",
                      mathematica=TRUE)
      else if(input$property=="log_pdf")
        fMakeFunction(mainName=paste0("-0.5 Log[2 Pi] - Log[", eval(parse(text=input$sigma)),
                                      "] - (x - ",
                                      eval(parse(text=input$mu)), ")^2 / (2 ",
                                      eval(parse(text=input$sigma)), "^2)"),
                      freeform=TRUE)
      else if(input$property=="random")
        fMakeFunction(mainName="RandomVariate[NormalDistribution",
                      params=c(input$mu,input$sigma),
                      postfixparams="n",
                      mathematica=TRUE)
    }
  })
  
  output$juliacode <- renderUI({
    
    if(input$dist=="Normal"){
      if(input$property=="pdf")
        short <- fMakeFunction(mainName="pdf",
                               params=c(input$mu,input$sigma),
                               postfixparams="x",
                               julia=TRUE)
      else if(input$property=="log_pdf")
        short <- fMakeFunction(mainName="logpdf",
                      params=c(input$mu,input$sigma),
                      postfixparams="x",
                      julia=TRUE)
      else if(input$property=="random")
        short <- fMakeFunction(mainName="rand(Normal",
                               params=c(input$mu,input$sigma),
                               postfixparams="n",
                               julia=TRUE)
    }
      
    tagList(short,
            h3("Note that code assumes that 'Compat' and 'Distributions' packages are installed by typing:"),
            h3("Pkg.add(\"Compat\")", style="color:#d95f02"),
            h3("Pkg.add(\"Distributions\")", style="color:#d95f02"),
            h3("at Julia command line."))
  })
  
  output$cpluspluscode <- renderUI({
    
    if(input$dist=="Normal"){
      if(input$property=="pdf")
        tagList(p(HTML(paste("#include &lt;math&gt",
                             sep="<br/>")), style="color:#d95f02"),
                p(HTML(paste("double normal_pdf(int n, double mu, double sigma)",
                              "{",
                             paste0("return 1.0 / (std::sqrt(2.0 * M_PI) * ",
                                    eval(parse(text=input$sigma)),
                                    ") * exp(-pow(x - ",
                                    eval(parse(text=input$mu)),
                                    ", 2.0) / (2 * pow(",
                                    input$sigma, ", 2.0)));"),
                             "}",
                             sep="<br/>")), style="color:#d95f02"))
      else if(input$property=="log_pdf")
        tagList(p(HTML(paste("#include &lt;math&gt",
                             sep="<br/>")), style="color:#d95f02"),
                p(HTML(paste("double normal_lpdf(int n, double mu, double sigma)",
                             "{",
                             paste0("return -0.5 * log(2 * M_PI) - log(",
                                    eval(parse(text=input$sigma)),
                                    ") - pow(x - ",
                                    eval(parse(text=input$mu)),
                                    ", 2.0) / (2 * pow(",
                                    input$sigma, ", 2.0)));"),
                             "}",
                             sep="<br/>")), style="color:#d95f02"))
      else if(input$property=="random")
        tagList(p(HTML(paste("#include &lt;random&gt;",
                     "#include &lt;vector&gt",
                     "#include &lt;math&gt",
                     "#include &lt;chrono&gt;",
                     sep="<br/>")), style="color:#d95f02"),
        p(HTML("// unseeded"), style="color:#d95f02"),
        p(HTML(paste0("std::vector&lt;double&gt; normal_rng(int n, double mu, double sigma)", "<br/>",
                      "{", "<br/>",
                      "unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();", "<br/>",
                      "std::normal_distribution&lt;double&gt; distribution(",
                      eval(parse(text=input$mu)), ", ", eval(parse(text=input$sigma)), ");", "<br/>",
                      "std::vector&lt;double&gt; samples(n);", "<br/>",
                      "for(int i = 0; i < n; i++)", "<br/>",
                      "&emsp;", "samples[i] = distribution(generator);", "<br/>",
                      "return samples;", "<br/>",
                      "}")), style="color:#d95f02"),
        p(HTML("// seeded"), style="color:#d95f02"),
        p(HTML(paste0("std::vector&lt;double&gt; normal_rng(int n, double mu, double sigma, unsigned seed)", "<br/>",
              "{", "<br/>",
              "std::normal_distribution&lt;double&gt; distribution(",
              eval(parse(text=input$mu)), ", ", eval(parse(text=input$sigma)), ");", "<br/>",
              "std::vector&lt;double&gt; samples(n);", "<br/>",
              "for(int i = 0; i < n; i++)", "<br/>",
              "&emsp;", "samples[i] = distribution(generator);", "<br/>",
              "return samples;", "<br/>",
              "}")), style="color:#d95f02"))
    }
  })

  
  output$language <- renderUI({
     selectInput("language", "Language",
                 c("R"="R",
                  "Python"="Python",
                  "Matlab"="Matlab",
                  "Mathematica"="Mathematica",
                  "Julia"="Julia",
                  "C++"="Cplusplus"),
                 selected="R")
  })
  
  output$property <- renderUI({
    selectInput("property", "Property",
                c("PDF"="pdf",
                "Log PDF"="log_pdf",
                "random sample of size n"="random"),
                selected="pdf")
  })
  
  output$code <- renderUI({
    if(is.null(input$language)){
      uiOutput("rcode")
    }else{
      switch(input$language,
             R=uiOutput("rcode"),
             Python=uiOutput("pythoncode"),
             Matlab=uiOutput("matlabcode"),
             Mathematica=uiOutput("mathematicacode"),
             Julia=uiOutput("juliacode"),
             Cplusplus=uiOutput("cpluspluscode"))
    }
  })
  
  output$mytabs = renderUI({
      if(input$distType!='Multivariate'){
        myTabs = tabsetPanel(type = "tabs", 
                             tabPanel("Plot of PDF", plotOutput("plot"),
                                      uiOutput("runningQuantities")), 
                             tabPanel("Plot of CDF", plotOutput("plotCDF"),
                                      uiOutput("runningQuantities1")),
                             tabPanel("Formulae", 
                                      uiOutput("formulae")),
                             tabPanel("LaTeX", 
                                      uiOutput("latex")),
                             tabPanel("Code", 
                                      uiOutput("language"),
                                      uiOutput("property"),
                                      uiOutput("code"))
        )
      }else{
        myTabs = tabsetPanel(type = "tabs", 
                             tabPanel("Plot of PDF", plotOutput("plot"),
                                      uiOutput("runningQuantities")),
                             tabPanel("Formulae", 
                                      uiOutput("formulae"))
        )
      }
  })
  

  })