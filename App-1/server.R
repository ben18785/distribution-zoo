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
library(tidyverse)

source("functions.R")
source("formulae.R")
source("CDF.R")
source("PDF.R")
source("code_latex.R")
source("code_r.R")
source("code_python.R")
source("code_stan.R")
source("code_matlab.R")
source("code_mathematica.R")
source("code_julia.R")
source("code_cplusplus.R")
source("plotting.R")
source("example_uses.R")

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
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
           BetaBinomial=dCustomBetaBinomial,
           Binomial=dbinom,
           DiscreteUniform=dunifdisc,
           Poisson=dpois,
           NegativeBinomial=dnbinom,
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
             DiscreteUniform=punifdisc,
             Poisson=ppois,
             NegativeBinomial=pnbinom,
             BetaBinomial=pCustomBetaBinomial,
             dbern)
    } 
  })
  
  fScale <- reactive({
    lScale <- fScaleFull(input)
  })
  
  fScale1 <- reactive({
    lScale <- fScaleFull1(input)
  })
  
  fExtraFunctionInputs <- reactive({
    lExtra <- fExtraFunctionInputsFull(input)
    return(lExtra)
  })
  
  fExtra1FunctionInputs <- reactive({
    lExtra <- fExtra1FunctionInputsFull(input)
    return(lExtra)
  })
  
  fScaleMVR <- reactive({
    if(input$dist2=="MultivariateNormal")
      lSeq <- seq(-input$multivariatenormal_range, input$multivariatenormal_range,
                  2 * input$multivariatenormal_range / 100)
    else if(input$dist2=="MultivariateT")
      lSeq <- seq(-input$multivariatet_range, input$multivariatet_range,
                  2 * input$multivariatenormal_range / 100)
    return(lSeq)
  })
  
  fCalculateMean <- reactive({
    lExtra <- fCalculateMeanFull(input)
    
  })
  
  fCalculateVariance <- reactive({
    aVar <- fCalculateVarianceFull(input)
    return(aVar)
  })
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
    fPythoncode(input)
  })
  
  output$matlabcode <- renderUI({
    fMatlabcode(input)
  })
  
  output$mathematicacode <- renderUI({
    fMathematicacode(input)
  })
  
  output$juliacode <- renderUI({
    fJuliacode(input)
  })
  
  output$cpluspluscode <- renderUI({
    fCpluspluscode(input)
  })
  
  output$stancode <- renderUI({
    fStanCode(input)
  })
  
  output$BUGScode <- renderUI({
  })

  output$JAGScode <- renderUI({
  })
  
  output$ccode <- renderUI({
  })
  
  output$fortrancode <- renderUI({
  })
  
  output$example_uses <- renderUI({
    fExampleUses(input)
  })
  
  output$language <- renderUI({
     selectInput("language", "Language",
                 c("Mathematica"="Mathematica",
                   "Matlab"="Matlab",
                   "Python"="Python",
                   "R"="R",
                   "Stan"="Stan"
                  ),
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
    tagList(h4("Note: generates dynamic code for distributions with same properties as in plots"),
    if(is.null(input$language)){
      uiOutput("rcode")
    }else{
      switch(input$language,
             R=uiOutput("rcode"),
             Python=uiOutput("pythoncode"),
             Matlab=uiOutput("matlabcode"),
             Fortran=uiOutput("fortrancode"),
             Mathematica=uiOutput("mathematicacode"),
             Julia=uiOutput("juliacode"),
             C=uiOutput("ccode"),
             Cplusplus=uiOutput("cpluspluscode"),
             Stan=uiOutput("stancode"),
             BUGS=uiOutput("BUGScode"),
             JAGS=uiOutput("JAGScode"))
    })
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
                                      uiOutput("code")),
                             tabPanel("Practical tips",
                                      uiOutput("example_uses"))
        )
      }else{
        myTabs = tabsetPanel(type = "tabs", 
                             tabPanel("Plot of PDF", plotOutput("plot"),
                                      uiOutput("runningQuantities")),
                             tabPanel("Formulae", 
                                      uiOutput("formulae")),
                             tabPanel("LaTeX", 
                                      uiOutput("latex")),
                             tabPanel("Code", 
                                      uiOutput("language"),
                                      uiOutput("property"),
                                      uiOutput("code")),
                             tabPanel("Practical tips",
                                      uiOutput("example_uses"))
        )
      }
  })
  

  })
