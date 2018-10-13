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

dCustomInverseChiSquared <- function(x,df){
  return(ifelse(x>0,dinvchisq(x,df),0))
}

pCustomInverseChiSquared <- function(x,df){
  return(ifelse(x>0,integrate(function(z) dinvchisq(z,df),0,x),0))
}

dCustomHalfCauchy <- function(x,location,scale){
  return(dcauchy(x,location,scale))
}

pCustomHalfCauchy <- function(x,location,scale){
  aInt <- integrate(function(x) dcauchy(x,location,scale),0,Inf)
  return((1/aInt)*pcauchy(x,location,scale))
}

rcoronion<-function(d,eta=1){ 
  d<-as.integer(d)
  if(d<=0 || !is.integer(d))
  { stop("The dimension 'd' should be a positive integer!\n") }
  if(eta<=0)
  { stop("'eta' should be positive!\n") }
  
  #handling of d=1 and d=2
  if(d==1) 
  { rr<-matrix(1,1,1); return(rr) }
  if(d==2) 
  { rho<-2*rbeta(1,eta,eta)-1
  rr<-matrix(c(1,rho,rho,1),2,2); return(rr) 
  }
  rr<-matrix(0,d,d)
  beta<-eta+(d-2)/2
  # step 1
  r12<-2*rbeta(1,beta,beta)-1
  rr<-matrix(c(1,r12,r12,1),2,2)
  # iterative steps
  for(m in 2:(d-1))
  { beta<-beta-0.5
  y<-rbeta(1,m/2,beta)
  z<-rnorm(m,0,1)
  znorm<-sqrt(sum(z^2))
  # random on surface of unit sphere
  z<-z/znorm
  w=sqrt(y)*z
  # can spped up by programming incremental Cholesky?
  rhalf<-chol(rr)
  qq<-w%*%rhalf
  rr<-cbind(rr,t(qq))
  rr<-rbind(rr,c(qq,1))
  }
  # return rr
  rr
}

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
           BetaBinomial=dbetabinom.ab,
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
             BetaBinomial=pbetabinom.ab,
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
                     BetaBinomial=paste("size=",input$sizeBetaBin,",shape1=",input$shapeBetaBin1,",shape2=",input$shapeBetaBin2),
                     paste("mean=1,sd=1"))
    return(lExtra)
  })
  
  fScaleMVR <- reactive({
    lSeq <- seq(-input$rangeN,input$rangeN,2*input$rangeN/100)
    return(lSeq)
  })
  
  fCalculateMean <- reactive({
    
    lExtra <- if (input$distType=='Continuous'){
        switch(input$dist,
           Normal=input$mu,
           Uniform = 0.5 * (input$a + input$b),
           LogNormal = exp(input$meanlog+0.5*input$sdlog^2),
           Exponential = 1/input$rate,
           Gamma= input$shape / input$rateGam,
           t = ifelse(input$nuT>1,input$muT,NA),
           Beta=input$alpha/(input$alpha+input$beta),
           Cauchy=NA,
           HalfCauchy=NA,
           InverseGamma=ifelse(input$shapeIG>1,input$scaleIG/(input$shapeIG-1),NA),
           InverseChiSquared=ifelse(input$dfIC>2,1/(input$dfIC-2),NA),
           LogitNormal=integrate(function(x) x * (1/(input$sigmaLogitN * sqrt(2 * pi))) * (1/(x * (1 - x))) * exp(- (log(x/(1-x)) - input$muLogitN)^2 / (2 * input$sigmaLogitN^2)),0,1)[[1]],
           1)
    } else if (input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=1+input$probBer,
             Binomial=1 + input$sizeBin * input$probBin,
             Poisson=1 + input$lambdaPois,
             NegativeBinomial=1 + input$meanNB,
             BetaBinomial=1 + input$sizeBetaBin * input$shapeBetaBin1 / (input$shapeBetaBin1 + input$shapeBetaBin2),
             paste("mean=1,sd=1")
             )
    }
  })
  
  fCalculateVariance <- reactive({
    aVar <- switch(input$dist,
             Normal=input$sigma,
              1)
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
      lExtra <- fExtraFunctionInputs()
      lScale <- fScale()
      aLen <- length(lScale)
      lPDF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
       
      qplot(lScale,lPDF,geom="path",
          xlab="X",ylab="probability density",
          xlim=c(lScale[1],lScale[aLen]))+
      geom_line(color='darkblue',size=2) + geom_vline(xintercept=ifelse(is.na(aMean),-10000,aMean),
                                                      colour="orange",
                                                      linetype = "longdash",
                                                      size=1)
    } else if (input$distType=='Discrete') {
      lExtra <- fExtra1FunctionInputs()
      lScale <- fScale1()
      aLen <- length(lScale)
      lPMF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
      dataF <- data.frame(a=lScale,pmf=lPMF)
      ggplot(data=dataF, aes(x=factor(a), y=pmf)) +
        geom_bar(stat="identity", position=position_dodge(),fill="darkblue", colour="black") + xlab('X') +
        geom_vline(xintercept=aMean,
                     colour="orange",
                     linetype = "longdash",
                     size=1)
    } else if (input$dist2=='MultivariateNormal'){
      lMean <- c(input$meanXN,input$meanYN)
      lSigma <- matrix(c(input$sigmaXN^2,input$sigmaXN * input$sigmaYN * input$rhoxyN,
                         input$sigmaXN * input$sigmaYN * input$rhoxyN, input$sigmaYN^2),
                       nrow = 2,ncol = 2)
      lScale <- fScaleMVR()
      aLen <- length(lScale)
      x.points <- lScale
      y.points <- x.points
      z <- matrix(0,nrow=aLen,ncol=aLen)
      for (i in 1:aLen) {
        for (j in 1:aLen) {
          z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),
                            mean=lMean,sigma=lSigma)
        }
      }
      dataF <- melt(z)
      dataF$X1<-rep(x.points,aLen)
      dataF$X2 <- unlist(lapply(x.points,function(x) rep(x,aLen)))
      names(dataF) <- c("x", "y", "pdf")
      ggplot(dataF, aes(x, y, z = pdf))+ geom_raster(aes(fill=pdf),interpolate = TRUE)+
        geom_contour(colour = "white")+
        xlim(c(-input$rangeN,input$rangeN)) + ylim(c(-input$rangeN,input$rangeN)) 
    }else if (input$dist2=='MultivariateT'){
      lMean <- c(input$meanXT,input$meanYT)
      lSigma <- matrix(c(input$sigmaXT^2,input$sigmaXT * input$sigmaYT * input$rhoxyT,
                         input$sigmaXT * input$sigmaYT * input$rhoxyT, input$sigmaYT^2),
                       nrow = 2,ncol = 2)
      lScale <- fScaleMVR()
      aLen <- length(lScale)
      x.points <- lScale
      y.points <- x.points
      z <- matrix(0,nrow=aLen,ncol=aLen)
      for (i in 1:aLen) {
        for (j in 1:aLen) {
          z[i,j] <- mvtnorm::dmvt(c(x.points[i],y.points[j]),
                            delta=lMean,sigma=lSigma,df=input$dfMVT,log=FALSE)
        }
      }
      dataF <- melt(z)
      dataF$X1<-rep(x.points,aLen)
      dataF$X2 <- unlist(lapply(x.points,function(x) rep(x,aLen)))
      names(dataF) <- c("x", "y", "pdf")
      ggplot(dataF, aes(x, y, z = pdf))+ geom_raster(aes(fill=pdf),interpolate = TRUE)+
        geom_contour(colour = "white")+
        xlim(c(-input$rangeN,input$rangeN)) + ylim(c(-input$rangeN,input$rangeN)) 
    } else if (input$dist2=='Wishart'){
      lList<-rWishart(input$sampleSizeWish,input$dfWish,diag(input$dimensionWish))
      lOffD <- vector(length=input$sampleSizeWish)
      lOffD1 <- vector(length=input$sampleSizeWish)
      lDiag <- vector(length=input$sampleSizeWish)
      for (i in 1:input$sampleSizeWish){
        aMatrix <- lList[,,i]
        lOffD[i] <- aMatrix[1,2]/sqrt(aMatrix[1,1]*aMatrix[2,2])
        lOffD1[i] <- aMatrix[2,3]/sqrt(aMatrix[2,2]*aMatrix[3,3])
        lDiag[i] <- aMatrix[1,1]
      }
      h1 <- qplot(log(lDiag),fill=I("blue")) + xlab(expression(paste("log(",sigma[1]^2,")")))
      aDataF <- data.frame(x=lOffD,y=lOffD1)
      m <- ggplot(aDataF, aes(x = x, y = y)) +
        geom_point() + xlab(expression(rho[12])) + ylab(expression(rho[23]))
      p <- m + geom_density2d(size=1)
      p1<-ggExtra::ggMarginal(p,type = "histogram",fill=I("blue"))
      grid.arrange(h1,p1,ncol = 2)
      
    }else if (input$dist2=='InverseWishart'){
      lList<-rWishart(input$sampleSizeInvWish,input$dfInvWish,diag(input$dimensionInWish))
      lOffD <- vector(length=input$sampleSizeInvWish)
      lOffD1 <- vector(length=input$sampleSizeInvWish)
      lDiag <- vector(length=input$sampleSizeInvWish)
      for (i in 1:input$sampleSizeInvWish){
        aMatrix <- solve(lList[,,i])
        lOffD[i] <- aMatrix[1,2]/sqrt(aMatrix[1,1]*aMatrix[2,2])
        lOffD1[i] <- aMatrix[2,3]/sqrt(aMatrix[2,2]*aMatrix[3,3])
        lDiag[i] <- aMatrix[1,1]
      }
      h1 <- qplot(log(lDiag),fill=I("blue")) + xlab(expression(paste("log(",sigma[1]^2,")")))
      aDataF <- data.frame(x=lOffD,y=lOffD1)
      m <- ggplot(aDataF, aes(x = x, y = y)) +
        geom_point() + xlab(expression(rho[12])) + ylab(expression(rho[23]))
      p <- m + geom_density2d(size=1)
      p1<-ggExtra::ggMarginal(p,type = "histogram",fill=I("blue"))
      grid.arrange(h1,p1,ncol = 2)
    } else if (input$dist2=='Dirichlet'){
      if(input$dimensionsDirichlet==2){
             lSamples <- rdirichlet(input$sampleSizeDirichlet, c(input$alphaDirichlet1,input$alphaDirichlet2))
             qplot(lSamples[,1],fill=I("blue")) + xlab(expression(p[1]))
             
      } else if (input$dimensionsDirichlet==3){     
             plot(DR_data(rdirichlet(input$sampleSizeDirichlet, c(input$alphaDirichlet1,input$alphaDirichlet2,input$alphaDirichlet3))), a2d = list(colored = TRUE),dim.labels=c(expression(p[1]),expression(p[2]),expression(p[3])))
      } else {     
             plot(DR_data(rdirichlet(input$sampleSizeDirichlet, c(input$alphaDirichlet1,input$alphaDirichlet2,input$alphaDirichlet3,input$alphaDirichlet4))), a3d = list(colored = TRUE,rgl=FALSE),dim.labels=c(expression(p[1]),expression(p[2]),expression(p[3]),expression(p[4])))
      }
      
    } else if(input$dist2=='Multinomial'){
      X <- t(as.matrix(expand.grid(0:input$sizeMultinomial, 0:input$sizeMultinomial)))
      X <- X[ , colSums(X) <= input$sizeMultinomial]; X <- rbind(X, input$sizeMultinomial - colSums(X))
      Z <- round(apply(X, 2, function(x) dmultinom(x, prob = c(input$probMultinomial1,input$probMultinomial2,input$probMultinomial3))), 3)
      A <- data.frame(x = X[1, ], y = X[2, ], probability = Z)
      scatterplot3d(A, type = "h", lwd = 3,highlight.3d=TRUE, 
                    box = FALSE,angle=input$angleMultinomial)
    
    } else if(input$dist2=='LKJ'){
      lSamples <- lapply(seq(1,input$sampleSizeLKJ,1), function(x) rcoronion(input$dimensionLKJ,
                                                               input$etaLKJ))
      lSample12 <- unlist(lapply(lSamples,function(x) x[1,2]))
      lSample23 <- unlist(lapply(lSamples,function(x) x[2,3]))
      aDataF <- data.frame(x=lSample12,y=lSample23)
      m <- ggplot(aDataF, aes(x = x, y = y)) +
        geom_point() + xlab(expression(rho[12])) + ylab(expression(rho[23]))
      p <- m + geom_density2d(size=1)
      p1<-ggExtra::ggMarginal(p,type = "histogram",fill=I("blue"))
      p1
    }
  })
  
  output$plotCDF <- renderPlot({
    
    aDist <- dataCDF()
    
    if (input$distType=='Continuous'){
    lScale <- fScale()
    aLen <- length(lScale)
    lExtra <- fExtraFunctionInputs()
    # print(lExtra)
    aMean <- fCalculateMean()
    
    lPDF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
    qplot(lScale,lPDF,geom="path",
          xlab="X",ylab="pdf",
          xlim=c(lScale[1],lScale[aLen]))+
      geom_line(color='darkblue',size=2) + geom_vline(xintercept=aMean,
                                                      colour="orange",
                                                      linetype = "longdash",
                                                      size=1)
    } else if (input$distType=='Discrete'){
      lExtra <- fExtra1FunctionInputs()
      aMean <- fCalculateMean()
      lScale <- fScale1()
      aLen <- length(lScale)
      lPMF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
      dataF <- data.frame(a=lScale,pmf=lPMF)
      ggplot(data=dataF, aes(x=factor(a), y=pmf)) +
        geom_bar(stat="identity", position=position_dodge(),fill="darkblue", colour="black") + xlab('X')+ 
        geom_vline(xintercept=aMean,
                   colour="orange",
                   linetype = "longdash",
                   size=1)
    }else if (input$dist2=='MultivariateNormal'){
      lMean <- c(input$meanXN,input$meanYN)
      lSigma <- matrix(c(input$sigmaXN^2,input$sigmaXN * input$sigmaYN * input$rhoxyN,
                         input$sigmaXN * input$sigmaYN * input$rhoxyN, input$sigmaYN^2),
                       nrow = 2,ncol = 2)
      lScale <- fScaleMVR()
      aLen <- length(lScale)
      x.points <- lScale
      y.points <- x.points
      z <- matrix(0,nrow=aLen,ncol=aLen)
      for (i in 1:aLen) {
        for (j in 1:aLen) {
          z[i,j] <- pmvnorm(lower=c(-Inf,-Inf),upper=c(x.points[i],y.points[j]),
                            mean=lMean,sigma=lSigma)
        }
      }
      dataF <- melt(z)
      dataF$X1<-rep(x.points,aLen)
      dataF$X2 <- unlist(lapply(x.points,function(x) rep(x,aLen)))
      names(dataF) <- c("x", "y", "cdf")
      ggplot(dataF, aes(x, y, z = cdf))+ geom_tile(aes(fill = cdf)) + stat_contour()+
        xlim(c(-input$rangeN,input$rangeN)) + ylim(c(-input$rangeN,input$rangeN)) 
    }else if (input$dist2=='MultivariateT'){
      lMean <- c(input$meanXT,input$meanYT)
      lSigma <- matrix(c(input$sigmaXT^2,input$sigmaXT * input$sigmaYT * input$rhoxyT,
                         input$sigmaXT * input$sigmaYT * input$rhoxyT, input$sigmaYT^2),
                       nrow = 2,ncol = 2)
      lScale <- fScaleMVR()
      aLen <- length(lScale)
      x.points <- lScale
      y.points <- x.points
      z <- matrix(0,nrow=aLen,ncol=aLen)
      for (i in 1:aLen) {
        for (j in 1:aLen) {
          z[i,j] <- mvtnorm::pmvt(lower=c(-Inf,-Inf),upper=c(x.points[i],y.points[j]),
                                  delta=lMean,sigma=lSigma,df=input$dfMVT)
        }
      }
      dataF <- melt(z)
      dataF$X1<-rep(x.points,aLen)
      dataF$X2 <- unlist(lapply(x.points,function(x) rep(x,aLen)))
      names(dataF) <- c("x", "y", "cdf")
      ggplot(dataF, aes(x, y, z = cdf))+ geom_tile(aes(fill = cdf)) + stat_contour()+
        xlim(c(-input$rangeN,input$rangeN)) + ylim(c(-input$rangeN,input$rangeN)) 
    }
  })
  
  output$formulae <- renderUI({
    if (input$distType=='Continuous'){
      if (input$dist=='Normal'){
        
        withMathJax(h2("Moments"),h2("$$\\mathrm{E}(X) = \\mu$$"),
                    h2("$$var(X) = \\sigma^2$$"),
                    h2("PDF"),
                    h2("$$f(x|\\mu,\\sigma) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}} 
                             \\text{exp}\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right)$$"),
                    h2("CDF"),
                    h2("$$F(x|\\mu,\\sigma) = \\frac{1}{2}\\left[1+\\text{erf}\\left(\\frac{x-\\mu}{\\sigma\\sqrt{2}}\\right)\\right]$$"),
                    helpText(a("More information about the normal distribution.",
                               target="_blank",
                               href="https://en.wikipedia.org/wiki/Normal_distribution")))
      }else if(input$dist=='Uniform'){
        withMathJax(h2("Moments"),h2("$$\\mathrm{E}(\\theta) = \\frac{1}{2}(a + b)$$"),
                    h2("$$var(\\theta) = \\frac{1}{12}(b - a)$$"),
                    h2("PDF"),
                    h2("$$f(\\theta|a,b) = \\frac{1}{b - a} \\text{, where } a < b$$"),
                    h2("CDF"),
                    h2("$$F(\\theta|a,b) = $$"),
                    helpText(a("More information about the normal distribution.",
                               target="_blank",
                               href="https://en.wikipedia.org/wiki/Normal_distribution")))
      }
    }
  })
  
  output$runningQuantities <- renderUI({
    aMean <- fCalculateMean()
    aVar <- fCalculateVariance()
    if (input$distType=='Continuous'){
      withMathJax(h2(paste("mean = ",aMean),align="center"),
      h2(paste("variance = ",aVar),align="center") )
    }
  })
  
  output$runningQuantities1 <- renderUI({
    aMean <- fCalculateMean()
    aVar <- fCalculateVariance()
    if (input$distType=='Continuous'){
      withMathJax(h2(paste("median = ",aMean),align="center"))
    }
  })

  })