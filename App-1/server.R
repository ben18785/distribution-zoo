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

dCustomBetaBinomial <- function(x, n, alpha, beta){
  return(choose(n, x) * beta(x + alpha, n - x + beta) / beta(alpha, beta))
}

pCustomBetaBinomial <- function(x, n, alpha, beta){
  return(sum(sapply(0:x, function(i) dCustomBetaBinomial(i, n, alpha, beta))))
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

fMakeFunction <- function(headingName, mainName, params, prefixparams=NULL,postfixparams=NULL){
  common_prose <- paste(sapply(params, function(x) eval(parse(text=x))), collapse=", ")
  prefix_prose <- paste(prefixparams,  collapse = ", ")
  postfix_prose <- paste(postfixparams,  collapse = ", ")
  if(!is.null(prefixparams))
    if(!is.null(postfixparams))
      words <- paste0(mainName, "(", prefix_prose, ", ", common_prose, ", ", postfix_prose, ")")
    else
      words <- paste0(mainName, "(", prefix_prose, ", ", common_prose, ")")
  else
    if(!is.null(postfixparams))
      words <- paste0(mainName, "(", common_prose, ", ", postfix_prose, ")")
    else
      words <- paste0(mainName, "(", common_prose, ")")
  return(list(h2(headingName), h3(words, style="color:#d95f02")))
}

fMakeDistribution <- function(headingNames, mainNames, lParams, lPrefixParams, lPostfixParams){
  a_len <- length(mainNames)
  lFunctions <- lapply(seq(1, a_len, 1),
                       function(i) fMakeFunction(headingNames[[i]],
                                                 mainNames[[i]],
                                                 lParams[[i]],
                                                 lPrefixParams[[i]],
                                                 lPostfixParams[[i]]))
  return(tagList(lFunctions))
}

lHeadings <- c("PDF", "Log PDF", "Random sample of size n")

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
             Bernoulli=input$probBer,
             Binomial=input$sizeBin * input$probBin,
             Poisson=input$lambdaPois,
             NegativeBinomial=input$meanNB,
             BetaBinomial=input$sizeBetaBin * input$shapeBetaBin1 / (input$shapeBetaBin1 + input$shapeBetaBin2),
             paste("mean=1,sd=1")
             )
    }
  })
  
  fCalculateVariance <- reactive({
    if(input$distType=='Continuous'){
      aMeanLogitNormal <- integrate(function(x) x * (1/(input$sigmaLogitN * sqrt(2 * pi))) * (1/(x * (1 - x))) * exp(- (log(x/(1-x)) - input$muLogitN)^2 / (2 * input$sigmaLogitN^2)),0,1)[[1]]
      a2LogitNormal <- integrate(function(x) x^2 * (1/(input$sigmaLogitN * sqrt(2 * pi))) * (1/(x * (1 - x))) * exp(- (log(x/(1-x)) - input$muLogitN)^2 / (2 * input$sigmaLogitN^2)),0,1)[[1]]
    aVar <- switch(input$dist,
             Normal=input$sigma^2,
             Uniform = (1/12) * (input$b - input$a)^2,
             LogNormal = exp(input$sdlog^2 - 1) * exp(2 * input$meanlog + input$sdlog^2),
             Exponential = 1/input$rate^2,
             Gamma= input$shape / input$rateGam^2,
             t = ifelse(input$nuT > 2,
                        input$nuT / (input$nuT - 2), NA),
             Beta=(input$alpha * input$beta) / ((input$alpha+input$beta)^2 * (input$alpha+input$beta + 1)),
             Cauchy=NA,
             HalfCauchy=NA,
             InverseGamma=ifelse(input$shapeIG > 2,
                                 input$scaleIG/((input$shapeIG-1)^2 * (input$shapeIG-2)),NA),
             InverseChiSquared=ifelse(input$dfIC > 4,
                                      2 / ((input$dfIC-2)^2 * (input$dfIC-4)),NA),
             LogitNormal=a2LogitNormal-aMeanLogitNormal^2,
              1)
    }else if (input$distType=='Discrete'){
    aVar <- switch(input$dist1,
                   Bernoulli=input$probBer * (1 - input$probBer),
                   Binomial=input$sizeBin * input$probBin * (1 - input$probBer),
                   Poisson=input$lambdaPois,
                   NegativeBinomial=input$meanNB + (input$meanNB^2 / input$dispersionNB),
                   BetaBinomial=input$sizeBetaBin * input$shapeBetaBin1 * input$shapeBetaBin2 *(input$shapeBetaBin1 + input$shapeBetaBin2 + input$sizeBetaBin) / ((input$shapeBetaBin1 + input$shapeBetaBin2)^2 * (input$shapeBetaBin1 + input$shapeBetaBin2 + 1))
                   )
    }
    
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
      aLen <- length(lScale)
      lPDF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
       
      qplot(lScale,lPDF,geom="path",
          xlab="X",ylab="probability density",
          xlim=c(lScale[1],lScale[aLen]))+
      geom_line(color='darkblue',size=1) +
        geom_vline(xintercept=ifelse(is.na(aMean),-10000,aMean),
                                    colour="orange",
                                    linetype = "longdash",
                                    size=1) +
        theme_classic() +
        ggtitle(paste0("mean = ", round(aMean, 2), ", var = ", round(aVar, 2))) +
        theme(plot.title = element_text(hjust = 0.5, size = 18),
              axis.text = element_text(size=14),
              axis.title = element_text(size=16)) +
        ylim(0, NA)
    } else if (input$distType=='Discrete') {
      aVar <- fCalculateVariance()
      lExtra <- fExtra1FunctionInputs()
      lScale <- fScale1()
      aLen <- length(lScale)
      lPMF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
      dataF <- data.frame(a=lScale,pmf=lPMF)
      ggplot(data=dataF, aes(x=factor(a), y=pmf)) +
        geom_bar(stat="identity", position=position_dodge(),fill="darkblue", colour="black") + xlab('X') +
        geom_vline(xintercept=(aMean + 1),
                     colour="orange",
                     linetype = "longdash",
                     size=1) +
        theme_classic() +
        ggtitle(paste0("mean = ", round(aMean, 2), ", var = ", round(aVar, 2))) +
        theme(plot.title = element_text(hjust = 0.5, size = 18),
              axis.text = element_text(size=14),
              axis.title = element_text(size=16)) +
        ylim(0, NA)
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
      ggplot(dataF, aes(x, y, z = pdf)) +
        geom_raster(aes(fill=pdf),interpolate = TRUE) +
        scale_fill_distiller("density", palette = "Spectral") +
        geom_contour(colour = "white") +
        xlim(c(-input$rangeN,input$rangeN)) +
        ylim(c(-input$rangeN,input$rangeN)) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5, size = 18),
              axis.text = element_text(size=14),
              axis.title = element_text(size=16),
              legend.title = element_text(size=16),
              legend.text = element_text(size=14))
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
      ggplot(dataF, aes(x, y, z = pdf)) +
        geom_raster(aes(fill=pdf),interpolate = TRUE) +
        scale_fill_distiller("density", palette = "Spectral") +
        geom_contour(colour = "white") +
        xlim(c(-input$rangeN,input$rangeN)) + ylim(c(-input$rangeN,input$rangeN)) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5, size = 18),
              axis.text = element_text(size=14),
              axis.title = element_text(size=16),
              legend.title = element_text(size=16),
              legend.text = element_text(size=14))
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
    aMean <- fCalculateMean()
    if (input$distType=='Continuous'){
      aVar <- fCalculateVariance()
    lScale <- fScale()
    aLen <- length(lScale)
    lExtra <- fExtraFunctionInputs()
    
    lPDF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
    qplot(lScale,lPDF,geom="path",
          xlab="X",ylab="cumulative probability",
          xlim=c(lScale[1],lScale[aLen]))+
      geom_line(color='darkblue',size=1) +
      geom_vline(xintercept=aMean,
                            colour="orange",
                            linetype = "longdash",
                            size=1) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.text = element_text(size=14),
            axis.title = element_text(size=16)) +
      ylim(0, NA) +
      ggtitle(paste0("mean = ", round(aMean, 2), ", var = ", round(aVar, 2)))

    } else if (input$distType=='Discrete'){
      aVar <- fCalculateVariance()
      lExtra <- fExtra1FunctionInputs()
      lScale <- fScale1()
      aLen <- length(lScale)
      lPMF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
      dataF <- data.frame(a=lScale,pmf=lPMF)
      ggplot(data=dataF, aes(x=factor(a), y=pmf)) +
        geom_bar(stat="identity", position=position_dodge(),fill="darkblue", colour="black") + xlab('X')+ 
        geom_vline(xintercept=(1+aMean),
                   colour="orange",
                   linetype = "longdash",
                   size=1) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5, size = 18),
              axis.text = element_text(size=14),
              axis.title = element_text(size=16)) +
        ylim(0, NA) +
        ylab('cumulate probability') +
        ggtitle(paste0("mean = ", round(aMean, 2), ", var = ", round(aVar, 2)))
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
                    h2("$$F(x|\\mu,\\sigma) = \\frac{1}{2}\\left[1+\\text{erf}\\left(\\frac{x-\\mu}{\\sigma\\sqrt{2}}\\right)\\right]$$"))
      }else if(input$dist=='Uniform'){
        withMathJax(h2("Moments"),h2("$$\\mathrm{E}(X) = \\frac{1}{2}(a + b)$$"),
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
        withMathJax(h2("Moments"),h2("$$\\mathrm{E}(X) = \\text{exp}(\\mu + \\frac{\\sigma^2}{2})$$"),
                    h2("$$var(X) = \\left[\\text{exp}(\\sigma^2) - 1\\right] \\text{exp}(2\\mu + \\sigma^2)$$"),
                    h2("PDF"),
                    h2("$$ \\frac{1}{x \\sigma \\sqrt{2 \\pi}} \\text{exp}\\left(-\\frac{(\\text{log } x - \\mu)^2}{2\\sigma^2}\\right)$$"),
                    h2("CDF"),
                    h2("$$\\frac{1}{2} + \\frac{1}{2} \\text{erf}\\left(\\frac{\\text{log } x - \\mu}{\\sqrt{2} \\sigma}\\right)$$"),
                    helpText(a("More information about the log-normal distribution.",
                               target="_blank",
                               href="https://en.wikipedia.org/wiki/Log-normal_distribution")))
      }
    }
  })
  
  output$latex <- renderUI({
    if (input$dist=='Normal'){
      tagList(h2("Moments"),
      h3("\\mathrm{E}(X) = \\mu"),
      h3("var(X) = \\sigma^2"),
      h2("PDF"),
      h3("f(x|\\mu,\\sigma) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}}\\text{exp}\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right)"),
      h2("CDF"),
      h3("F(x|\\mu,\\sigma) = \\frac{1}{2}\\left[1+\\text{erf}\\left(\\frac{x-\\mu}{\\sigma\\sqrt{2}}\\right)\\right]"))
    }
    })
  
  output$rcode <- renderUI({
    if(input$dist=="Normal"){
      fMakeDistribution(lHeadings,
                        c("dnorm", "dnorm", "rnorm"),
                        list(c(input$mu,input$sigma),c(input$mu,input$sigma),c(input$mu,input$sigma)),
                        list("x", "x", "n"), list(NULL, "log=TRUE", NULL))
    }
  })
  
  output$pythoncode <- renderUI({
    if(input$dist=="Normal"){
      tagList(h2("PDF"),
              h3("import scipy.stats"),
              h3(paste0("scipy.stats.norm.pdf(x, ",input$mu,", ",input$sigma,")")),
              h2("Log PDF"),
              h3(paste0("scipy.stats.norm.logpdf(x, ",input$mu,", ",input$sigma,")")),
              h2("Random sample of size n"),
              h3("Either:"),
              h3("import numpy as np"),
              h3("np.random.normal(",input$mu,", ",input$sigma))
    }
  })
  
  output$matlabcode <- renderUI({
    if(input$dist=="Normal"){
      tagList(h2("PDF"),
              h3("import scipy.stats"),
              h3(paste0("scipy.stats.norm.pdf(x, ",input$mu,", ",input$sigma,")")),
              h2("Log PDF"),
              h3("dnorm(x, mu, sigma, log=TRUE)"),
              h2("Random sample of size n"),
              h3("rnorm(n, mu, sigma)"))
    }
  })
  
  output$mathematicacode <- renderUI({
    if(input$dist=="Normal"){
      tagList(h2("PDF"),
              h3("import scipy.stats"),
              h3(paste0("scipy.stats.norm.pdf(x, ",input$mu,", ",input$sigma,")")),
              h2("Log PDF"),
              h3("dnorm(x, mu, sigma, log=TRUE)"),
              h2("Random sample of size n"),
              h3("rnorm(n, mu, sigma)"))
    }
  })
  
  output$juliacode <- renderUI({
    
    if(input$dist=="Normal"){
              tagList(h2("PDF"),
              h3("import scipy.stats"),
              h3(paste0("scipy.stats.norm.pdf(x, ",input$mu,", ",input$sigma,")")),
              h2("Log PDF"),
              h3("dnorm(x, mu, sigma, log=TRUE)"),
              h2("Random sample of size n"),
              h3("rnorm(n, mu, sigma)"))
    }
  })
  
  output$cpluspluscode <- renderUI({
    
    if(input$dist=="Normal"){
      tagList(h2("PDF"),
              h3("import scipy.stats"),
              h3(paste0("scipy.stats.norm.pdf(x, ",input$mu,", ",input$sigma,")")),
              h2("Log PDF"),
              h3("dnorm(x, mu, sigma, log=TRUE)"),
              h2("Random sample of size n"),
              h3("rnorm(n, mu, sigma)"))
    }
  })

  
  output$language <- renderUI({
     tagList(selectInput("language", "Language",
                 c("R"="R",
                   "Python"="Python",
                   "Matlab"="Matlab",
                   "Mathematica"="Mathematica",
                   "Julia"="Julia",
                   "C++"="Cplusplus"),
                 selected="R")
               )
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
                             tabPanel("code", 
                                      uiOutput("language"),
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