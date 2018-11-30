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

fMarkdownMaker <- function(mu){
  code <- paste0("```{r}", "dnorm(x, ", "mu", ", 1) ```")
  fileConn<-file("test-markdown.md")
  writeLines(code, fileConn)
  close(fileConn)
}


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

fMakeFunction <- function(mainName, params, prefixparams=NULL,postfixparams=NULL, import=NULL, freeform=NULL, mathematica=FALSE, julia=FALSE){
  if(mathematica){
    a_forward_brace <- "["
    a_backward_brace <- "]"
  }else{
    a_forward_brace <- "("
    a_backward_brace <- ")"
  }
  if(is.null(freeform)){
    common_prose <- paste(sapply(params, function(x) eval(parse(text=x))), collapse=", ")
    prefix_prose <- paste(prefixparams,  collapse = ", ")
    postfix_prose <- paste(postfixparams,  collapse = ", ")
    if(!is.null(prefixparams))
      if(!is.null(postfixparams))
        words <- paste0(mainName, a_forward_brace, prefix_prose, ", ", common_prose, ", ", postfix_prose, a_backward_brace)
      else
        words <- paste0(mainName, a_forward_brace, prefix_prose, ", ", common_prose, a_backward_brace)
    else
      if(!is.null(postfixparams))
        if(!mathematica)
          if(!julia)
            words <- paste0(mainName, a_forward_brace, common_prose, ", ", postfix_prose, a_backward_brace)
          else
            words <- paste0(mainName, a_forward_brace, common_prose, "), ", postfix_prose, a_backward_brace)
        else
          words <- paste0(mainName, a_forward_brace, common_prose, "], ", postfix_prose, a_backward_brace)
      else
        words <- paste0(mainName, a_forward_brace, common_prose, a_backward_brace)
    if(is.null(import))
      lWords <- h3(words, style="color:#d95f02")
    else
      lWords <- list(h3(import, style="color:#d95f02"), h3(words, style="color:#d95f02"))
  }else{
    lWords <- h3(mainName, style="color:#d95f02")
  }
  return(lWords)
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
        ggtitle(paste0("mean = ", round(aMean, 2), ", sd = ", round(sqrt(aVar), 2))) +
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
        ggtitle(paste0("mean = ", round(aMean, 2), ", var = ", round(sqrt(aVar), 2))) +
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
  })
  
  output$latex <- renderUI({
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
    })
  
  output$rcode <- renderUI({
    if(input$dist=="Normal"){
      if(input$property=="pdf")
        fMakeFunction(mainName="dnorm",
                      params=c(input$mu,input$sigma),
                      prefixparams="x")
      else if(input$property=="log_pdf")
        fMakeFunction(mainName="dnorm",
                      params=c(input$mu,input$sigma),
                      prefixparams="x",
                      postfixparams="log=TRUE")
      else if(input$property=="random")
        fMakeFunction(mainName="rnorm",
                      params=c(input$mu,input$sigma),
                      prefixparams="n")
    }else if(input$dist=="Uniform"){
      if(input$property=="pdf")
        HTML(markdown::markdownToHTML(text="```{r}
                                      dnorm(0, 1, 2)", options=c("highlight_code")))
    }
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