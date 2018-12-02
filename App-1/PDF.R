
fPlotPDF <- function(input, aDist, aMean, aVar, lScale, lExtra){

  if (input$distType == 'Continuous'){

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
}