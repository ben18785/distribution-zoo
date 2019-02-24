
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
      ggtitle(paste0("mean (orange line) = ", round(aMean, 2), ", sd = ", round(sqrt(aVar), 2))) +
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.text = element_text(size=14),
            axis.title = element_text(size=16)) +
      ylim(0, NA)
  } else if (input$distType=='Discrete') {
    aLen <- length(lScale)
    lPMF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
    dataF <- data.frame(a=lScale,pmf=lPMF)
    ggplot(data=dataF, aes(x=a, y=pmf)) +
      geom_col(fill="darkblue", colour="black") + xlab('X') +
      geom_vline(xintercept=(aMean),
                 colour="orange",
                 linetype = "longdash",
                 size=1) +
      theme_classic() +
      ggtitle(paste0("mean (orange line) = ", round(aMean, 2), ", sd = ", round(sqrt(aVar), 2))) +
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.text = element_text(size=14),
            axis.title = element_text(size=16)) +
      ylim(0, NA)
  } else if (input$dist2=='MultivariateNormal'){
    lMean <- c(input$multivariatenormal_mux,input$multivariatenormal_muy)
    lSigma <- matrix(c(input$multivariatenormal_sigmax^2,input$multivariatenormal_sigmax * input$multivariatenormal_sigmay * input$multivariatenormal_rho,
                       input$multivariatenormal_sigmax * input$multivariatenormal_sigmay * input$multivariatenormal_rho, input$multivariatenormal_sigmay^2),
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
      xlim(c(-input$multivariatenormal_range,input$multivariatenormal_range)) +
      ylim(c(-input$multivariatenormal_range,input$multivariatenormal_range)) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.text = element_text(size=14),
            axis.title = element_text(size=16),
            legend.title = element_text(size=16),
            legend.text = element_text(size=14))
  }else if (input$dist2=='MultivariateT'){
    lMean <- c(input$multivariatet_mux,input$multivariatet_muy)
    lSigma <- matrix(c(input$multivariatet_sigmax^2,input$multivariatet_sigmax * input$multivariatet_sigmay * input$multivariatet_rho,
                       input$multivariatet_sigmax * input$multivariatet_sigmay * input$multivariatet_rho, input$multivariatet_sigmay^2),
                     nrow = 2,ncol = 2)
    aLen <- length(lScale)
    x.points <- lScale
    y.points <- x.points
    z <- matrix(0,nrow=aLen,ncol=aLen)
    for (i in 1:aLen) {
      for (j in 1:aLen) {
        z[i,j] <- mvtnorm::dmvt(c(x.points[i],y.points[j]),
                                delta=lMean,sigma=lSigma,df=input$multivariatet_df,log=FALSE)
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
      xlim(c(-input$multivariatet_range,input$multivariatet_range)) +
      ylim(c(-input$multivariatet_range,input$multivariatet_range)) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.text = element_text(size=14),
            axis.title = element_text(size=16),
            legend.title = element_text(size=16),
            legend.text = element_text(size=14))
  } else if (input$dist2=='Wishart'){
    lList<-rWishart(input$wishart_samplesize,input$wishart_df,diag(input$wishart_dimension))
    lOffD <- vector(length=input$wishart_samplesize)
    lOffD1 <- vector(length=input$wishart_samplesize)
    lDiag <- vector(length=input$wishart_samplesize)
    for (i in 1:input$wishart_samplesize){
      aMatrix <- lList[,,i]
      lOffD[i] <- aMatrix[1,2]/sqrt(aMatrix[1,1]*aMatrix[2,2])
      lOffD1[i] <- aMatrix[2,3]/sqrt(aMatrix[2,2]*aMatrix[3,3])
      lDiag[i] <- aMatrix[1,1]
    }
    h1 <- qplot(log(lDiag),fill=I("blue")) + 
      xlab(expression(paste("log(",sigma[1]^2,")"))) + 
      ylab("count") +
      theme_classic() +
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size=14))
    aDataF <- data.frame(x=lOffD,y=lOffD1)
    m <- ggplot(aDataF, aes(x = x, y = y)) +
      geom_point() + xlab(expression(rho[12])) + ylab(expression(rho[23]))
    p <- m + geom_density2d(size=1) +
      theme_classic() +
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size=14))
    p1<-ggExtra::ggMarginal(p,type = "histogram",fill=I("blue"))
    grid.arrange(h1,p1,ncol = 2,
                 top=textGrob("Samples from distribution",gp=gpar(fontsize=20,font=1)))
    
  }else if (input$dist2=='InverseWishart'){
    lList<-rWishart(input$inversewishart_samplesize,input$inversewishart_df,diag(input$inversewishart_dimension))
    lOffD <- vector(length=input$inversewishart_samplesize)
    lOffD1 <- vector(length=input$inversewishart_samplesize)
    lDiag <- vector(length=input$inversewishart_samplesize)
    for (i in 1:input$inversewishart_samplesize){
      aMatrix <- solve(lList[,,i])
      lOffD[i] <- aMatrix[1,2]/sqrt(aMatrix[1,1]*aMatrix[2,2])
      lOffD1[i] <- aMatrix[2,3]/sqrt(aMatrix[2,2]*aMatrix[3,3])
      lDiag[i] <- aMatrix[1,1]
    }
    h1 <- qplot(log(lDiag),fill=I("blue")) + 
      xlab(expression(paste("log(",sigma[1]^2,")"))) + 
      ylab("count") +
      theme_classic() +
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size=14))
    aDataF <- data.frame(x=lOffD,y=lOffD1)
    m <- ggplot(aDataF, aes(x = x, y = y)) +
      geom_point() + xlab(expression(rho[12])) + ylab(expression(rho[23]))
    p <- m + geom_density2d(size=1) +
      theme_classic() +
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size=14))
    p1<-ggExtra::ggMarginal(p,type = "histogram",fill=I("blue"))
    grid.arrange(h1,p1,ncol = 2,
                 top=textGrob("Samples from distribution",gp=gpar(fontsize=20,font=1)))
  } else if (input$dist2=='Dirichlet'){
    if(input$dirichlet_dimension==2){
      lSamples <- rdirichlet(input$dirichlet_samplesize, c(input$dirichlet_alpha1,input$dirichlet_alpha2))
      qplot(lSamples[,1],fill=I("blue")) + xlab(expression(p[1])) +
        theme_classic() +
        ylab("count") +
        ggtitle("Samples from distribution") +
        theme(axis.title = element_text(size=18),
              axis.text = element_text(size=14),
              title = element_text(size=20))
      
    } else if (input$dirichlet_dimension==3){     
      plot(DR_data(rdirichlet(input$dirichlet_samplesize,
                              c(input$dirichlet_alpha1,input$dirichlet_alpha2,input$dirichlet_alpha3))),
           a2d = list(colored = TRUE),
           dim.labels=c(expression(p[1]),expression(p[2]),expression(p[3])))
    } else {     
      plot(DR_data(rdirichlet(input$dirichlet_samplesize, c(input$dirichlet_alpha1,input$dirichlet_alpha2,input$dirichlet_alpha3,input$dirichlet_alpha4))), a3d = list(colored = TRUE,rgl=FALSE),dim.labels=c(expression(p[1]),expression(p[2]),expression(p[3]),expression(p[4])))
    }
    
  } else if(input$dist2=='Multinomial'){
    X <- t(as.matrix(expand.grid(0:input$multinomial_size, 0:input$multinomial_size)))
    X <- X[ , colSums(X) <= input$multinomial_size]; X <- rbind(X, input$multinomial_size - colSums(X))
    Z <- round(apply(X, 2, function(x) dmultinom(x, prob = c(input$multinomial_prob1,input$multinomial_prob2,input$multinomial_prob3))), 3)
    A <- data.frame(x = X[1, ], y = X[2, ], probability = Z)
    scatterplot3d(A, type = "h", lwd = 3, 
                  highlight.3d=TRUE, 
                  box = FALSE,
                  angle=input$multinomial_angle,
                  cex.lab = 2,
                  cex.axis = 1)
    
  } else if(input$dist2=='LKJ'){
    lSamples <- lapply(seq(1,input$lkj_samplesize,1), function(x) rcoronion(input$lkj_dimension,
                                                                           input$lkj_eta))
    lSample12 <- unlist(lapply(lSamples,function(x) x[1,2]))
    lSample23 <- unlist(lapply(lSamples,function(x) x[2,3]))
    aDataF <- data.frame(x=lSample12,y=lSample23)
    m <- ggplot(aDataF, aes(x = x, y = y)) +
      geom_point() + xlab(expression(rho[12])) + ylab(expression(rho[23]))
    p <- m + geom_density2d(size=1) +
      theme_classic() +
      ggtitle("Samples from distribution") +
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size=14),
            title = element_text(size=20))
    p1<-ggExtra::ggMarginal(p,type = "histogram",fill=I("blue"))
    p1
  }
}
