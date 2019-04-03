
fPlotCDF <- function(input, aDist, aMean, aVar, lScale, lExtra){
  
  if (input$distType=='Continuous'){
    
    aLen <- length(lScale)
    lPDF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
    g <- qplot(lScale,lPDF,geom="path",
          xlab="X",ylab="cumulative probability",
          xlim=c(lScale[1],lScale[aLen])) +
      geom_line(color='darkblue',size=1) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.text = element_text(size=14),
            axis.title = element_text(size=16)) +
      ylim(0, NA) +
      ggtitle(paste0("mean (orange line) = ", round(aMean, 2), ", sd = ", round(sqrt(aVar), 2)))
    if(!is.na(aMean))
      g + geom_vline(xintercept=aMean,
                     colour="orange",
                     linetype = "longdash",
                     size=1)
    else
      g
    
  } else if (input$distType=='Discrete'){
    
    aLen <- length(lScale)
    lPMF <- unlist(lapply(lScale, function(x) eval(parse(text=paste("aDist(x,",lExtra,")")))))
    dataF <- data.frame(a=lScale,pmf=lPMF)
    ggplot(data=dataF, aes(x=a, y=pmf)) +
      geom_col(fill="darkblue", colour="black") + xlab('X')+ 
      geom_vline(xintercept=(aMean),
                 colour="orange",
                 linetype = "longdash",
                 size=1) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 18),
            axis.text = element_text(size=14),
            axis.title = element_text(size=16)) +
      ylim(0, NA) +
      ylab('cumulate probability') +
      ggtitle(paste0("mean (orange line) = ", round(aMean, 2), ", sd = ", round(sqrt(aVar), 2)))
  }else if (input$dist2=='MultivariateNormal'){
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
        z[i,j] <- pmvnorm(lower=c(-Inf,-Inf),upper=c(x.points[i],y.points[j]),
                          mean=lMean,sigma=lSigma)
      }
    }
    dataF <- melt(z)
    dataF$X1<-rep(x.points,aLen)
    dataF$X2 <- unlist(lapply(x.points,function(x) rep(x,aLen)))
    names(dataF) <- c("x", "y", "cdf")
    ggplot(dataF, aes(x, y, z = cdf))+ geom_tile(aes(fill = cdf)) + stat_contour()+
      xlim(c(-input$multivariatenormal_range,input$multivariatenormal_range)) + ylim(c(-input$multivariatenormal_range,input$multivariatenormal_range)) 
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
        z[i,j] <- mvtnorm::pmvt(lower=c(-Inf,-Inf),upper=c(x.points[i],y.points[j]),
                                delta=lMean,sigma=lSigma,df=input$multivariatet_df)
      }
    }
    dataF <- melt(z)
    dataF$X1<-rep(x.points,aLen)
    dataF$X2 <- unlist(lapply(x.points,function(x) rep(x,aLen)))
    names(dataF) <- c("x", "y", "cdf")
    ggplot(dataF, aes(x, y, z = cdf))+ geom_tile(aes(fill = cdf)) + stat_contour()+
      xlim(c(-input$multivariatenormal_range,input$multivariatenormal_range)) + ylim(c(-input$multivariatenormal_range,input$multivariatenormal_range)) 
  }
}
