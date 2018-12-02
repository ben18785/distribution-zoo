
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