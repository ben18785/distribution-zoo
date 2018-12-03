
fScaleFull <- function(input) {
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
  return(lScale)
}

fScaleFull1 <- function(input){
  lScale <- switch(input$dist1,
                   Bernoulli=c(0,1),
                   Binomial=seq(0,input$sizeBin,1),
                   Poisson=seq(0,input$rangePois,1),
                   NegativeBinomial=seq(0,input$rangeNB,1),
                   BetaBinomial=seq(0,input$sizeBetaBin,1)
  )
  return(lScale)
}

fExtraFunctionInputsFull <- function(input){
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
}

fExtra1FunctionInputsFull <- function(input){
  lExtra <- switch(input$dist1,
                   Bernoulli=paste("prob=",input$probBer),
                   Binomial=paste("size=",input$sizeBin,",prob=",input$probBin),
                   Poisson=paste("lambda=",input$lambdaPois),
                   NegativeBinomial=paste("mu=",input$meanNB,",size=",input$dispersionNB),
                   BetaBinomial=paste("n=",input$sizeBetaBin,",alpha=",input$shapeBetaBin1,",beta=",input$shapeBetaBin2),
                   paste("mean=1,sd=1"))
  return(lExtra)
}