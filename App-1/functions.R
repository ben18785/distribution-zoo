## format code with tags and language
prismAddTags <- function(code, language = "r") {
  paste0("<pre><code class = 'language-", language, "'>",
         code, 
         "</code></pre>")
}
prismCodeBlock <- function(code, language = "r") {
  tagList(
    HTML(prismAddTags(code, language = language)),
    tags$script("Prism.highlightAll()")
  )
}

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
  if(x >= 0)
    return(1.0 / (pi * (1 + ((x-location)/scale)^2) * scale * (0.5 + atan(location / scale) / pi)))
  else
    return(0.0)
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

fMakeFunctionPaste <- function(mainName, params, prefixparams=NULL,postfixparams=NULL, import=NULL, freeform=NULL, mathematica=FALSE, julia=FALSE){
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
      lWords <- words
    else
      lWords <- paste(import, words, sep="\n")
  }else{
    lWords <- mainName
  }
  return(lWords)
}


fCalculateMeanFull <- function(input){
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
}

fCalculateVarianceFull <- function(input){
  if(input$distType=='Continuous'){
    if(input$dist=='LogitNormal'){
      aMeanLogitNormal <- integrate(function(x) x * (1/(input$sigmaLogitN * sqrt(2 * pi))) * (1/(x * (1 - x))) * exp(- (log(x/(1-x)) - input$muLogitN)^2 / (2 * input$sigmaLogitN^2)),0,1)[[1]]
      a2LogitNormal <- integrate(function(x) x^2 * (1/(input$sigmaLogitN * sqrt(2 * pi))) * (1/(x * (1 - x))) * exp(- (log(x/(1-x)) - input$muLogitN)^2 / (2 * input$sigmaLogitN^2)),0,1)[[1]]
    }
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
}