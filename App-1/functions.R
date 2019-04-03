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
  return((1/aInt[1]$value)*(pcauchy(x,location,scale) - pcauchy(0, location, scale)))
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

# from https://stats.stackexchange.com/questions/3930/are-there-default-functions-for-discrete-uniform-distributions-in-r/3940
dunifdisc<-function(x, min=0, max=1) ifelse(x>=min & x<=max & round(x)==x, 1 / (max - min + 1), 0)
punifdisc<-function(q, min=0, max=1) ifelse(q<min, 0, ifelse(q>=max, 1, (floor(q)-min+1)/(max-min+1)))
qunifdisc<-function(p, min=0, max=1) floor(p*(max-min+1))
runifdisc<-function(n, min=0, max=1) sample(min:max, n, replace=T)

fMakeFunctionPaste <- function(mainName, params, prefixparams=NULL,
                               postfixparams=NULL, import=NULL, freeform=NULL,
                               mathematica=FALSE, julia=FALSE, named_arguments=NULL,
                               vector_params=FALSE, python_vector=FALSE, other_params=FALSE,
                               end_brace=FALSE, mathematica_vector=FALSE){
  if(mathematica){
    a_forward_brace <- "["
    a_backward_brace <- "]"
  }else{
    a_forward_brace <- "("
    a_backward_brace <- ")"
  }
  if(is.null(freeform)){
    if(is.null(named_arguments)){
      common_prose <- paste(sapply(params, function(x) eval(parse(text=x))), collapse=", ")
      if(vector_params){
        if(!python_vector&!mathematica_vector)
          common_prose <- paste0("c(", common_prose, ")")
        else if(mathematica_vector)
          common_prose <- paste0("{", common_prose, "}")
        else{
          common_prose <- paste0("[", common_prose, "]")
          if(other_params)
            common_prose <- paste0(other_params, ", ", common_prose)
        }
      }
    }
    else{
      vars <- map_dbl(params,function(x) eval(parse(text=x)))
      named_arguments1 <- map_chr(named_arguments, ~paste0(.,"="))
      if(length(named_arguments1) > 1)
        common_prose <- paste(map2_chr(named_arguments1, vars, function(x,y) paste0(x,y)), collapse=", ")
      else{
        common_prose <- paste0(named_arguments1, "c(", paste0(vars, collapse=", "), ")")
      }
        
    }
      
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
  if(end_brace)
    lWords <- paste0(lWords, a_backward_brace)
  return(lWords)
}

fMakeFunctionPaste_stan <- function(mainName, params, prefixparams=NULL,
                               postfixparams=NULL, import=NULL, freeform=NULL,
                               mathematica=FALSE, julia=FALSE, named_arguments=NULL,
                               vector_params=FALSE, python_vector=FALSE, other_params=FALSE,
                               end_brace=FALSE){
    a_forward_brace <- "("
    a_backward_brace <- ")"
  if(is.null(freeform)){
    if(is.null(named_arguments)){
      common_prose <- paste(sapply(params, function(x) eval(parse(text=x))), collapse=", ")
      if(vector_params){
        if(!python_vector)
          common_prose <- paste0("c(", common_prose, ")")
        else{
          common_prose <- paste0("[", common_prose, "]")
          if(other_params)
            common_prose <- paste0(other_params, ", ", common_prose)
        }
      }
    }
    else{
      vars <- map_dbl(params,function(x) eval(parse(text=x)))
      named_arguments1 <- map_chr(named_arguments, ~paste0(.,"="))
      if(length(named_arguments1) > 1)
        common_prose <- paste(map2_chr(named_arguments1, vars, function(x,y) paste0(x,y)), collapse=", ")
      else{
        common_prose <- paste0(named_arguments1, "c(", paste0(vars, collapse=", "), ")")
      }
      
    }
    
    prefix_prose <- paste(prefixparams,  collapse = ", ")
    postfix_prose <- paste(postfixparams,  collapse = ", ")
    if(!is.null(prefixparams))
      if(!is.null(postfixparams))
        words <- paste0(mainName, a_forward_brace, prefix_prose, "| ", common_prose, ", ", postfix_prose, a_backward_brace)
      else
        words <- paste0(mainName, a_forward_brace, prefix_prose, "| ", common_prose, a_backward_brace)
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
  if(end_brace)
    lWords <- paste0(lWords, a_backward_brace)
  return(lWords)
}



fCalculateMeanFull <- function(input){
  lExtra <- if (input$distType=='Continuous'){
    switch(input$dist,
           Normal=input$normal_mu,
           Uniform = 0.5 * (input$uniform_a + input$uniform_b),
           LogNormal = exp(input$lognormal_mu+0.5*input$lognormal_sigma^2),
           Exponential = 1/input$exponential_rate,
           Gamma= input$gamma_shape / input$gamma_rate,
           t = ifelse(input$t_nu>1,input$t_mu,NA),
           Beta=input$beta_a/(input$beta_a+input$beta_b),
           Cauchy=NA,
           HalfCauchy=NA,
           InverseGamma=ifelse(input$inversegamma_shape>1,input$inversegamma_scale/(input$inversegamma_shape-1),NA),
           InverseChiSquared=ifelse(input$inversechisquared_df>2,1/(input$inversechisquared_df-2),NA),
           LogitNormal=integrate(function(x) x * (1/(input$logitnormal_sigma * sqrt(2 * pi))) * (1/(x * (1 - x))) * exp(- (log(x/(1-x)) - input$logitnormal_mu)^2 / (2 * input$logitnormal_sigma^2)),0,1)[[1]],
           1)
  } else if (input$distType=='Discrete'){
    switch(input$dist1,
           Bernoulli=input$bernoulli_prob,
           Binomial=input$binomial_size * input$binomial_prob,
           DiscreteUniform=0.5 * (input$discreteuniform_lower + input$discreteuniform_upper),
           Poisson=input$poisson_lambda,
           NegativeBinomial=input$negativebinomial_mean,
           BetaBinomial=input$betabinomial_size * input$betabinomial_shape1 / (input$betabinomial_shape1 + input$betabinomial_shape2),
           paste("mean=1,sd=1")
    )
  }
}

fCalculateVarianceFull <- function(input){
  if(input$distType=='Continuous'){
    if(input$dist=='LogitNormal'){
      aMeanLogitNormal <- integrate(function(x) x * (1/(input$logitnormal_sigma * sqrt(2 * pi))) * (1/(x * (1 - x))) * exp(- (log(x/(1-x)) - input$logitnormal_mu)^2 / (2 * input$logitnormal_sigma^2)),0,1)[[1]]
      a2LogitNormal <- integrate(function(x) x^2 * (1/(input$logitnormal_sigma * sqrt(2 * pi))) * (1/(x * (1 - x))) * exp(- (log(x/(1-x)) - input$logitnormal_mu)^2 / (2 * input$logitnormal_sigma^2)),0,1)[[1]]
    }
    aVar <- switch(input$dist,
                   Normal=input$normal_sigma^2,
                   Uniform = (1/12) * (input$uniform_b - input$uniform_a)^2,
                   LogNormal = exp(input$lognormal_sigma^2 - 1) * exp(2 * input$lognormal_mu + input$lognormal_sigma^2),
                   Exponential = 1/input$exponential_rate^2,
                   Gamma= input$gamma_shape / input$gamma_rate^2,
                   t = ifelse(input$t_nu > 2,
                              input$t_nu / (input$t_nu - 2), NA),
                   Beta=(input$beta_a * input$beta_b) / ((input$beta_a+input$beta_b)^2 * (input$beta_a+input$beta_b + 1)),
                   Cauchy=NA,
                   HalfCauchy=NA,
                   InverseGamma=ifelse(input$inversegamma_shape > 2,
                                       input$inversegamma_scale/((input$inversegamma_shape-1)^2 * (input$inversegamma_shape-2)),NA),
                   InverseChiSquared=ifelse(input$inversechisquared_df > 4,
                                            2 / ((input$inversechisquared_df-2)^2 * (input$inversechisquared_df-4)),NA),
                   LogitNormal=a2LogitNormal-aMeanLogitNormal^2,
                   1)
  }else if (input$distType=='Discrete'){
    aVar <- switch(input$dist1,
                   Bernoulli=input$bernoulli_prob * (1 - input$bernoulli_prob),
                   Binomial=input$binomial_size * input$binomial_prob * (1 - input$binomial_prob),
                   DiscreteUniform= (1 / 12) * ((input$discreteuniform_upper - input$discreteuniform_lower + 1)^2 - 1),
                   Poisson=input$poisson_lambda,
                   NegativeBinomial=input$negativebinomial_mean + (input$negativebinomial_mean^2 / input$negativebinomial_dispersion),
                   BetaBinomial=input$betabinomial_size * input$betabinomial_shape1 * input$betabinomial_shape2 *(input$betabinomial_shape1 + input$betabinomial_shape2 + input$betabinomial_size) / ((input$betabinomial_shape1 + input$betabinomial_shape2)^2 * (input$betabinomial_shape1 + input$betabinomial_shape2 + 1))
    )
  }
  
  return(aVar)
}
