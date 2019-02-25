fBulletPoint <- function(text){
  return(paste0("<li>", text, "</li>"))
}

fBulletPoints <- function(ltext){
  return(HTML(paste0("<ul>", paste0(map(ltext, ~fBulletPoint(.)), collapse = ""), "</ul>")))
}


fHelperTips <- function(likelihood_example, prior_example, preferred_distribution=NULL){
  if(!is.null(preferred_distribution))
     tagList(h2("Example uses"),
       h3("Likelihood for"),
          fBulletPoints(likelihood_example),
          h3("Prior for"),
          fBulletPoints(prior_example),
          h2("Preferred distribution"),
          fBulletPoints(preferred_distribution))
     else
       tagList(h2("Example uses"),
               h3("Likelihood for"),
               fBulletPoints(likelihood_example),
               h3("Priors for"),
               fBulletPoints(prior_example))
}


fExampleUses <- function(input){
  text <- 
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fHelperTips(c("Error term in linear regression model",
                                  "Body temperature measurements"),
                                c("Regression coefficient",
                                  "Mean in a model of heights a population")),
             Uniform=fHelperTips(c("Failure point along a railway track"),
                                 c("Probability that a section of railway track malfunctions on a given day"),
                                 c("Beta distribution because it is more general")),
             LogNormal=fHelperTips(c("Wealth of individuals",
                                     "Density of HIV virions in blood"),
                                   c("Overdispersion parameter for negative binomial")),
             Exponential=fHelperTips(c("Waiting time before a mutation arises in a population",
                                       "Distance travelled by elephants in 15 minutes"),
                                     c("Variance parameter in model determining phenotype frequencies in a population")),
             Gamma=fHelperTips(c("Daily rainfall amount"),
                               c("Variance parameter in model for daily rainfall amount")),
             t=fHelperTips(c("Individual stock returns",
                             "Error term in regression model with considerable uncertainty"),
                           c("Mean stock return",
                             "Regression coefficients with considerable uncertainty")),
             Beta=fHelperTips(c("Proportion of individuals with a disease in a population",
                                "Probability an individual votes for conservative party in next election"),
                              c("Probability a coin lands heads up on a given flip")),
             Cauchy=fHelperTips(c("Individual stock returns"),
                                c("Mode in a sampling distribution to describe individual wealths in a population")),
             HalfCauchy=fHelperTips(c("Wealth of individuals"),
                                    c("Variance parameter in model of stock returns")),
             InverseGamma=fHelperTips(c("Distance travelled by elephants in one hour"),
                                      c("Variance parameter in model of disease incidence"),
                                      c("For priors, a half-Cauchy distribution is preferable for variance and scale parameters because inverse-gamma distributions are sensitive to their inputs when these parameters are near zero")),
             InverseChiSquared=fHelperTips(c("Distance travelled by elephants in one hour"),
                                           c("Variance parameter in model of disease incidence"),
                                           c(c("For priors, a half-Cauchy distribution is preferable for variance and scale parameters because inverse-chi-squared distributions are sensitive to their inputs when these parameters are near zero"))),
             LogitNormal=fHelperTips(c("Disease prevalence in hierarchical model"),
                                     c("Probability of developing a particular disease"),
                                     c("Dependent on circumstance, it may be worth considering a beta distribution as an alternative"))
      )
    }else if(input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=fHelperTips(c("Infection status of a single individual",
                                     "Whether an individual animal is seen by a camera trap on a given day"),
                                   c("In models with an unobserved hidden binary state, a Bernoulli model is often used as a prior (although it's often not called that)")),
             Binomial=fHelperTips(c("Count of infected individuals across n sampled persons"),
                                  c("No examples come to mind - using this distribution as a prior is not impossible but uncommon")),
             DiscreteUniform=fHelperTips(c("No examples come to mind - using this distribution as a likelihood is not impossible but uncommon since the binomial has the same support and is more flexible"),
                                         c("Often used as a prior across multiple latent (i.e. unobserved) discrete states in a larger statistical model")),
             Poisson=fHelperTips(c("Count of component failures per week"),
                                 c("No examples come to mind - using this distribution as a prior is not impossible but uncommon"),
                                 c("In many biological applications, over-dispersion occurs and so a negative binomial may be worth considering")),
             NegativeBinomial=fHelperTips(c("Number of mosquitoes caught in one day",
                                "Number of cars passing a traffic light in one hour"),
                                c("No examples come to mind - using this distribution as a prior is not impossible but uncommon")),
             BetaBinomial=fHelperTips(c("Count of bank failures across Europe in one year"),
                                      c("No examples come to mind - using this distribution as a prior is not impossible but uncommon"))
      )
    }else if(input$distType=='Multivariate'){
      switch(input$dist2,
             MultivariateNormal=fHelperTips(c("Protein concentrations across many genes"),
                                            c("Regression coefficient vectors")),
             MultivariateT=fHelperTips(c("Stock returns of across many companies"),
                                       c("Mean returns in model for stock price changes")),
             Multinomial=fHelperTips(c("Blood type counts across n individuals",
                                       "Numbers of people voting for each party in a sample"),
                                     c("No examples come to mind - using this distribution as a prior is not impossible but very uncommon")),
             Wishart=fHelperTips(c("Covariance of stock returns"),
                                 c("Covariance of stock returns in a multivariate normal likelihood"),
                                 c("In many applications, an LKJ distribution is a better alternative to the Wishart family because Wisharts place strong restrictions on correlations between parameters")),
             InverseWishart=fHelperTips(c("Covariance of stock returns"),
                                        c("Covariance of stock returns in a multivariate normal likelihood"),
                                        c("In many applications, an LKJ distribution is a better alternative to the Wishart family because Wisharts place strong restrictions on correlations between parameters")),
             LKJ=fHelperTips(c("Correlation matrix of stock returns",
                               "In conjunction with a scale parameter, a covariance matrix of stock returns"),
                             c("Correlation matrix representing genealogical relationships between individuals")),
             Dirichlet=fHelperTips(c("Proportions of individuals voting for each party in an election"),
                                   c("Probabilities of having a range of phenotypes"))
      )
    }
  return(text)
}