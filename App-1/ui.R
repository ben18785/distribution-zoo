library(shiny)
library(purrr)

prismDependencies <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
  tags$link(rel = "stylesheet", type = "text/css",
            href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
)
prismLanguageDependencies <- function(languages) {
  lapply(languages, function(x) {
    tags$head(
      tags$script(
        src = paste0("https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-",
                     x, ".min.js")
      )
    )
  })
}


# Define UI for random distribution application
ben_link <- a("Ben Lambert", href="https://ben-lambert.com/bayesian/", target="_blank")
fergus_link <- a("Fergus Cooper", href="https://www.cs.ox.ac.uk/people/fergus.cooper/site/", target="_blank")
survey_link <- a("two minute survey", href="https://oxford.onlinesurveys.ac.uk/distribution_zoo_evaluation", target="_blank")

ga_30_line <- ""
ga_all_line <- ""

try({
  ga_30 <- rjson::fromJSON(file="https://fcooper8472.github.io/distribution-zoo-analytics/data_30.json")
  ga_all <- rjson::fromJSON(file="https://fcooper8472.github.io/distribution-zoo-analytics/data_all_time.json")
  ga_30_line <- h4("Last month: used by ", ga_30['user_count'], " people over ", ga_30['session_count'], "sessions in ", ga_30['country_count'], " countries")
  ga_all_line <- h4("Since created: used by ", ga_all['user_count'], " people over ", ga_all['session_count'], "sessions in ", ga_all['country_count'], " countries")
})

shinyUI(fluidPage(

  tags$head(includeHTML(("google-analytics.html"))),
  includeCSS("styles.css"),

  # Application title
  headerPanel("The distribution zoo"),
  tagList(h4("by")),
  fluidRow(h4(ben_link, " and ", fergus_link), ga_30_line, ga_all_line),
  prismDependencies,
  prismLanguageDependencies(c("r", "python", "latex",
                              "matlab", "mathematica", "c-like",
                              "c", "cpp", "julia")),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      selectInput("distType", "Category of distribution",
                   c("Continuous Univariate"="Continuous",
                     "Discrete Univariate"="Discrete",
                     "Multivariate"="Multivariate"),
                   selected="Continuous"),
      conditionalPanel("input.distType=='Continuous'",
      selectInput("dist", "Distribution type:",
                   c("Beta"="Beta",
                     "Cauchy"="Cauchy",
                     "Exponential" = "Exponential",
                     "Gamma" = "Gamma",
                     "Half-Cauchy"="HalfCauchy",
                     "Inverse-Chi-Squared"="InverseChiSquared",
                     "Inverse-Gamma"="InverseGamma",
                     "Logit-Normal"="LogitNormal",
                     "Log-Normal" = "LogNormal",
                     "Normal" = "Normal",
                     "Student-t" = "t",
                     "Uniform" = "Uniform"),
                  selected="Normal")),
      conditionalPanel("input.distType=='Discrete'",
                       selectInput("dist1", "Distribution type:",
                                    c("Bernoulli" = "Bernoulli",
                                      "Beta-Binomial" = "BetaBinomial",
                                      "Binomial" = "Binomial",
                                      # "Categorical"="Categorical",
                                      "Discrete-Uniform" = "DiscreteUniform",
                                      "Negative-Binomial" = "NegativeBinomial",
                                      "Poisson" = "Poisson"))),
      conditionalPanel("input.distType=='Multivariate'",
                       selectInput("dist2", "Distribution type:",
                                   c("Dirichlet"="Dirichlet",
                                     "Inverse-Wishart"="InverseWishart",
                                     "LKJ"="LKJ",
                                     "Multinomial"="Multinomial",
                                     "Multivariate Normal" = "MultivariateNormal",
                                     "Multivariate Student-t" = "MultivariateT",
                                     "Wishart"="Wishart"))),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist!='Beta'",
                       sliderInput("n", "Range", value = 10,min = 0, max = 1000)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Normal'",
                       sliderInput("normal_mu", "Mean", min=-30, max=30, value=0, step=0.2),
                       sliderInput("normal_sigma", "Standard deviation", min=0.1, max=20, value=1, step=0.2)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Uniform'",
                       sliderInput("uniform_a", "Lower bound", min=-30, max=0, value=0, step=0.2),
                       sliderInput("uniform_b", "Upper bound", min=0.1, max=30, value=1, step=0.2)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='LogNormal'",
                       sliderInput("lognormal_mu", "Mean of log", min=-10, max=10, value=0, step=0.2),
                       sliderInput("lognormal_sigma", "Standard deviation of log", min=0.1, max=20, value=1, step=0.2)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Exponential'",
                       sliderInput("exponential_rate", "Rate parameter", min=0, max=5.0, value=0.5, step=0.2)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Gamma'",
                       sliderInput("gamma_shape", "Shape parameter", min=0.1, max=10, value=1),
                       sliderInput("gamma_rate", "Rate parameter", min=0.1, max=2, value=0.5)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='t'",
                       sliderInput("t_mu", "mode", min=-30, max=30, value=0, step=0.2),
                       sliderInput("t_sigma", "sigma parameter", min=0.1, max=10, value=1, step=0.2),
                       sliderInput("t_nu", "degrees of freedom", min=0, max=20, value=3, step=0.2)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Beta'",
                       sliderInput("beta_a", "Shape parameter 1", min=0.5, max=10, value=1),
                       sliderInput("beta_b", "Shape parameter 2", min=0.5, max=10, value=1)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='Cauchy'",
                       sliderInput("cauchy_location", "location parameter", min=-30, max=30, value=0, step=0.2),
                       sliderInput("cauchy_scale", "scale parameter", min=0.5, max=10, value=1, step=0.2)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='HalfCauchy'",
                       sliderInput("halfcauchy_location", "location parameter", min=-30, max=30, value=0, step=0.2),
                       sliderInput("halfcauchy_scale", "scale parameter", min=0.5, max=10, value=1, step=0.2)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='InverseGamma'",
                       sliderInput("inversegamma_shape", "shape parameter", min=0.5, max=10, value=2),
                       sliderInput("inversegamma_scale", "scale parameter", min=0.5, max=10, value=1)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='InverseChiSquared'",
                       sliderInput("inversechisquared_df", "degrees of freedom", min=0.5, max=10, value=3)),
      conditionalPanel(condition="input.distType=='Continuous'&&input.dist=='LogitNormal'",
                       sliderInput("logitnormal_mu", "mu parameter", min=-10, max=10, value=1, step=0.2),
                       sliderInput("logitnormal_sigma", "sigma parameter", min=0.5, max=10, value=1, step=0.2)),
      conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='Bernoulli'",
                       sliderInput("bernoulli_prob", "probability", min=0, max=1, value=0.5)),
      conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='BetaBinomial'",
                       sliderInput("betabinomial_size", "Size", min=0, max=50, value=10),
                       sliderInput("betabinomial_shape1", "Shape parameter 1", min=0, max=50, value=1, step=0.2),
                       sliderInput("betabinomial_shape2", "Shape parameter 2", min=0, max=50, value=1, step=0.2)),
      conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='Binomial'",
                       sliderInput("binomial_size", "size", min=0, max=50, value=10),
                       sliderInput("binomial_prob", "probability", min=0, max=1, value=0.5)),
      conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='DiscreteUniform'",
                       sliderInput("discreteuniform_lower", "Lower bound", min=-30, max=0, value=0, step=1),
                       sliderInput("discreteuniform_upper", "Upper bound", min=0, max=30, value=1, step=1)),
      conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='Poisson'",
                       sliderInput("poisson_lambda", "Rate", min=0, max=50, value=10, step=0.2),
                       sliderInput("poisson_range", "Range", min=0, max=100, value=40)),
      conditionalPanel(condition="input.distType=='Discrete'&&input.dist1=='NegativeBinomial'",
                       sliderInput("negativebinomial_mean", "Mean", min=0, max=50, value=10, step=0.2),
                       sliderInput("negativebinomial_dispersion", "Inverse-dispersion", min=0, max=100, value=3,step=0.2),
                       sliderInput("negativebinomial_range", "Range", min=0, max=100, value=40)),
      conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='MultivariateNormal'",
                       sliderInput("multivariatenormal_mux", "Mean of X", min=-10, max=10, value=0, step=0.2),
                       sliderInput("multivariatenormal_muy", "Mean of y", min=-10, max=10, value=0, step=0.2),
                       sliderInput("multivariatenormal_sigmax", "Standard deviation of x", min=0, max=5, value=1, step=0.2),
                       sliderInput("multivariatenormal_sigmay", "Standard deviation of y", min=0, max=5, value=1, step=0.2),
                       sliderInput("multivariatenormal_rho", "Correlation between x and y", min=-1, max=1, value=0, step=0.2),
                       sliderInput("multivariatenormal_range", "Range of plot", min=0, max=100, value=10)),
      conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='MultivariateT'",
                       sliderInput("multivariatet_mux", "Mode of x", min=-10, max=10, value=0, step=0.2),
                       sliderInput("multivariatet_muy", "Mode of y", min=-10, max=10, value=0, step=0.2),
                       sliderInput("multivariatet_sigmax", "Sigma x", min=0, max=5, value=1, step=0.2),
                       sliderInput("multivariatet_sigmay", "Sigma y", min=0, max=5, value=1, step=0.2),
                       sliderInput("multivariatet_rho", "Correlation component", min=-1, max=1, value=0,step=0.2),
                       sliderInput("multivariatet_df", "Degrees of freedom", min=0, max=50, value=10,step=0.2),
                       sliderInput("multivariatet_range", "Range of plot", min=0, max=100, value=10)),
      conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='Wishart'",
                       sliderInput("wishart_dimension", "Dimensions", min=4, max=20, value=4),
                       sliderInput("wishart_df", "Degrees of freedom", min=4, max=100, value=8, step=0.2),
                       sliderInput("wishart_samplesize", "Sample size", min=1000, max=20000, value=5000)),
      conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='InverseWishart'",
                       sliderInput("inversewishart_dimension", "Dimensions", min=4, max=20, value=4),
                       sliderInput("inversewishart_df", "Degrees of freedom", min=4, max=100, value=8, step=0.2),
                       sliderInput("inversewishart_samplesize", "Sample size", min=1000, max=20000, value=5000)),
      conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='Dirichlet'",
                       sliderInput("dirichlet_dimension","Dimensions",min=2,max=4,value=2,step=1),
                       sliderInput("dirichlet_samplesize", "Sample size", min=10, max=20000, value=1000),
                       sliderInput("dirichlet_alpha1","alpha 1",min=0.1,max=10,value=2),
                       sliderInput("dirichlet_alpha2","alpha 2",min=0.1,max=10,value=2),
                       conditionalPanel(condition="input.dirichlet_dimension>'2'",
                        sliderInput("dirichlet_alpha3","alpha 3",min=0.1,max=10,value=2),
                        conditionalPanel(condition="input.dirichlet_dimension>'3'",
                                         sliderInput("dirichlet_alpha4","alpha 4",min=0.1,max=10,value=2)))),
      conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='Multinomial'",
                       sliderInput("multinomial_angle","Viewpoint angle",min=0,max=360,value=100),
                       sliderInput("multinomial_size","size",min=2,max=100,value=6),
                       sliderInput("multinomial_prob1","unnormalised probability 1",min=0,max=1,value=0.5),
                       sliderInput("multinomial_prob2","unnormalised probability 2",min=0,max=1,value=0.5),
                       sliderInput("multinomial_prob3","unnormalised probability 3",min=0,max=1,value=0.5)),
      conditionalPanel(condition="input.distType=='Multivariate'&&input.dist2=='LKJ'",
                       sliderInput("lkj_dimension", "Dimensions", min=4, max=20, value=4),
                       sliderInput("lkj_eta", "Degrees of freedom", min=0, max=40, value=1,step=0.2),
                       sliderInput("lkj_samplesize", "Sample size", min=1000, max=20000, value=2000)),
      br()
    ),
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      uiOutput('mytabs')
    )
  )
)
)
