# ---------------------------------------------------------------------------
# Shared test setup: locate the app, source its files, and build the
# `input` fixtures that the app's functions expect.
#
# The app is a Shiny app whose logic lives in plain functions that all take a
# Shiny-style `input` list (input$distType, input$normal_mu, ...). For testing
# we reconstruct that list in plain R, so the functions can be exercised
# without launching a Shiny session.
# ---------------------------------------------------------------------------

suppressMessages({
  library(shiny)   # tag rendering used by fFormulae / fLatex / code generators
  library(purrr)   # map_dbl / map_chr used inside fMakeFunctionPaste
  library(dplyr)   # if_else used by the Dirichlet generator in code_r.R
                   # (the live app gets this via server.R's library(tidyverse))
})

# Walk upwards from the working directory until we find the app root.
find_app_root <- function(start = getwd()) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in 1:8) {
    if (file.exists(file.path(d, "server.R")) &&
        file.exists(file.path(d, "functions.R"))) {
      return(d)
    }
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }
  stop("Could not locate the distribution-zoo app directory (no server.R / functions.R found above ", start, ")")
}

APP_ROOT <- find_app_root()

# Source the app's logic (not server.R / ui.R, which need a live Shiny session).
.app_files <- c(
  "functions.R", "formulae.R",
  "code_latex.R", "code_r.R", "code_python.R", "code_stan.R",
  "code_matlab.R", "code_mathematica.R", "code_julia.R", "code_cplusplus.R",
  "plotting.R", "PDF.R", "CDF.R", "example_uses.R"
)
for (f in .app_files) {
  source(file.path(APP_ROOT, f), chdir = TRUE, local = FALSE)
}

# ---------------------------------------------------------------------------
# Distribution catalogue (must mirror the selectInputs in ui.R).
# ---------------------------------------------------------------------------
CONTINUOUS_DISTS <- c("Normal", "Uniform", "LogNormal", "Exponential", "Gamma",
                      "t", "Beta", "Cauchy", "HalfCauchy", "InverseGamma",
                      "InverseChiSquared", "LogitNormal")
DISCRETE_DISTS   <- c("Bernoulli", "BetaBinomial", "Binomial", "DiscreteUniform",
                      "NegativeBinomial", "Poisson")
MULTIVARIATE_DISTS <- c("Dirichlet", "InverseWishart", "LKJ", "Multinomial",
                        "MultivariateNormal", "MultivariateT", "Wishart")

# ---------------------------------------------------------------------------
# Default slider values, copied verbatim from ui.R. Every field is always
# present so that functions referencing "the other category's" inputs (e.g.
# `if (input$dist == "Normal")` inside the C++ generator) never hit a
# zero-length condition.
# ---------------------------------------------------------------------------
.DEFAULTS <- list(
  n = 10,
  normal_mu = 0, normal_sigma = 1,
  uniform_a = 0, uniform_b = 1,
  lognormal_mu = 0, lognormal_sigma = 1,
  exponential_rate = 0.5,
  gamma_shape = 1, gamma_rate = 0.5,
  t_mu = 0, t_sigma = 1, t_nu = 3,
  beta_a = 1, beta_b = 1,
  cauchy_location = 0, cauchy_scale = 1,
  halfcauchy_location = 0, halfcauchy_scale = 1,
  inversegamma_shape = 2, inversegamma_scale = 1,
  inversechisquared_df = 3,
  logitnormal_mu = 1, logitnormal_sigma = 1,
  bernoulli_prob = 0.5,
  betabinomial_size = 10, betabinomial_shape1 = 1, betabinomial_shape2 = 1,
  binomial_size = 10, binomial_prob = 0.5,
  discreteuniform_lower = 0, discreteuniform_upper = 1,
  poisson_lambda = 10, poisson_range = 40,
  negativebinomial_mean = 10, negativebinomial_dispersion = 3, negativebinomial_range = 40,
  multivariatenormal_mux = 0, multivariatenormal_muy = 0,
  multivariatenormal_sigmax = 1, multivariatenormal_sigmay = 1,
  multivariatenormal_rho = 0, multivariatenormal_range = 10,
  multivariatet_mux = 0, multivariatet_muy = 0,
  multivariatet_sigmax = 1, multivariatet_sigmay = 1,
  multivariatet_rho = 0, multivariatet_df = 10, multivariatet_range = 10,
  wishart_dimension = 4, wishart_df = 8, wishart_samplesize = 5000,
  inversewishart_dimension = 4, inversewishart_df = 8, inversewishart_samplesize = 5000,
  dirichlet_dimension = 2, dirichlet_samplesize = 1000,
  dirichlet_alpha1 = 2, dirichlet_alpha2 = 2, dirichlet_alpha3 = 2, dirichlet_alpha4 = 2,
  multinomial_angle = 100, multinomial_size = 6,
  multinomial_prob1 = 0.5, multinomial_prob2 = 0.5, multinomial_prob3 = 0.5,
  lkj_dimension = 4, lkj_eta = 1, lkj_samplesize = 2000,
  # selection + non-slider inputs (overwritten by make_input)
  distType = "Continuous", dist = "Normal", dist1 = "Bernoulli",
  dist2 = "MultivariateNormal", property = "pdf", language = "R"
)

# Build a complete `input` list selecting one distribution.
#   category: "Continuous" | "Discrete" | "Multivariate"
#   name:     the distribution name as used in ui.R
make_input <- function(category, name, property = "pdf") {
  inp <- .DEFAULTS
  inp$distType <- category
  if (category == "Continuous") {
    inp$dist <- name
  } else if (category == "Discrete") {
    inp$dist1 <- name
  } else {
    inp$dist2 <- name
  }
  inp$property <- property
  inp
}

# Flatten a Shiny tag / tagList to its visible text, stripping HTML tags and
# entities. Adequate for non-emptiness checks, but lossy: it also removes code
# operators such as `<-`, `>=` and `<`. Do NOT use it to record snapshots.
render_text <- function(x) {
  s <- paste(as.character(x), collapse = "")
  s <- gsub("<[^>]*>", "", s)                 # strip HTML tags
  s <- gsub("&[a-zA-Z]+;|&#[0-9]+;", " ", s)  # strip HTML entities
  trimws(s)
}

# Faithful text extraction for snapshots. The code generators wrap their output
# in a single <pre><code ...>CODE</code></pre> block; we return CODE verbatim so
# that operators like `<-` and `>=` survive. For output without a <code> block
# (e.g. LaTeX), we fall back to the full rendered string, which is still stable.
code_block_text <- function(x) {
  s <- paste(as.character(x), collapse = "\n")
  m <- regmatches(s, regexpr("(?s)<code[^>]*>.*</code>", s, perl = TRUE))
  if (length(m) == 1L) {
    s <- sub("(?s)^<code[^>]*>", "", m, perl = TRUE)
    s <- sub("(?s)</code>.*$", "", s, perl = TRUE)
  }
  # decode the handful of HTML entities the generators emit (mainly C++)
  s <- gsub("&lt;", "<", s, fixed = TRUE)
  s <- gsub("&gt;", ">", s, fixed = TRUE)
  s <- gsub("&amp;", "&", s, fixed = TRUE)
  s <- gsub("&emsp;", "  ", s, fixed = TRUE)
  trimws(s)
}

# A relative+absolute closeness assertion, suited to Monte Carlo comparisons
# where the expected value may be near zero.
expect_close <- function(observed, expected, tol, info = NULL) {
  testthat::expect_lt(abs(observed - expected), tol, label = info)
}
