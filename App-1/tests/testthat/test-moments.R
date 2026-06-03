# ---------------------------------------------------------------------------
# Analytic mean / variance vs Monte Carlo.
#
# For each univariate distribution we draw a large sample using the SAME
# parameterisation the app advertises, then compare the sample moments to the
# values returned by fCalculateMeanFull() / fCalculateVarianceFull().
#
# Each sampler below was independently cross-checked against the analytic
# formulae in a separate (scipy/numpy) oracle. That oracle flagged one genuine
# bug: the LogNormal variance in functions.R is written as
#     exp(sigma^2 - 1) * exp(2*mu + sigma^2)
# but should be
#     (exp(sigma^2) - 1) * exp(2*mu + sigma^2).
# This test therefore FAILS on LogNormal variance until that line is fixed —
# that failure is the point, not a flaky test.
# ---------------------------------------------------------------------------

# Samplers use the app's parameterisation. `check_var = FALSE` is set only
# where a Monte Carlo variance is statistically unreliable (Student-t with
# nu = 3 has infinite kurtosis, so the sample variance does not concentrate).
moment_specs <- list(
  Normal = list(
    category = "Continuous",
    sampler  = function(n) rnorm(n, 0, 1)
  ),
  Uniform = list(
    category = "Continuous",
    sampler  = function(n) runif(n, 0, 1)
  ),
  LogNormal = list(
    category = "Continuous",
    sampler  = function(n) rlnorm(n, 0, 1)
  ),
  Exponential = list(
    category = "Continuous",
    sampler  = function(n) rexp(n, rate = 0.5)
  ),
  Gamma = list(
    category = "Continuous",
    sampler  = function(n) rgamma(n, shape = 1, rate = 0.5)
  ),
  t = list(
    category  = "Continuous",
    sampler   = function(n) 0 + 1 * rt(n, df = 3),  # location-scale t
    check_var = FALSE
  ),
  Beta = list(
    category = "Continuous",
    sampler  = function(n) rbeta(n, 1, 1)
  ),
  InverseGamma = list(
    category = "Continuous",
    # X ~ InvGamma(shape, scale)  <=>  1/X ~ Gamma(shape, rate = scale)
    sampler  = function(n) 1 / rgamma(n, shape = 2, rate = 1)
  ),
  InverseChiSquared = list(
    category = "Continuous",
    # LaplacesDemon scaled inverse chi-squared with default scale = 1/df
    sampler  = function(n) 1 / rchisq(n, df = 3)
  ),
  LogitNormal = list(
    category = "Continuous",
    sampler  = function(n) plogis(rnorm(n, 1, 1))
  ),
  Bernoulli = list(
    category = "Discrete",
    sampler  = function(n) rbinom(n, 1, 0.5)
  ),
  BetaBinomial = list(
    category = "Discrete",
    sampler  = function(n) rbinom(n, 10, rbeta(n, 1, 1))
  ),
  Binomial = list(
    category = "Discrete",
    sampler  = function(n) rbinom(n, 10, 0.5)
  ),
  DiscreteUniform = list(
    category = "Discrete",
    sampler  = function(n) sample(0:1, n, replace = TRUE)
  ),
  Poisson = list(
    category = "Discrete",
    sampler  = function(n) rpois(n, 10)
  ),
  NegativeBinomial = list(
    category = "Discrete",
    sampler  = function(n) rnbinom(n, mu = 10, size = 3)
  )
  # Cauchy and HalfCauchy are intentionally omitted: the app correctly
  # reports NA mean/variance for them (undefined), so there is nothing
  # to compare against.
)

test_that("analytic mean and variance match Monte Carlo samples", {
  set.seed(20240501)
  N <- 2e6

  for (nm in names(moment_specs)) {
    spec <- moment_specs[[nm]]
    inp  <- make_input(spec$category, nm)
    samp <- spec$sampler(N)

    an_mean <- fCalculateMeanFull(inp)
    an_var  <- fCalculateVarianceFull(inp)

    # Mean
    if (!is.null(an_mean) && !is.na(an_mean)) {
      tol <- 0.02 * abs(an_mean) + 0.03
      expect_close(mean(samp), an_mean, tol,
                   info = paste0(nm, ": mean (analytic=", round(an_mean, 4), ")"))
    }

    # Variance
    check_var <- is.null(spec$check_var) || isTRUE(spec$check_var)
    if (check_var && !is.null(an_var) && !is.na(an_var)) {
      tol <- 0.06 * abs(an_var) + 0.06
      expect_close(var(samp), an_var, tol,
                   info = paste0(nm, ": variance (analytic=", round(an_var, 4), ")"))
    }
  }
})
