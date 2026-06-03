# ---------------------------------------------------------------------------
# Plot-rendering tests.
#
# These reproduce the wiring in server.R's renderPlot()/renderPlot(plotCDF)
# blocks, call fPlotPDF()/fPlotCDF(), and then force full evaluation of the
# returned ggplot via ggplot_build(). A distribution "plots" if it builds
# without error and yields a non-empty data layer.
#
# Scope: all univariate distributions (PDF + CDF) plus the two analytic
# bivariate cases (MultivariateNormal, MultivariateT) for the PDF. The
# sampling-based multivariate plots (Wishart, InverseWishart, Dirichlet,
# Multinomial, LKJ) are heavier / stochastic and are left as a follow-up.
# ---------------------------------------------------------------------------

# PDF density-function dispatch, mirroring server.R's data() reactive.
.pdf_fun <- function(inp) {
  if (inp$distType == "Continuous") {
    switch(inp$dist,
           Normal = dnorm, Uniform = dunif, LogNormal = dlnorm, Exponential = dexp,
           Gamma = dgamma, t = dst, Beta = dbeta, Cauchy = dcauchy,
           HalfCauchy = dCustomHalfCauchy, InverseGamma = dinvgamma,
           InverseChiSquared = dCustomInverseChiSquared, LogitNormal = dlogitnorm,
           dnorm)
  } else if (inp$distType == "Discrete") {
    switch(inp$dist1,
           Bernoulli = dbern, BetaBinomial = dCustomBetaBinomial, Binomial = dbinom,
           DiscreteUniform = dunifdisc, Poisson = dpois, NegativeBinomial = dnbinom,
           dbern)
  } else {
    NULL  # multivariate PDF is computed inside fPlotPDF, not via this function
  }
}

# CDF dispatch, mirroring server.R's dataCDF() reactive.
.cdf_fun <- function(inp) {
  if (inp$distType == "Continuous") {
    switch(inp$dist,
           Normal = pnorm, Uniform = punif, LogNormal = plnorm, Exponential = pexp,
           Gamma = pgamma, t = pst, Beta = pbeta, Cauchy = pcauchy,
           HalfCauchy = pCustomHalfCauchy, InverseGamma = pinvgamma,
           InverseChiSquared = pCustomInverseChiSquared, LogitNormal = plogitnorm,
           pnorm)
  } else if (inp$distType == "Discrete") {
    switch(inp$dist1,
           Bernoulli = pbern, Binomial = pbinom, DiscreteUniform = punifdisc,
           Poisson = ppois, NegativeBinomial = pnbinom, BetaBinomial = pCustomBetaBinomial,
           pbern)
  } else {
    NULL
  }
}

# Multivariate plotting range, mirroring server.R's fScaleMVR().
.mv_scale <- function(inp) {
  if (inp$dist2 == "MultivariateNormal") {
    seq(-inp$multivariatenormal_range, inp$multivariatenormal_range,
        2 * inp$multivariatenormal_range / 100)
  } else {
    seq(-inp$multivariatet_range, inp$multivariatet_range,
        2 * inp$multivariatenormal_range / 100)
  }
}

# Build a plot, capturing any error message. Returns list(built=, err=).
.try_build <- function(expr) {
  err <- NULL
  built <- tryCatch(ggplot2::ggplot_build(expr),
                    error = function(e) { err <<- conditionMessage(e); NULL })
  list(built = built, err = err)
}

.univariate_catalogue <- function() {
  c(setNames(rep("Continuous", length(CONTINUOUS_DISTS)), CONTINUOUS_DISTS),
    setNames(rep("Discrete",   length(DISCRETE_DISTS)),   DISCRETE_DISTS))
}

test_that("PDF plots build for every univariate distribution", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("LaplacesDemon")  # dst, dbern
  skip_if_not_installed("actuar")         # dinvgamma
  skip_if_not_installed("logitnorm")      # dlogitnorm
  suppressMessages({
    library(ggplot2); library(LaplacesDemon); library(actuar); library(logitnorm)
  })

  univ <- .univariate_catalogue()
  for (nm in names(univ)) {
    category <- univ[[nm]]
    inp   <- make_input(category, nm)
    aDist <- .pdf_fun(inp)
    aMean <- fCalculateMeanFull(inp)
    aVar  <- fCalculateVarianceFull(inp)
    if (category == "Continuous") {
      lScale <- fScaleFull(inp);  lExtra <- fExtraFunctionInputsFull(inp)
    } else {
      lScale <- fScaleFull1(inp); lExtra <- fExtra1FunctionInputsFull(inp)
    }

    res <- .try_build(fPlotPDF(inp, aDist, aMean, aVar, lScale, lExtra))
    expect_true(is.null(res$err),
                info = paste("PDF plot errored for", nm, "-", res$err))
    if (!is.null(res$built)) expect_gt(nrow(res$built$data[[1]]), 0)
  }
})

test_that("CDF plots build for every univariate distribution", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("LaplacesDemon")
  skip_if_not_installed("actuar")
  skip_if_not_installed("logitnorm")
  suppressMessages({
    library(ggplot2); library(LaplacesDemon); library(actuar); library(logitnorm)
  })

  univ <- .univariate_catalogue()
  for (nm in names(univ)) {
    category <- univ[[nm]]
    inp   <- make_input(category, nm)
    aDist <- .cdf_fun(inp)
    aMean <- fCalculateMeanFull(inp)
    aVar  <- fCalculateVarianceFull(inp)
    if (category == "Continuous") {
      lScale <- fScaleFull(inp);  lExtra <- fExtraFunctionInputsFull(inp)
    } else {
      lScale <- fScaleFull1(inp); lExtra <- fExtra1FunctionInputsFull(inp)
    }

    res <- .try_build(fPlotCDF(inp, aDist, aMean, aVar, lScale, lExtra))
    expect_true(is.null(res$err),
                info = paste("CDF plot errored for", nm, "-", res$err))
    if (!is.null(res$built)) expect_gt(nrow(res$built$data[[1]]), 0)
  }
})

test_that("PDF plots build for the bivariate normal and t", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("mvtnorm")  # dmvt used inside fPlotPDF
  skip_if_not_installed("reshape")  # melt used inside fPlotPDF
  suppressMessages({ library(ggplot2); library(mvtnorm); library(reshape) })

  for (nm in c("MultivariateNormal", "MultivariateT")) {
    inp    <- make_input("Multivariate", nm)
    lScale <- .mv_scale(inp)
    # aMean/aVar/lExtra are ignored by the multivariate branch of fPlotPDF
    res <- .try_build(fPlotPDF(inp, NULL, NA, -99, lScale, NULL))
    expect_true(is.null(res$err),
                info = paste("PDF plot errored for", nm, "-", res$err))
    if (!is.null(res$built)) expect_gt(nrow(res$built$data[[1]]), 0)
  }
})
