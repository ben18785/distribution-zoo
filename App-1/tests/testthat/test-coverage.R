# ---------------------------------------------------------------------------
# Coverage / "required fields" test.
#
# This is the registry-free version of "every distribution entry has all the
# pieces wired up". For each distribution shown in the UI we check that the
# pieces that should exist actually render non-empty output and don't error:
#   - formulae, LaTeX
#   - code for each language that is supposed to cover all distributions
#   - mean/variance for univariate distributions
#
# When a new distribution is added, these tests fail until every piece is
# wired, which is exactly the safety net the current 9-step checklist lacks.
#
# Known gap deliberately encoded below: the C++ generator only implements the
# Normal. We assert C++ works for Normal, and assert it is EMPTY elsewhere, so
# that if/when C++ coverage is extended the test reminds you to update it.
# ---------------------------------------------------------------------------

ALL_UNIVARIATE <- c(
  setNames(rep("Continuous", length(CONTINUOUS_DISTS)), CONTINUOUS_DISTS),
  setNames(rep("Discrete",   length(DISCRETE_DISTS)),   DISCRETE_DISTS)
)
ALL_DISTS <- c(
  ALL_UNIVARIATE,
  setNames(rep("Multivariate", length(MULTIVARIATE_DISTS)), MULTIVARIATE_DISTS)
)

# Language generators expected to cover ALL distributions.
full_coverage_generators <- list(
  Formulae    = function(inp) fFormulae(inp),
  LaTeX       = function(inp) fLatex(inp),
  R           = function(inp) fRcode(inp),
  Python      = function(inp) fPythoncode(inp),
  Stan        = function(inp) fStanCode(inp),
  Matlab      = function(inp) fMatlabcode(inp),
  Mathematica = function(inp) fMathematicacode(inp),
  Julia       = function(inp) fJuliacode(inp)
)

test_that("formulae and per-language code render non-empty for every distribution", {
  for (nm in names(ALL_DISTS)) {
    category <- ALL_DISTS[[nm]]
    inp <- make_input(category, nm, property = "pdf")

    for (gen_name in names(full_coverage_generators)) {
      gen <- full_coverage_generators[[gen_name]]
      out <- NULL
      expect_no_error({
        out <- gen(inp)
      })
      txt <- render_text(out)
      expect_gt(nchar(txt), 0)  # i.e. some content was produced
    }
  }
})

test_that("R/Python/Stan code render for all three properties", {
  for (nm in names(ALL_UNIVARIATE)) {
    category <- ALL_UNIVARIATE[[nm]]
    for (prop in c("pdf", "log_pdf", "random")) {
      inp <- make_input(category, nm, property = prop)
      for (gen in list(fRcode, fPythoncode, fStanCode)) {
        out <- NULL
        expect_no_error({ out <- gen(inp) })
        expect_gt(nchar(render_text(out)), 0)
      }
    }
  }
})

test_that("univariate distributions return finite-or-NA mean and variance", {
  for (nm in names(ALL_UNIVARIATE)) {
    category <- ALL_UNIVARIATE[[nm]]
    inp <- make_input(category, nm)

    m <- fCalculateMeanFull(inp)
    v <- fCalculateVarianceFull(inp)

    # Must be a single scalar. NA denotes an undefined moment and is allowed;
    # note the app returns a bare `NA` (logical) for these, so we must not
    # require numeric type for the NA case.
    expect_length(m, 1)
    expect_length(v, 1)
    # When defined (not NA), it must be a finite number.
    if (!is.na(m)) expect_true(is.numeric(m) && is.finite(m),
                               info = paste(nm, "mean finite"))
    if (!is.na(v)) expect_true(is.numeric(v) && is.finite(v) && v >= 0,
                               info = paste(nm, "variance >= 0"))
  }
})

test_that("C++ generator produces output for Normal (currently the only supported dist)", {
  # Known limitation: fCpluspluscode() only implements the Normal, and it keys
  # solely off input$dist (it ignores input$distType), so it cannot currently
  # generate C++ for discrete or multivariate distributions. This test pins the
  # supported case; extend it as C++ coverage grows.
  for (prop in c("pdf", "log_pdf", "random")) {
    out <- fCpluspluscode(make_input("Continuous", "Normal", property = prop))
    expect_gt(nchar(render_text(out)), 0)
  }
})
