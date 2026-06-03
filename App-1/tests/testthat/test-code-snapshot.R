# ---------------------------------------------------------------------------
# Snapshot tests for generated code.
#
# Code generation is deterministic given an input, so we dump every
# (distribution x language x property) combination to a single stable string
# and snapshot it. The first run records the baseline in _snaps/; thereafter
# any change to generated output shows up as a reviewable diff and must be
# accepted deliberately with testthat::snapshot_accept().
#
# This guards the README's promise that the displayed code matches the plotted
# distribution: if a refactor silently changes a generated call, the snapshot
# catches it.
# ---------------------------------------------------------------------------

# Distribution -> category map, in a fixed order for stable output.
.snapshot_catalogue <- c(
  setNames(rep("Continuous",   length(CONTINUOUS_DISTS)),   CONTINUOUS_DISTS),
  setNames(rep("Discrete",     length(DISCRETE_DISTS)),     DISCRETE_DISTS),
  setNames(rep("Multivariate", length(MULTIVARIATE_DISTS)), MULTIVARIATE_DISTS)
)

.snapshot_generators <- list(
  R           = function(inp) fRcode(inp),
  Python      = function(inp) fPythoncode(inp),
  Stan        = function(inp) fStanCode(inp),
  Matlab      = function(inp) fMatlabcode(inp),
  Mathematica = function(inp) fMathematicacode(inp),
  Julia       = function(inp) fJuliacode(inp),
  LaTeX       = function(inp) fLatex(inp)
)

# Build the full deterministic dump as a character vector of lines.
dump_all_code <- function(properties = "pdf") {
  lines <- character(0)
  for (nm in names(.snapshot_catalogue)) {
    category <- .snapshot_catalogue[[nm]]
    for (prop in properties) {
      inp <- make_input(category, nm, property = prop)
      for (lang in names(.snapshot_generators)) {
        txt <- render_text(.snapshot_generators[[lang]](inp))
        lines <- c(lines,
                   sprintf("===== %s | %s | %s =====", nm, lang, prop),
                   txt, "")
      }
    }
  }
  lines
}

test_that("generated code (pdf) is stable across all distributions and languages", {
  expect_snapshot(cat(dump_all_code("pdf"), sep = "\n"))
})

test_that("generated R/Python/Stan code is stable across all properties", {
  # Limit to the three languages and the univariate distributions to keep the
  # snapshot focused on the property-dependent (pdf / log_pdf / random) paths.
  catalogue <- c(
    setNames(rep("Continuous", length(CONTINUOUS_DISTS)), CONTINUOUS_DISTS),
    setNames(rep("Discrete",   length(DISCRETE_DISTS)),   DISCRETE_DISTS)
  )
  gens <- .snapshot_generators[c("R", "Python", "Stan")]

  dump <- character(0)
  for (nm in names(catalogue)) {
    category <- catalogue[[nm]]
    for (prop in c("pdf", "log_pdf", "random")) {
      inp <- make_input(category, nm, property = prop)
      for (lang in names(gens)) {
        txt <- render_text(gens[[lang]](inp))
        dump <- c(dump, sprintf("===== %s | %s | %s =====", nm, lang, prop), txt, "")
      }
    }
  }
  expect_snapshot(cat(dump, sep = "\n"))
})
