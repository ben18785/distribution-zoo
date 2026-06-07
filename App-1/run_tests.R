#!/usr/bin/env Rscript
# Run the distribution-zoo test suite.
#
# Usage (from the App-1 directory):
#   Rscript run_tests.R
#
# This app is not structured as an R package, so we point testthat at the
# test directory directly rather than using test_check().

library(testthat)

# testthat skips snapshot tests when it thinks it's on CRAN, which it assumes
# for any non-interactive session unless NOT_CRAN=true. Set it so the snapshot
# baseline is generated when running this script.
Sys.setenv(NOT_CRAN = "true")

app_dir   <- normalizePath(dirname(sub("--file=", "",
              grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])))
if (is.na(app_dir) || length(app_dir) == 0) app_dir <- getwd()

test_dir <- file.path(app_dir, "tests", "testthat")
if (!dir.exists(test_dir)) test_dir <- file.path(getwd(), "tests", "testthat")

cat("Running tests in:", test_dir, "\n\n")
testthat::test_dir(test_dir, stop_on_failure = FALSE)
