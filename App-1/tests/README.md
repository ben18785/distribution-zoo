# Distribution-zoo tests

A first `testthat` suite for the app's core logic. The app isn't an R package,
so the tests source the app's `.R` files directly and reconstruct the Shiny
`input` list as a plain R list (see `testthat/helper-setup.R`).

## Running

From the `App-1/` directory:

```sh
Rscript run_tests.R
```

or, interactively in R with the working directory at `App-1/`:

```r
testthat::test_dir("tests/testthat")
```

## What's covered

| File | What it checks |
|------|----------------|
| `test-moments.R` | Analytic mean/variance from `fCalculateMeanFull`/`fCalculateVarianceFull` match a large Monte Carlo sample drawn with the app's parameterisation. |
| `test-coverage.R` | Every distribution in the UI produces non-empty formulae, LaTeX, and code for R/Python/Stan/Matlab/Mathematica/Julia; univariate distributions return a single finite-or-NA mean and variance. Documents the C++ generator's Normal-only support. |
| `test-code-snapshot.R` | Snapshots all generated code (every distribution x language x property) so any change to generated output appears as a reviewable diff. |
| `test-plots.R` | `fPlotPDF`/`fPlotCDF` build without error and yield non-empty data for all univariate distributions, plus the bivariate normal and t PDFs. |

## Dependencies

The suite needs the app's own packages. At minimum: `testthat`, `shiny`,
`purrr`. The plot tests additionally use `ggplot2`, `LaplacesDemon`, `actuar`,
`logitnorm`, `mvtnorm`, `reshape` and `skip()` themselves if a package is
missing.

## Snapshots (generate the baseline locally before the first push)

`test-code-snapshot.R` records the generated code for every distribution to
`tests/testthat/_snaps/`. The comparison only has teeth once that baseline is
committed to the repo, so generate it locally first rather than letting CI do
it.

**Why local, not CI:** on the very first run with no `_snaps/` present, testthat
*writes* the baseline and the test passes (there's nothing to compare against
yet). A fresh CI checkout has no `_snaps/`, so CI would silently regenerate the
baseline every run and never detect drift. Committing the baseline from your
machine is what turns the snapshot test into a real guard.

### One-time setup

From the `App-1/` directory, with the app's R dependencies installed:

```sh
Rscript run_tests.R
```

(or, in an R session with the working directory at `App-1/`:
`testthat::test_dir("tests/testthat")`).

This creates:

```
App-1/tests/testthat/_snaps/
  code-snapshot.md        # the recorded code for every distribution / language
```

Open that file, sanity-check that the generated code looks right, then commit
the whole `_snaps/` directory:

```sh
git add App-1/tests/testthat/_snaps
git commit -m "Add code-generation snapshot baseline"
```

### Day-to-day workflow

- A change that alters generated code makes the snapshot test **fail**, showing
  a diff of exactly what changed.
- If the change was **unintentional**, you've caught a regression — fix the code.
- If the change was **intentional** (e.g. you improved a code template), accept
  the new baseline and commit it:

  ```r
  testthat::snapshot_accept()   # run from App-1/, or pass the test file path
  ```

  ```sh
  git add App-1/tests/testthat/_snaps
  git commit -m "Update code snapshots after <change>"
  ```

### Don't ignore the snapshots

Make sure `_snaps/` is **not** covered by `.gitignore` — it is source-controlled
data, not build output. (testthat also writes `*.new.md` files next to a failed
snapshot; those *are* throwaway and can be ignored or deleted.)

## A bug this suite already caught and fixed

When first written, `test-moments.R` failed on the LogNormal variance. In
`functions.R` the variance had been written as

```r
exp(input$lognormal_sigma^2 - 1) * exp(...)      # wrong: -1 inside the exp
```

when it should be

```r
(exp(input$lognormal_sigma^2) - 1) * exp(...)    # correct
```

At mu=0, sigma=1 the app reported a variance of 2.72; the true value is 4.67,
so the "sd" shown under every LogNormal plot was understated. The line has
been corrected and the test now passes — it stays in place as a regression
guard.
