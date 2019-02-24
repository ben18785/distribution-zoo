# The distribution zoo
App to view distribution properties and access dynamic code in R, Python, Matlab, Mathematica and Stan.

## To add a new distribution

- Add distribution to relevant conditional panel at top of `ui.R`.
- Add slider inputs for the parameter values in `ui.R`.
- Add the mean and variance in `fCalculateMeanFull` and `fCalculateVarianceFull` within `functions.R`.
