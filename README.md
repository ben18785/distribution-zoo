# The distribution zoo
App to view distribution properties and access dynamic code in R, Python, Matlab, Mathematica and Stan. The app is available online <a href="https://ben18785.shinyapps.io/distribution-zoo/" target="_blank">here</a>.

## To add a new distribution

- Add distribution to relevant conditional panel at top of `ui.R`.
- Add slider inputs for the parameter values in `ui.R` and, if necessary, range parameters that will determine plotting range.
- Add the mean and variance in `fCalculateMeanFull` and `fCalculateVarianceFull` within `functions.R`.
- Add arguments in one of the `fExtra[]FunctionInputsFull` functions that will be passed to the function to call it (for plotting) to `plotting.R`. For example, the normal distribution is parameterised by two named arguments in R's implementation `mean` and `sd`. So to plot this distribution as we vary the user-selected parameters `input$normal_mu` and `input$normal_sigma`, we need to make a named argument call of the form `paste("mean=",input$normal_mu,",sd=",input$normal_sigma)`, which is pasted than evaluated to plot.
- Add a scale for discrete distributions in `fScaleFull1` or continuous distributions in `fScaleFull` in `plotting.R`.
- Add function to call to evaluate PDF/PMF in the `data <- reactive({` function in `server.R`. Note, if this function does not exist in base R or within a package, you can define a custom one within the `functions.R` file. Note the named arguments to the function must the same as that specified in the above step which takes place in `plotting.R`.
- Add function to call to evaluate CDF in the `dataCDF <- reactive({` function in `server.R`. Note, see above for custom functions.
- Add formulae in latex form in `formulae.R`.
- Copy code from `formulae.R` and put into correct form using helper functions (`fLatexHelper_`) where possible which aid with the construction of code that renders nicely in the app.
- Add code for R, Python, Mathematica, Matlab and Stan by updating the relevant `code_` files. Note that the code is intended to be dynamic and the properties shown on the plots should be exactly replicated by the code examples. This means that the parameterisation in the function call may not reflect the default parameterisation.
- Add example likelihood and prior uses for the distribution in `example_users.R`.
