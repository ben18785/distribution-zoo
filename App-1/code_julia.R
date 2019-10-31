fJuliaHelper <- function(name, input, params){
    if(input$property=="pdf")
      fMakeFunctionPaste(mainName=paste0("pdf(", input$dist),
                         params=paste(params, sep = ", "),
                         postfixparams="x",
                         julia=TRUE)
    else if(input$property=="log_pdf")
      fMakeFunctionPaste(mainName=paste0("logpdf(", input$dist),
                         params=paste(params, sep = ", "),
                         postfixparams="x",
                         julia=TRUE)
    else if(input$property=="random")
      fMakeFunctionPaste(mainName=paste0("rand(", input$dist),
                         params=paste(params, sep = ", "),
                         postfixparams="n",
                         julia=TRUE)
}

fJuliaHelperDiscrete <- function(name, input, params){
  if(input$property=="pdf")
    fMakeFunctionPaste(mainName=paste0("pdf(", input$dist1),
                       params=paste(params, sep = ", "),
                       postfixparams="x",
                       julia=TRUE)
  else if(input$property=="log_pdf")
    fMakeFunctionPaste(mainName=paste0("logpdf(", input$dist1),
                       params=paste(params, sep = ", "),
                       postfixparams="x",
                       julia=TRUE)
  else if(input$property=="random")
    fMakeFunctionPaste(mainName=paste0("rand(", input$dist1),
                       params=paste(params, sep = ", "),
                       postfixparams="n",
                       julia=TRUE)
}

fTCustomJulia <- function(input){
  if(input$property=="pdf")
    paste0("pdf(TDist(", input$t_nu, "), ", "(x - ", input$t_mu, ") / ", input$t_sigma, ") / ", input$t_sigma)
  else if(input$property=="log_pdf")
    paste0("logpdf(TDist(", input$t_nu, "), ", "(x - ", input$t_mu, ") / ", input$t_sigma, ") - log(", input$t_sigma, ")")
  else if(input$property=="random")
    paste0(input$t_mu, " .+ ", input$t_sigma, " .* ", "rand(TDist(", input$t_nu, ")", ", n)")
}

fTruncatedCauchyJulia <- function(input){
  preamble <- paste0("d=Truncated(Cauchy(", input$halfcauchy_location, ", ", input$halfcauchy_scale, ")", ", 0, Inf)")
  switch(input$property,
         pdf=paste(preamble, "pdf(d, x)", sep = "\n"),
         log_pdf=paste(preamble, "logpdf(d, x)", sep = "\n"),
         random=paste(preamble, "rand(d, n)", sep = "\n"))
}

fInverseChiSquaredJulia <- function(input){
  switch(input$property,
         pdf=paste0("pdf(InverseGamma(", input$inversechisquared_df, " / 2, 1 / 2), x)"),
         log_pdf=paste0("logpdf(InverseGamma(", input$inversechisquared_df, " / 2, 1 / 2), x)"),
         random=paste0("rand(InverseGamma(", input$inversechisquared_df, " / 2, 1 / 2), n)"))
}

dMultivariateNormalJulia <- function(mux, muy, sigmax, sigmay, rho){
  top <- paste0("aDist=MvNormal(float([", mux, ", ", muy, "]), float([[", sigmax^2, " ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, " ",  
                sigmay^2, "]]))")
}


fMultivariatenormalJulia <- function(input){
  top <- dMultivariateNormalJulia(input$multivariatenormal_mux,
                                         input$multivariatenormal_muy,
                                         input$multivariatenormal_sigmax,
                                         input$multivariatenormal_sigmay,
                                         input$multivariatenormal_rho)
  switch(input$property,
         pdf=paste(top, "pdf(aDist, x)", sep = "\n"),
         log_pdf=paste(top, "logpdf(aDist, x)", sep = "\n"),
         random=paste(top, "rand(aDist, n)", sep = "\n")
  )
}

dMultivariatetJulia <- function(mux, muy, sigmax, sigmay, rho, df){
  top <- paste0("aDist=MvTDist(", df, ", [", mux, ", ", muy, "], float([[", sigmax^2, " ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, " ",  
                sigmay^2, "]]))")
}

fMultivariatetJulia <- function(input){
  top <- dMultivariatetJulia(input$multivariatet_mux,
                                    input$multivariatet_muy,
                                    input$multivariatet_sigmax,
                                    input$multivariatet_sigmay,
                                    input$multivariatet_rho,
                                    input$multivariatet_df)
  switch(input$property,
         pdf=paste(top, "pdf(aDist, x)", sep = "\n"),
         log_pdf=paste(top, "logpdf(aDist, x)", sep = "\n"),
         random=paste(top, "rand(aDist, n)", sep = "\n")
  )
}

fMultinomialJulia <- function(input){
  comps <- c(input$multinomial_prob1, input$multinomial_prob2, input$multinomial_prob3)
  comps <- comps / sum(comps)
  top <- paste0("aDist=Multinomial(", input$multinomial_size, ", ", "[", comps[1], ", ",
                comps[2], ", ", comps[3], "])")
  switch(input$property,
         pdf=paste(top, "pdf(aDist, x)", sep = "\n"),
         log_pdf=paste(top, "logpdf(aDist, x)", sep = "\n"),
         random=paste(top, "rand(aDist, n)", sep = "\n")
  )
}

fWishartJulia <- function(input){
  top <- paste0("aDist=Wishart(", input$wishart_df, ", S)")
  switch(input$property,
         pdf=paste(top, "pdf(aDist, x)", sep = "\n"),
         log_pdf=paste(top, "logpdf(aDist, x)", sep = "\n"),
         random=paste(top, "rand(aDist, n)", sep = "\n")
  )
}

fInverseWishartJulia <- function(input){
  top <- paste0("aDist=InverseWishart(", input$inversewishart_df, ", S)")
  switch(input$property,
         pdf=paste(top, "pdf(aDist, x)", sep = "\n"),
         log_pdf=paste(top, "logpdf(aDist, x)", sep = "\n"),
         random=paste(top, "rand(aDist, n)", sep = "\n")
  )
}

fDirichletMathematica <- function(input){
  if(input$dirichlet_dimension==2){
    alpha <- c(input$dirichlet_alpha1, input$dirichlet_alpha2)
  }else if(input$dirichlet_dimension==3){
    alpha <- c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3)
  }else if(input$dirichlet_dimension==4){
    alpha <- c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3, input$dirichlet_alpha4)
  }
  top <- paste0("Dirichlet([", paste(alpha, collapse = ", "), "])")
  switch(input$property,
         pdf=paste(paste0("pdf(", top, ", x)"), sep="\n"),
         log_pdf=paste0("logpdf(", top, ", x)"),
         random=paste(paste0("rand(", top, ", n)"), sep="\n")
  )
}

dLKJJulia <- paste(
  "using LinearAlgebra",
  "using GSL",
  "function LKJ_pdf(X, nu)",
  "    d = size(X)[1]",
  "    if sum(Diagonal(X)) != d || sum(LowerTriangular(X)) != sum(UpperTriangular(X)) || !isposdef(X) || nu < 0",
  "        return 0.0;",
  "    end",
  "",
  "    a_sum = 0.0;",
  "    a_prod = 1.0;",
  "    for k = 1:(d - 1)",
  "        a_sum += (2 * nu - 2 + d - k) * (d - k);",
  "        a_prod *= sf_beta(nu + 0.5 * (d - k - 1), nu + 0.5 * (d - k - 1));",
  "    end",
  "    a_sum = 2^a_sum;",
  "    return a_sum * a_prod * det(X)^(nu - 1);",
  "end",
  sep = "\n"
)

dLKJJulia_log <- paste(
  "using LinearAlgebra",
  "using GSL",
  "function LKJ_pdf(X, nu)",
  "    d = size(X)[1]",
  "    if sum(Diagonal(X)) != d || sum(LowerTriangular(X)) != sum(UpperTriangular(X)) || !isposdef(X) || nu < 0",
  "        return 0.0;",
  "    end",
  "",
  "    a_sum = 0.0;",
  "    a_prod = 1.0;",
  "    for k = 1:(d - 1)",
  "        a_sum += (2 * nu - 2 + d - k) * (d - k);",
  "        a_prod *= sf_beta(nu + 0.5 * (d - k - 1), nu + 0.5 * (d - k - 1));",
  "    end",
  "    a_sum = 2^a_sum;",
  "    return log(a_sum) + log(a_prod) + (nu - 1) * log(det(X));",
  "end",
  sep = "\n"
)

rLKJJulia <- paste(
  "# use with caution as have disabled posdef matrix checking for Cholesky decomp",
  "using Distributions",
  "using LinearAlgebra",
  "function LKJ_rand_1(nu, d)",
  "    @assert nu > 0 && d > 0",
  "    d = Int(d)",
  "    if d == 1",
  "        r = [1];",
  "    elseif d == 2",
  "        rho = 2 * rand(Beta(nu, nu), 1)[1] - 1;",
  "        r = [[1 rho]; [rho 1]];",
  "    else",
  "        beta = nu + (d - 2) / 2;",
  "        u = rand(Beta(beta, beta), 1)[1];",
  "        r_12 = 2 * u - 1;",
  "        r = [[1 r_12]; [r_12 1]];",
  "        for m = 2:(d - 1)",
  "            beta -= 0.5;",
  "            y = rand(Beta(m / 2.0, beta), 1)[1];",
  "            a = rand(Normal(0, 1), m);",
  "            anorm = sqrt(sum(a.^2));",
  "            u = a ./ anorm;",
  "            w = sqrt(y) .* u;",
  "            A = cholesky(Hermitian(r), check=false).U;",
  "            z = A * w;",
  "            r = [r z; transpose(z) 1];",
  "        end",
  "    end",
  "    return r;",
  "end",
  "",
  "function LKJ_rand(n, nu, d)",
  "    r_list = []",
  "    for i = 1:n",
  "        push!(r_list, LKJ_rand_1(nu, d))",
  "    end",
  "    return r_list;",
  "end",
  sep = "\n"
)

fLKJJulia <- function(input){
  switch(input$property,
         pdf=paste(dLKJJulia,
                   paste0("LKJ_pdf(x, ", input$lkj_eta, ")"),
                   sep = "\n"),
         log_pdf=paste(dLKJJulia_log,
                       paste0("LKJ_logpdf(x, ", input$lkj_eta, ")"),
                       sep = "\n"),
         random=paste(rLKJJulia,
                      paste0("LK_rand(n, ", input$lkj_eta, ", ", input$lkj_dimension, "]"),
                      sep = "\n")
  )
}


fJuliacode <- function(input){
  text <- 
    if(input$distType=='Continuous'){
      switch(input$dist,
              Normal=fJuliaHelper("Normal", input, c(input$normal_mu, input$normal_sigma)),
              Uniform=fJuliaHelper("Uniform", input, c(input$uniform_a, input$uniform_b)),
              LogNormal=fJuliaHelper("LogNormal", input, c(input$lognormal_mu, input$lognormal_sigma)),
              Exponential=fJuliaHelper("Exponential", input, c(1 / input$exponential_rate)),
              Gamma=fJuliaHelper("Gamma", input, c(input$gamma_shape, 1 / input$gamma_rate)),
              t=fTCustomJulia(input),
              Beta=fJuliaHelper("Beta", input, c(input$beta_a, input$beta_b)),
              Cauchy=fJuliaHelper("Cauchy", input, c(input$cauchy_location,input$cauchy_scale)),
              HalfCauchy=fTruncatedCauchyJulia(input),
              InverseGamma=fJuliaHelper("InverseGamma", input, c(input$inversegamma_shape, input$inversegamma_scale)),
              InverseChiSquared=fInverseChiSquaredJulia(input),
              LogitNormal=fJuliaHelper("LogitNormal", input, c(input$logitnormal_mu, input$logitnormal_sigma)),
              "Coming soon.")
    }else if(input$distType=='Discrete'){
       switch(input$dist1,
              Bernoulli=fJuliaHelperDiscrete("Bernoulli", input, c(input$bernoulli_prob)),
              BetaBinomial=fJuliaHelperDiscrete("BetaBinomial", input, c(input$betabinomial_size, input$betabinomial_shape1, input$betabinomial_shape2)),
              Binomial=fJuliaHelperDiscrete("Binomial", input, c(input$binomial_size, input$binomial_prob)),
              DiscreteUniform=fJuliaHelperDiscrete("DiscreteUniform", input, c(input$discreteuniform_lower, input$discreteuniform_upper)),
              NegativeBinomial=fJuliaHelperDiscrete("NegativeBinomial", input, c(input$negativebinomial_dispersion, input$negativebinomial_dispersion / (input$negativebinomial_dispersion + input$negativebinomial_mean))),
              Poisson=fJuliaHelperDiscrete("Poisson", input, c(input$poisson_lambda)),
              "Coming soon.")
    }else if(input$distType=='Multivariate'){
      switch(input$dist2,
             MultivariateNormal=fMultivariatenormalJulia(input),
             MultivariateT=fMultivariatetJulia(input),
             Multinomial=fMultinomialJulia(input),
             Wishart=paste(paste0("# S must be symmetric and positive definite and of size ",
                                  input$wishart_dimension, "x", input$wishart_dimension),
                                  fWishartJulia(input), sep="\n"),
             InverseWishart=paste(paste0("# S must be symmetric and positive definite and of size ",
                                         input$inversewishart_dimension, "x", input$inversewishart_dimension),
                                  fInverseWishartJulia(input), sep="\n"),
             Dirichlet=fDirichletMathematica(input),
             LKJ=fLKJJulia(input),
             "Coming soon.")
    }
  
  if(text!="Coming soon."){
  tagList(prismCodeBlock(paste0("using Random, Distributions\n",
                                text), language = "julia"),
          h3("Note that code assumes that 'Compat' and 'Distributions' packages are installed by typing:"),
          prismCodeBlock(paste("Pkg.add(\"Compat\")",
                               "Pkg.add(\"Distributions\")",
                               sep = "\n"), language = "julia"),
          h3("at Julia command line."))
  }else{
      tagList(h3(text))
    }
}
