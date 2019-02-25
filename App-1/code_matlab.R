fMatlabHelper <- function(mainName, params, input, import=NULL, named_arguments=NULL, vector_params=FALSE, python_vector=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0(mainName, "pdf"),
                                params=params, prefixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params,
                                python_vector=python_vector),
         log_pdf=paste0(fMakeFunctionPaste(mainName=paste0("log(", mainName, "pdf"),
                                    params=params, prefixparams="x",
                                    import=import, named_arguments=named_arguments,
                                    vector_params=vector_params,
                                    python_vector=python_vector),
                        ")"),
         random=fMakeFunctionPaste(mainName=paste0(mainName, "rnd"),
                                   params=params, postfixparams="[n, 1]",
                                   import=import, named_arguments=named_arguments,
                                   vector_params=vector_params,
                                   python_vector=python_vector))
}

fMatlabHelper1 <- function(mainName, params, input, import=NULL, named_arguments=NULL, vector_params=FALSE, python_vector=FALSE){
  switch(input$property,
         pdf=fMakeFunctionPaste(mainName=paste0(mainName, "pdf"),
                                params=params, prefixparams="x",
                                import=import, named_arguments=named_arguments,
                                vector_params=vector_params,
                                python_vector=python_vector),
         log_pdf=paste0(fMakeFunctionPaste(mainName=paste0("log(", mainName, "pdf"),
                                           params=params, prefixparams="x",
                                           import=import, named_arguments=named_arguments,
                                           vector_params=vector_params,
                                           python_vector=python_vector),
                        ")"),
         random=fMakeFunctionPaste(mainName=paste0(mainName, "rnd"),
                                   params=params, postfixparams="n",
                                   import=import, named_arguments=named_arguments,
                                   vector_params=vector_params,
                                   python_vector=python_vector))
}

dStudentt_matlab <- paste(
  "function f = studenttpdf(x, mu, sigma, nu)",
  "    numer = (nu / (nu + ((x - mu) / sigma)^2))^((nu + 1) / 2);",
  "    f = numer / (sqrt(nu) * sigma * beta(nu / 2, 1 / 2));",
  "end",
  sep = "\n"
)

rStudentt_matlab <- paste(
  "function x = studenttrnd(mu, sigma, nu, M)",
  "    y = trnd(nu, M);",
  "    x = sigma * y + mu;",
  "end",
  sep = "\n"
)

fStudentt_matlab <- function(input){
  switch(input$property,
         pdf=paste("% calling function",
                   fMatlabHelper("studentt",
                           params = c(input$t_mu,input$t_sigma, input$t_nu),
                           input),
                   " ",
                   dStudentt_matlab,
           sep = "\n"),
         log_pdf=paste("% calling function",
                       fMatlabHelper("studentt",
                               params = c(input$t_mu,input$t_sigma, input$t_nu),
                               input),
                       " ",
                       dStudentt_matlab,
                       sep = "\n"),
         random=paste("% calling function",
                      fMatlabHelper("studentt",
                              params = c(input$t_mu,input$t_sigma, input$t_nu),
                              input),
                      " ",
                      rStudentt_matlab,
                      sep = "\n")
  )
}

dCauchy_matlab <- paste(
  "function f = cauchypdf(x, a, b)",
  "    f = b ./ (pi * (b.^2 + (x - a).^2));",
  "end",
  sep = "\n"
)

rCauchy_matlab <- paste(
  "function x = cauchyrnd(a, b, M)",
  "    y = trnd(1, M);",
  "    x = b * y + a;",
  "end",
  sep = "\n"
)

fCauchy_matlab <- function(input){
  lparams <- c(input$cauchy_location, input$cauchy_scale)
  switch(input$property,
         pdf=paste("% calling function",
                   fMatlabHelper("cauchy",
                                 params = lparams,
                                 input),
                   " ",
                   dCauchy_matlab,
                   sep = "\n"),
         log_pdf=paste("% calling function",
                       fMatlabHelper("cauchy",
                                     params = lparams,
                                     input),
                       " ",
                       dCauchy_matlab,
                       sep = "\n"),
         random=paste("% calling function",
                      fMatlabHelper("cauchy",
                                    params = lparams,
                                    input),
                      " ",
                      rCauchy_matlab,
                      sep = "\n")
  )
}

dHalfCauchy_matlab <- paste(
  "function f = halfcauchypdf(x, a, b)",
  "    if x < 0",
  "        f = 0",
  "    else",
  "        fun = @(y) b ./ (pi * (b.^2 + (y - a).^2));",
  "        c = integral(fun, 0, Inf);",
  "        f = (1 / c) * b ./ (pi * (b.^2 + (x - a).^2));",
  "    end",
  "end",
  sep = "\n"
)

rHalfCauchy_matlab <- paste(
  "function x = cauchyrnd(a, b, M)",
  "    y = trnd(1, M);",
  "    x = b * y + a;",
  "end",
  " ",
  "function x = halfcauchysinglernd(a, b)",
  "    x = cauchyrnd(a, b, 1);",
  "    while x < 0",
  "        x = cauchyrnd(a, b, 1);",
  "    end",
  "end",
  " ",
  "function x = halfcauchyrnd(a, b, M)",
  "    x = zeros(M);",
  "    n = numel(x);",
  "    y = zeros([n, 1]);",
  "    for i = 1:n",
  "        y(i) = halfcauchysinglernd(a, b);",
  "    end",
  "    x = reshape(y, M);",
  "end",
  sep = "\n"
)

fHalfCauchy_matlab <- function(input){
  lparams <- c(input$halfcauchy_location, input$halfcauchy_scale)
  switch(input$property,
         pdf=paste("% calling function",
                   fMatlabHelper("halfcauchy",
                                 params = lparams,
                                 input),
                   " ",
                   dHalfCauchy_matlab,
                   " ",
                   sep = "\n"),
         log_pdf=paste("% calling function",
                       fMatlabHelper("halfcauchy",
                                     params = lparams,
                                     input),
                       " ",
                       dHalfCauchy_matlab,
                       sep = "\n"),
         random=paste("% calling function",
                      fMatlabHelper("halfcauchy",
                                    params = lparams,
                                    input),
                      " ",
                      rHalfCauchy_matlab,
                      sep = "\n")
  )
}

dInverseGamma_matlab <- paste(
  "function f = inversegammapdf(x, alpha, beta)",
  "    if x < 0",
  "        f = 0;",
  "    else",
  "        f = (beta^alpha) / gamma(alpha) * x^(-alpha-1) * exp(-beta / x);",
  "    end",
  "end",
  sep = "\n"
)

rInverseGamma_matlab <- paste(
  "function x = inversegammarnd(alpha, beta, M)",
  "    y = gamrnd(alpha, 1 / beta, M);",
  "    x = 1 ./ y;",
  "end",
  sep = "\n"
)

fInverseGamma_matlab <- function(input){
  lparams <- c(input$inversegamma_shape, input$inversegamma_scale)
  switch(input$property,
         pdf=paste("% calling function",
                   fMatlabHelper("inversegamma",
                                 params = lparams,
                                 input),
                   " ",
                   dInverseGamma_matlab,
                   sep = "\n"),
         log_pdf=paste("% calling function",
                       fMatlabHelper("inversegamma",
                                     params = lparams,
                                     input),
                       " ",
                       dInverseGamma_matlab,
                       sep = "\n"),
         random=paste("% calling function",
                      fMatlabHelper("inversegamma",
                                    params = lparams,
                                    input),
                      " ",
                      rInverseGamma_matlab,
                      sep = "\n")
  )
}

dInverseChiSquared_matlab <- paste(
  "function f = inversechisquaredpdf(x, nu)",
  "    f = 2^(-nu/2) / gamma(nu / 2) * x^(-nu / 2 - 1) * exp(-1 / (2 * x));",
  "end",
  sep = "\n"
)

rInverseChiSquared_matlab <- paste(
  "function x = inversechisquaredrnd(nu, M)",
  "    y = chi2rnd(nu, M);",
  "    x = 1 ./ y;",
  "end",
  sep = "\n"
)

fInverseChiSquared_matlab <- function(input){
  lparams <- input$inversechisquared_df
  switch(input$property,
         pdf=paste("% calling function",
                   fMatlabHelper("inversechisquared",
                                 params = lparams,
                                 input),
                   " ",
                   dInverseChiSquared_matlab,
                   sep = "\n"),
         log_pdf=paste("% calling function",
                       fMatlabHelper("inversechisquared",
                                     params = lparams,
                                     input),
                       " ",
                       dInverseChiSquared_matlab,
                       sep = "\n"),
         random=paste("% calling function",
                      fMatlabHelper("inversechisquared",
                                    params = lparams,
                                    input),
                      " ",
                      rInverseChiSquared_matlab,
                      sep = "\n")
  )
}

dLogitNormal_matlab <- paste(
  "function f = logitnormalpdf(x, mu, sigma)",
  "    if x > 1 || x < 0",
  "        f = 0;",
  "    else",
  "        f = 1 / (sigma * sqrt(2 * pi)) * exp(-(log(x / (1 - x)) - mu)^2 / (2 * sigma^2)) * 1.0 / (x * (1.0 - x));",
  "    end",
  "end",
  sep = "\n"
)

rLogitNormal_matlab <- paste(
  "function x = logitnormalrnd(mu, sigma, M)",
  "    y = normrnd(mu, sigma, M);",
  "    x = 1 ./ (1 + exp(-y));",
  "end",
  sep = "\n"
)

fLogitNormal_matlab <- function(input){
  lparams <- c(input$logitnormal_mu, input$logitnormal_sigma)
  switch(input$property,
         pdf=paste("% calling function",
                   fMatlabHelper("logitnormal",
                                 params = lparams,
                                 input),
                   " ",
                   dLogitNormal_matlab,
                   sep = "\n"),
         log_pdf=paste("% calling function",
                       fMatlabHelper("logitnormal",
                                     params = lparams,
                                     input),
                       " ",
                       dLogitNormal_matlab,
                       sep = "\n"),
         random=paste("% calling function",
                      fMatlabHelper("logitnormal",
                                    params = lparams,
                                    input),
                      " ",
                      rLogitNormal_matlab,
                      sep = "\n")
  )
}

dBetaBinomial_matlab <- paste(
  "function f = betabinomialpdf(x, n, alpha, beta1)",
  "    if abs(x - round(x)) > 0",
  "        f = 0;",
  "    elseif x < 0 || x > n",
  "        f = 0;",
  "    else",
  "        f = nchoosek(n, x) * beta(x + alpha, n - x + beta1) / beta(alpha, beta1);",
  "    end",
  "end",
  sep = "\n"
)

rBetaBinomial_matlab <- paste(
  "function x = betabinomialrnd(n, alpha, beta1, M)",
  "    theta = betarnd(alpha, beta1, M);",
  "    x = binornd(n, theta);",
  "end",
  sep = "\n"
)


fBetaBinomial_matlab <- function(input){
  lparams <- c(input$betabinomial_size, input$betabinomial_shape1, input$betabinomial_shape2)
  switch(input$property,
         pdf=paste("% calling function",
                   fMatlabHelper("betabinomial",
                                 params = lparams,
                                 input),
                   " ",
                   dBetaBinomial_matlab,
                   sep = "\n"),
         log_pdf=paste("% calling function",
                       fMatlabHelper("betabinomial",
                                     params = lparams,
                                     input),
                       " ",
                       dBetaBinomial_matlab,
                       sep = "\n"),
         random=paste("% calling function",
                      fMatlabHelper("betabinomial",
                                    params = lparams,
                                    input),
                      " ",
                      rBetaBinomial_matlab,
                      sep = "\n")
  )
}

fMultivariatenormal_matlab <- function(input){
  mux <- input$multivariatenormal_mux
  muy <- input$multivariatenormal_muy
  sigmax <- input$multivariatenormal_sigmax
  sigmay <- input$multivariatenormal_sigmay
  rho <- input$multivariatenormal_rho
  
  switch(input$property,
         pdf=paste0("mvnpdf(x, [", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]])"),
         log_pdf=paste0("log(mvnpdf(x, [", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]]))"),
         random=paste0("mvnrnd([", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]], n)")
  )
}

dMultivariatet_matlab <- paste(
  "function f = multivariatetpdf(x, mu, Sigma, nu)",
  "    d = length(mu);",
  "    x_minus_mu = reshape(x - mu, d, 1);",
  "    f = gamma((nu + d) / 2) / (gamma(nu / 2) * nu^(d / 2) * pi^(d / 2) * det(Sigma)^0.5) * (1 + (1 / nu) * x_minus_mu' * inv(Sigma) * x_minus_mu)^(-(nu + d) / 2);",
  "end",
  sep = "\n"
)

rMultivariatet_matlab <- paste(
  "function x = multivariatetrnd(mu, Sigma, nu, n)",
  "    d = length(mu);",
  "    y = mvnrnd(zeros([d, 1]), Sigma, n);",
  "    u = chi2rnd(nu, n);",
  "    x = zeros([n, d]);",
  "    for i = 1:n",
  "        x(i, :) = mu + y(i, :) / sqrt(u(i) / nu);",
  "    end",
  "end",
  sep = "\n"
)

fMultivariatet_matlab <- function(input){
  
  mux <- input$multivariatet_mux
  muy <- input$multivariatet_muy
  sigmax <- input$multivariatet_sigmax
  sigmay <- input$multivariatet_sigmay
  rho <- input$multivariatet_rho
  df <- input$multivariatet_df
  
  switch(input$property,
         pdf=paste(
                   "% calling function",
                   paste0("multivariatetpdf(x, [", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]], ", df, ")"),
                   " ",
                   dMultivariatet_matlab,
                   sep = "\n"),
         log_pdf=paste(
                       "% calling function",
                       paste0("log(multivariatetpdf(x, [", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]], ", df, "))"),
                       " ",
                       dMultivariatet_matlab,
                       sep = "\n"),
         random=paste("% calling function",
           paste0("multivariatetrnd([", mux, ", ", muy, "], [[", sigmax^2, ", ", sigmax * sigmay * rho, "]; [", sigmax * sigmay * rho, ", ", sigmay^2, "]], ", df, ", n)"),
           " ",
           rMultivariatet_matlab,
           sep = "\n")
         )
}

fMultinomial_matlab <- function(input){
  comps <- c(input$multinomial_prob1, input$multinomial_prob2, input$multinomial_prob3)
  switch(input$property,
         pdf=paste0("mnpdf(x, [", comps[1], ", ", comps[2], ", ", comps[3], "]", " / sum([", comps[1], ", ", comps[2], ", ", comps[3], "]))"),
         log_pdf=paste0("log(mnpdf(x, [", comps[1], ", ", comps[2], ", ", comps[3], "]", " / sum([", comps[1], ", ", comps[2], ", ", comps[3], "])))"),
         random=paste0("mnrnd(",input$multinomial_size, ", [", comps[1], ", ", comps[2], ", ", comps[3], "]", " / sum([", comps[1], ", ", comps[2], ", ", comps[3], "]), n)")
         )
}

dWishart_matlab <- paste(
  "function g = multivariate_gamma(p, a)",
  "    j = 1:p;",
  "    g = gamma(a + (1 - j) / 2);",
  "    g = pi^(p * (p - 1) / 4) * prod(g);",
  "end",
  " ",
  "function f = wishartpdf(x, nu, S)",
  "    m = size(x);",
  "    d = m(1);",
  "    if nu < d - 1",
  "        f = 0;",
  "    else",
  "        f = det(x)^((nu - d - 1) / 2) * exp(-trace(inv(S) * x) / 2) * 1 / (2^(nu * d / 2) * det(S)^(nu / 2) * multivariate_gamma(d, nu / 2));",
  "    end",
  "end",
  sep = "\n")

fWishart_matlab <- function(input){
  lparams <- input$wishart_df
  switch(input$property,
         pdf=paste("% calling function (S must be symmetric and positive definite)",
           paste0("wishartpdf(x, ", lparams, ", S)"),
           " ",
           dWishart_matlab,
           sep = "\n"),
         log_pdf=paste("% calling function (S must be symmetric and positive definite)",
                       paste0("log(wishartpdf(x, ", lparams, ", S))"),
                       " ",
                       dWishart_matlab,
                       sep = "\n"),
         random=paste0(paste("for i = 1:n",
                             paste0("    wishrnd(S, ", lparams, ")"),
                             "end",
                             sep = "\n"))
           )
}

dInverseWishart_matlab <- paste(
  "function g = multivariate_gamma(p, a)",
  "    j = 1:p;",
  "    g = gamma(a + (1 - j) / 2);",
  "    g = pi^(p * (p - 1) / 4) * prod(g);",
  "end",
  " ",
  "function f = inversewishartpdf(x, nu, S)",
  "    m = size(x);",
  "    d = m(1);",
  "    if nu < d - 1",
  "        f = 0;",
  "    else",
  "        f = det(S)^(nu / 2) * det(x)^(-(nu + d + 1) / 2) * exp(-trace(S * inv(x)) / 2) * 1 / (2^(nu * d / 2) * multivariate_gamma(d, nu /2));",
  "    end",
  "end",
  sep = "\n")

fInverseWishart_matlab <- function(input){
  lparams <- input$inversewishart_df
  switch(input$property,
         pdf=paste("% calling function (S must be symmetric and positive definite)",
                   paste0("inversewishartpdf(x, ", lparams, ", S)"),
                   " ",
                   dInverseWishart_matlab,
                   sep = "\n"),
         log_pdf=paste("% calling function (S must be symmetric and positive definite)",
                       paste0("log(inversewishartpdf(x, ", lparams, ", S))"),
                       " ",
                       dInverseWishart_matlab,
                       sep = "\n"),
         random=paste0(paste("for i = 1:n",
                             paste0("    iwishrnd(S, ", lparams, ")"),
                             "end",
                             sep = "\n"))
  )
}

dLKJ_matlab <- paste(
  "function f = lkjpdf(x, nu)",
  "    m = size(x);",
  "    d = m(1);",
  "    n = numel(x);",
  "    if sum(diag(x)) ~= d || sum(sum(tril(x)))~=sum(sum(triu(x)))",
  "        f = 0.0;",
  "        return;",
  "    end",
  "    a_sum = 0;",
  "    a_prod = 1;",
  "    for k = 1:(d-1)",
  "        a_sum = a_sum + (2 * nu - 2 + d - k) * (d - k);",
  "        a_prod = a_prod * beta(nu + 0.5 * (d - k - 1), nu + 0.5 * (d - k - 1));",
  "    end",
  "    a_sum = 2^a_sum;",
  "    f = a_sum * a_prod * det(x)^(nu - 1);",
  "end",
  sep = "\n"
)

rLKJ_matlab <- paste(
  "function r = lkjsinglernd(nu, d)",
  "    if d == 1",
  "        r = 1;",
  "    elseif d==2",
  "        rho = 2 * betarnd(nu, nu, 1) - 1;",
  "        r = [[1, rho]; [rho, 1]];",
  "    else",
  "        beta1 = nu + (d - 2) / 2;",
  "        u = betarnd(beta1, beta1, 1);",
  "        r_12 = 2 * u - 1;",
  "        r = [[1, r_12]; [r_12, 1]];",
  "        for m = 2:(d-1)",
  "            beta1 = beta1 - 0.5;",
  "            y = betarnd(m / 2, beta1, 1);",
  "            a = normrnd(0, 1, [m, 1]);",
  "            anorm = sqrt(sum(a.^2));",
  "            u = a / anorm;",
  "            w = sqrt(y) * u;",
  "            A = chol(r);",
  "            z = A * w;",
  "            r = [[r, z]; [z', 1]];",
  "        end",
  "    end",
  "end",
  " ",
  "function r_list = lkjrnd(nu, d, n)",
  "    r_list = cell([n,1]);",
  "    for i = 1:n",
  "        r_list{i} = lkjsinglernd(nu, d);",
  "    end",
  "end",
  sep = "\n"
)

fLKJ_matlab <- function(input){
  switch(input$property,
         pdf=paste("% calling function",
                   fMatlabHelper("lkj",
                                 params = input$lkj_eta,
                                 input),
                   " ",
                   dLKJ_matlab,
                   sep = "\n"),
         log_pdf=paste("% calling function",
                       fMatlabHelper("lkj",
                                     params = input$lkj_eta,
                                     input),
                       " ",
                       dLKJ_matlab,
                       sep = "\n"),
         random=paste("% calling function",
                      fMatlabHelper("lkj",
                                    params = c(input$lkj_eta, input$lkj_dimension),
                                    input),
                      " ",
                      rLKJ_matlab,
                      sep = "\n")
  )
}

dDirichlet_matlab <- paste(
  "function f = beta_long(alpha_vec)",
  "    a_prod = prod(gamma(alpha_vec));",
  "    f = a_prod / gamma(sum(alpha_vec));",
  "end",
  " ",
  "function f = dirichletpdf(x_vec, alpha_vec)",
  "    beta_denom = beta_long(alpha_vec);",
  "    a_prod = prod(x_vec.^(alpha_vec-1));",
  "    f = a_prod / beta_denom;",
  "end",
  sep = "\n"
)

rDirichlet_matlab <- paste(
  "function x = dirichletrnd(alpha_vec, n)",
  "    p = length(alpha_vec);",
  "    x = gamrnd(repmat(alpha_vec, n, 1), 1, n, p);",
  "    x = x ./ repmat(sum(x, 2), 1, p);",
  "end",
  sep = "\n"
)

fDirichlet_matlab <- function(input){
  if(input$dirichlet_dimension==2){
    alpha <- c(input$dirichlet_alpha1, input$dirichlet_alpha2)
  }else if(input$dirichlet_dimension==3){
    alpha <- c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3)
  }else if(input$dirichlet_dimension==4){
    alpha <- c(input$dirichlet_alpha1, input$dirichlet_alpha2, input$dirichlet_alpha3, input$dirichlet_alpha4)
  }
  switch(input$property,
         pdf=paste("% calling function",
                   fMatlabHelper("dirichlet",
                                 params = alpha,
                                 input, vector_params = T,
                                 python_vector = T),
                   " ",
                   dDirichlet_matlab,
                   sep = "\n"),
         log_pdf=paste("% calling function",
                       fMatlabHelper("dirichlet",
                                     params = alpha,
                                     input, vector_params = T,
                                     python_vector = T),
                       " ",
                       dDirichlet_matlab,
                       sep = "\n"),
         random=paste("% calling function",
                      fMatlabHelper1("dirichlet",
                                    params = alpha,
                                    input, vector_params = T,
                                    python_vector = T),
                      " ",
                      rDirichlet_matlab,
                      sep = "\n")
  )
}

ddiscreteuniform_matlab <- paste(
  "function f = discreteuniformpdf(x, lower, upper)",
  "  if lower <= 0",
  "    diff = 1 - lower;",
  "  elseif lower > 1",
  "    diff = -(lower - 1);",
  "  else",
  "    diff = 0;",
  "  end",
  "  f = unidpdf(x + diff, upper + diff);",
  "end",
  sep = "\n"
)

rdiscreteuniform_matlab <- paste(
  "function x = discreteuniformrnd(lower, upper, n)",
  "  if lower <= 0",
  "    diff = 1 - lower;",
  "  elseif lower > 1",
  "    diff = -(lower - 1);",
  "  else",
  "    diff = 0;",
  "  end",
  "  x = unidrnd(upper + diff, n);",
  "  x = x - diff;",
  "end",
  sep = "\n"
)


fdiscreteuniform_matlab <- function(input){
  lparams <- c(input$discreteuniform_lower, input$discreteuniform_upper)
  switch(input$property,
         pdf=paste("% calling function",
                   fMatlabHelper("discreteuniform",
                                 params = lparams,
                                 input),
                   " ",
                   ddiscreteuniform_matlab,
                   sep = "\n"),
         log_pdf=paste("% calling function",
                       fMatlabHelper("discreteuniform",
                                     params = lparams,
                                     input),
                       " ",
                       ddiscreteuniform_matlab,
                       sep = "\n"),
         random=paste("% calling function",
                      fMatlabHelper("discreteuniform",
                                    params = lparams,
                                    input),
                      " ",
                      rdiscreteuniform_matlab,
                      sep = "\n")
  )
}

fMatlabcode <- function(input){
  text <- 
    if(input$distType=='Continuous'){
      switch(input$dist,
             Normal=fMatlabHelper("norm",
                                  params=c(input$normal_mu,input$normal_sigma),
                                  input),
             Uniform=fMatlabHelper("unif",
                                   params = c(input$uniform_a, input$uniform_b),
                                   input),
             LogNormal=fMatlabHelper("logn",
                                     params = c(input$lognormal_mu, input$lognormal_sigma),
                                     input),
             Exponential=fMatlabHelper("exp",
                                       params = 1 / input$exponential_rate, 
                                       input),
             Gamma=fMatlabHelper("gam",
                                  params = c(input$gamma_shape, 1 / input$gamma_rate),
                                  input),
             t=fStudentt_matlab(input),
             Cauchy=fCauchy_matlab(input),
             HalfCauchy=fHalfCauchy_matlab(input),
             Beta=fMatlabHelper("beta",
                                params = c(input$beta_a, input$beta_b),
                                input),
             InverseGamma=fInverseGamma_matlab(input),
             InverseChiSquared=fInverseChiSquared_matlab(input),
             LogitNormal=fLogitNormal_matlab(input)
      )
    }else if(input$distType=='Discrete'){
      switch(input$dist1,
             Bernoulli=fMatlabHelper("bino",
                                     params = c(1, input$bernoulli_prob),
                                     input),
             Binomial=fMatlabHelper("bino",
                                    params = c(input$binomial_size, input$binomial_prob),
                                    input),
             DiscreteUniform=fdiscreteuniform_matlab(input),
             Poisson=fMatlabHelper("poiss",
                                   params = input$poisson_lambda,
                                   input),
             NegativeBinomial=fMatlabHelper("nbin",
                                            params = c(input$negativebinomial_dispersion,
                                                       input$negativebinomial_dispersion / (input$negativebinomial_dispersion + input$negativebinomial_mean)),
                                            input),
             BetaBinomial=fBetaBinomial_matlab(input)
      )
    }else if(input$distType=='Multivariate'){
      switch(input$dist2,
             MultivariateNormal=fMultivariatenormal_matlab(input),
             MultivariateT=fMultivariatet_matlab(input),
             Multinomial=fMultinomial_matlab(input),
             Wishart=fWishart_matlab(input),
             InverseWishart=fInverseWishart_matlab(input),
             LKJ=fLKJ_matlab(input),
             Dirichlet=fDirichlet_matlab(input)
      )
    }
  return(prismCodeBlock(text, language = "matlab"))
}
