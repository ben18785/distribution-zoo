#!/bin/sh

changeName()
{
  grep -rl "input\$$1[^a-zA-Z]" ./App-1/*.R | xargs sed -ie "s/input\$$1\([^a-zA-Z]\)/input\$$2\1/"
}

changeName mu normal_mu
changeName sigma normal_sigma
changeName a uniform_a
changeName b uniform_b
changeName meanlog lognormal_mu
changeName sdlog lognormal_sigma
changeName rate exponential_rate
changeName shape gamma_shape
changeName rateGam gamma_rate
changeName muT t_mu
changeName sigmaT t_sigma
changeName nuT t_nu
changeName alpha beta_a
changeName beta beta_b
changeName locationC cauchy_location
changeName scaleC cauchy_scale
changeName locationHC halfcauchy_location
changeName scaleHC halfcauchy_scale
changeName shapeIG inversegamma_shape
changeName scaleIG inversegamma_scale
changeName dfIC inversechisquared_df
changeName muLogitN logitnormal_mu
changeName sigmaLogitN logitnormal_sigma
changeName probBer bernoulli_prob
changeName sizeBin binomial_size
changeName probBin binomial_prob
changeName lambdaPois poisson_lambda
changeName rangePois poisson_range
changeName meanNB negativebinomial_mean
changeName dispersionNB negativebinomial_dispersion
changeName rangeNB negativebinomial_range
changeName sizeBetaBin betabinomial_size
changeName shapeBetaBin1 betabinomial_shape1
changeName shapeBetaBin2 betabinomial_shape2
changeName meanXN multivariatenormal_mux
changeName meanYN multivariatenormal_muy
changeName sigmaXN multivariatenormal_sigmax
changeName sigmaYN multivariatenormal_sigmay
changeName rhoxyN multivariatenormal_rho
changeName rangeN multivariatenormal_range
changeName meanXT multivariatet_mux
changeName meanYT multivariatet_muy
changeName sigmaXT multivariatet_sigmax
changeName sigmaYT multivariatet_sigmay
changeName rhoxyT multivariatet_rho
changeName dfMVT multivariatet_df
changeName dimensionWish wishart_dimension
changeName dfWish wishart_df
changeName sampleSizeWish wishart_samplesize
changeName dimensionInWish inversewishart_dimension
changeName dfInvWish inversewishart_df
changeName sampleSizeInvWish inversewishart_samplesize
changeName dimensionsDirichlet dirichlet_dimension
changeName sampleSizeDirichlet dirichlet_samplesize
changeName alphaDirichlet1 dirichlet_alpha1
changeName alphaDirichlet2 dirichlet_alpha2
changeName alphaDirichlet3 dirichlet_alpha3
changeName alphaDirichlet4 dirichlet_alpha4
changeName angleMultinomial multinomial_angle
changeName sizeMultinomial multinomial_size
changeName probMultinomial1 multinomial_prob1
changeName probMultinomial2 multinomial_prob2
changeName probMultinomial3 multinomial_prob3
changeName dimensionLKJ lkj_dimension
changeName etaLKJ lkj_eta
changeName sampleSizeLKJ lkj_samplesize

rm ./App-1/*.Re