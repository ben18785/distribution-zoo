# generated code (pdf) is stable across all distributions and languages

    Code
      cat(dump_all_code("pdf"), sep = "\n")
    Output
      ===== Normal | R | pdf =====
      dnorm(x, 0, 1)
      
      ===== Normal | Python | pdf =====
      import scipy.stats
      scipy.stats.norm.pdf(x, 0, 1)
      
      ===== Normal | Stan | pdf =====
      exp(normal_lpdf(x| 0, 1))
      
      ===== Normal | Matlab | pdf =====
      normpdf(x, 0, 1)
      
      ===== Normal | Mathematica | pdf =====
      PDF[NormalDistribution[0, 1], x]
      
      ===== Normal | Julia | pdf =====
      using Random, Distributions
      pdf(Normal(0, 1), x)
      
      ===== Normal | LaTeX | pdf =====
      \mathrm{E}(X) = \mu
      
      ===== Uniform | R | pdf =====
      dunif(x, 0, 1)
      
      ===== Uniform | Python | pdf =====
      import scipy.stats
      scipy.stats.uniform.pdf(x, 0, 1)
      
      ===== Uniform | Stan | pdf =====
      exp(uniform_lpdf(x| 0, 1))
      
      ===== Uniform | Matlab | pdf =====
      unifpdf(x, 0, 1)
      
      ===== Uniform | Mathematica | pdf =====
      PDF[UniformDistribution[{0, 1}], x]
      
      ===== Uniform | Julia | pdf =====
      using Random, Distributions
      pdf(Uniform(0, 1), x)
      
      ===== Uniform | LaTeX | pdf =====
      \mathrm{E}(X) = \frac{1}{2}(a + b)
      
      ===== LogNormal | R | pdf =====
      dlnorm(x, 0, 1)
      
      ===== LogNormal | Python | pdf =====
      import scipy.stats
      scipy.stats.lognorm.pdf(x, scale=1, s=1)
      
      ===== LogNormal | Stan | pdf =====
      exp(lognormal_lpdf(x| 0, 1))
      
      ===== LogNormal | Matlab | pdf =====
      lognpdf(x, 0, 1)
      
      ===== LogNormal | Mathematica | pdf =====
      PDF[LogNormalDistribution[0, 1], x]
      
      ===== LogNormal | Julia | pdf =====
      using Random, Distributions
      pdf(LogNormal(0, 1), x)
      
      ===== LogNormal | LaTeX | pdf =====
      \mathrm{E}(X) = \text{exp}(\mu + \frac{\sigma^2}{2})
      
      ===== Exponential | R | pdf =====
      dexp(x, 0.5)
      
      ===== Exponential | Python | pdf =====
      import scipy.stats
      scipy.stats.expon.pdf(x, loc=0, scale=2)
      
      ===== Exponential | Stan | pdf =====
      exp(exponential_lpdf(x| 0.5))
      
      ===== Exponential | Matlab | pdf =====
      exppdf(x, 2)
      
      ===== Exponential | Mathematica | pdf =====
      PDF[ExponentialDistribution[0.5], x]
      
      ===== Exponential | Julia | pdf =====
      using Random, Distributions
      pdf(Exponential(2), x)
      
      ===== Exponential | LaTeX | pdf =====
      \mathrm{E}(X) = \frac{1}{\lambda}
      
      ===== Gamma | R | pdf =====
      dgamma(x, 1, 0.5)
      
      ===== Gamma | Python | pdf =====
      import scipy.stats
      scipy.stats.gamma.pdf(x, a=1, loc=0, scale=2)
      
      ===== Gamma | Stan | pdf =====
      exp(gamma_lpdf(x| 1, 0.5))
      
      ===== Gamma | Matlab | pdf =====
      gampdf(x, 1, 2)
      
      ===== Gamma | Mathematica | pdf =====
      PDF[GammaDistribution[1, 2], x]
      
      ===== Gamma | Julia | pdf =====
      using Random, Distributions
      pdf(Gamma(1, 2), x)
      
      ===== Gamma | LaTeX | pdf =====
      \mathrm{E}(X) = \frac{\alpha}{\beta}
      
      ===== t | R | pdf =====
      library(LaplacesDemon)
      dst(x, 0, 1, 3)
      
      ===== t | Python | pdf =====
      import scipy.stats
      scipy.stats.t.pdf(x, 3, 0, 1)
      
      ===== t | Stan | pdf =====
      exp(student_t_lpdf(x| 3, 0, 1))
      
      ===== t | Matlab | pdf =====
      % calling function
      studenttpdf(x, 0, 1, 3)
       
      function f = studenttpdf(x, mu, sigma, nu)
          numer = (nu / (nu + ((x - mu) / sigma)^2))^((nu + 1) / 2);
          f = numer / (sqrt(nu) * sigma * beta(nu / 2, 1 / 2));
      end
      
      ===== t | Mathematica | pdf =====
      PDF[StudentTDistribution[0, 1, 3], x]
      
      ===== t | Julia | pdf =====
      using Random, Distributions
      pdf(TDist(3), (x - 0) / 1) / 1
      
      ===== t | LaTeX | pdf =====
      \mathrm{E}(X) = \mu, \text{ if }\nu>1 \text{ otherwise undefined}
      
      ===== Beta | R | pdf =====
      dbeta(x, 1, 1)
      
      ===== Beta | Python | pdf =====
      import scipy.stats
      scipy.stats.beta.pdf(x, 1, 1)
      
      ===== Beta | Stan | pdf =====
      exp(beta_lpdf(x| 1, 1))
      
      ===== Beta | Matlab | pdf =====
      betapdf(x, 1, 1)
      
      ===== Beta | Mathematica | pdf =====
      PDF[BetaDistribution[1, 1], x]
      
      ===== Beta | Julia | pdf =====
      using Random, Distributions
      pdf(Beta(1, 1), x)
      
      ===== Beta | LaTeX | pdf =====
      \mathrm{E}(X) = \frac{\alpha}{\alpha + \beta}
      
      ===== Cauchy | R | pdf =====
      dcauchy(x, 0, 1)
      
      ===== Cauchy | Python | pdf =====
      import scipy.stats
      scipy.stats.cauchy.pdf(x, 0, 1)
      
      ===== Cauchy | Stan | pdf =====
      exp(cauchy_lpdf(x| 0, 1))
      
      ===== Cauchy | Matlab | pdf =====
      % calling function
      cauchypdf(x, 0, 1)
       
      function f = cauchypdf(x, a, b)
          f = b ./ (pi * (b.^2 + (x - a).^2));
      end
      
      ===== Cauchy | Mathematica | pdf =====
      PDF[CauchyDistribution[0, 1], x]
      
      ===== Cauchy | Julia | pdf =====
      using Random, Distributions
      pdf(Cauchy(0, 1), x)
      
      ===== Cauchy | LaTeX | pdf =====
      \mathrm{E}(X) = \text{ undefined}
      
      ===== HalfCauchy | R | pdf =====
      dhalfcauchy <- function(x, location, scale, log=FALSE){
        if(x >= 0)
          val <- 1.0 / (pi * (1 + ((x - location) / scale)^2) * scale * (0.5 + atan(location / scale) / pi))
        else
          val <- 0.0
        if(!log)
          return(val)
        else
          return(log(val))
      }
      # calling function
      dhalfcauchy(x, 0, 1)
      
      ===== HalfCauchy | Python | pdf =====
      import numpy
      def halfcauchy_pdf(x, location, scale):
          if x >= 0:
              val = 1.0 / (numpy.pi * (1 + ((x - location) / scale)**2) * scale * (0.5 + numpy.arctan(location / scale) / numpy.pi))
          else:
              val = 0.0
          return val
      # calling function
      halfcauchy_pdf(x, 0, 1)
      
      ===== HalfCauchy | Stan | pdf =====
      exp(cauchy_lpdf(x| 0, 1) - cauchy_lccdf(0| 0, 1))
      
      ===== HalfCauchy | Matlab | pdf =====
      % calling function
      halfcauchypdf(x, 0, 1)
       
      function f = halfcauchypdf(x, a, b)
          if x < 0
              f = 0
          else
              fun = @(y) b ./ (pi * (b.^2 + (y - a).^2));
              c = integral(fun, 0, Inf);
              f = (1 / c) * b ./ (pi * (b.^2 + (x - a).^2));
          end
      end
      
      ===== HalfCauchy | Mathematica | pdf =====
      aDist = TruncatedDistribution[{0, \[Infinity]}, CauchyDistribution[0, 1]]
      PDF[aDist, x]
      
      ===== HalfCauchy | Julia | pdf =====
      using Random, Distributions
      d=Truncated(Cauchy(0, 1), 0, Inf)
      pdf(d, x)
      
      ===== HalfCauchy | LaTeX | pdf =====
      \mathrm{E}(X) = \text{ undefined}
      
      ===== InverseGamma | R | pdf =====
      library(actuar)
      dinvgamma(x, 2, 1)
      
      ===== InverseGamma | Python | pdf =====
      import scipy.stats
      scipy.stats.invgamma.pdf(x, a=2, loc=0, scale=1)
      
      ===== InverseGamma | Stan | pdf =====
      exp(inv_gamma_lpdf(x| 2, 1))
      
      ===== InverseGamma | Matlab | pdf =====
      % calling function
      inversegammapdf(x, 2, 1)
       
      function f = inversegammapdf(x, alpha, beta)
          if x < 0
              f = 0;
          else
              f = (beta^alpha) / gamma(alpha) * x^(-alpha-1) * exp(-beta / x);
          end
      end
      
      ===== InverseGamma | Mathematica | pdf =====
      PDF[InverseGammaDistribution[2, 1], x]
      
      ===== InverseGamma | Julia | pdf =====
      using Random, Distributions
      pdf(InverseGamma(2, 1), x)
      
      ===== InverseGamma | LaTeX | pdf =====
      \mathrm{E}(X) = \begin{cases}
      \frac{\beta }{\alpha -1}, & \alpha >1 \\
      \text{undefined}, & \text{Otherwise}
      \end{cases}
      
      ===== InverseChiSquared | R | pdf =====
      library(LaplacesDemon)
      dinvchisq(x, 3)
      
      ===== InverseChiSquared | Python | pdf =====
      import numpy
      import scipy.special
      def inversechisquared_pdf(x, df):
          temp = df / 2.0
          val = (2**(-temp) / scipy.special.gamma(temp)) * x**-(temp + 1) * numpy.exp(-1.0 / (2 * x))
          return val
      # calling function
      inversechisquared_pdf(x, 3)
      
      ===== InverseChiSquared | Stan | pdf =====
      exp(inv_chi_square_lpdf(x| 3))
      
      ===== InverseChiSquared | Matlab | pdf =====
      % calling function
      inversechisquaredpdf(x, 3)
       
      function f = inversechisquaredpdf(x, nu)
          f = 2^(-nu/2) / gamma(nu / 2) * x^(-nu / 2 - 1) * exp(-1 / (2 * x));
      end
      
      ===== InverseChiSquared | Mathematica | pdf =====
      PDF[InverseChiSquareDistribution[3], x]
      
      ===== InverseChiSquared | Julia | pdf =====
      using Random, Distributions
      pdf(InverseGamma(3 / 2, 1 / 2), x)
      
      ===== InverseChiSquared | LaTeX | pdf =====
      \mathrm{E}(X) = \begin{cases}
      \frac{1}{\nu -2}, & \nu >2 \\
      \text{undefined}, & \text{Otherwise}
      \end{cases}
      
      ===== LogitNormal | R | pdf =====
      library(logitnorm)
      dlogitnorm(x, 1, 1)
      
      ===== LogitNormal | Python | pdf =====
      import numpy
      import scipy.special
      def logitnormal_pdf(x, mu, sigma):
          temp = ((scipy.special.logit(x) - mu)**2 / (2 * sigma**2))
          return (1.0 / sigma) * (1.0 / numpy.sqrt(2 * numpy.pi)) * numpy.exp(-temp) * (1.0 / (x * (1.0 - x)))
      # calling function
      logitnormal_pdf(x, 1, 1)
      
      ===== LogitNormal | Stan | pdf =====
      functions{
        real logitnormal_lpdf(real x, real mu, real sigma){
          real temp = (logit(x) - mu)^2 / (2 * sigma^2);
          if(sigma < 0)
            return(log(0));
          return(-log(sigma) - 0.5 * log(2 * pi()) - temp - log(x) - log(1.0 - x));
        }
      }
      // calling function
      exp(logitnormal_lpdf(x| 1, 1))
      
      ===== LogitNormal | Matlab | pdf =====
      % calling function
      logitnormalpdf(x, 1, 1)
       
      function f = logitnormalpdf(x, mu, sigma)
          if x > 1 || x < 0
              f = 0;
          else
              f = 1 / (sigma * sqrt(2 * pi)) * exp(-(log(x / (1 - x)) - mu)^2 / (2 * sigma^2)) * 1.0 / (x * (1.0 - x));
          end
      end
      
      ===== LogitNormal | Mathematica | pdf =====
      aDist=TransformedDistribution[LogisticSigmoid[x], x \[Distributed] NormalDistribution[1, 1]]
      PDF[aDist, x]
      
      ===== LogitNormal | Julia | pdf =====
      using Random, Distributions
      pdf(LogitNormal(1, 1), x)
      
      ===== LogitNormal | LaTeX | pdf =====
      \mathrm{E}(X) = \text{ No simple analytic expression}
      
      ===== Bernoulli | R | pdf =====
      dbinom(x, 1, 0.5)
      
      ===== Bernoulli | Python | pdf =====
      import scipy.stats
      scipy.stats.bernoulli.pmf(x, 0.5)
      
      ===== Bernoulli | Stan | pdf =====
      exp(bernoulli_lpmf(x| 0.5))
      
      ===== Bernoulli | Matlab | pdf =====
      binopdf(x, 1, 0.5)
      
      ===== Bernoulli | Mathematica | pdf =====
      PDF[BernoulliDistribution[0.5], x]
      
      ===== Bernoulli | Julia | pdf =====
      using Random, Distributions
      pdf(Bernoulli(0.5), x)
      
      ===== Bernoulli | LaTeX | pdf =====
      \mathrm{E}(X) = p
      
      ===== BetaBinomial | R | pdf =====
      # function definition
      dbetabinom <- function(x, size, alpha, beta, log=FALSE){
        if(!log)
          return(choose(size, x) * beta(x + alpha, size - x + beta) / beta(alpha, beta))
        else
          return(lchoose(size, x) + lbeta(x + alpha, size - x + beta) - lbeta(alpha, beta))
      }
      # calling function
      dbetabinom(x, 10, 1, 1)
      
      ===== BetaBinomial | Python | pdf =====
      import scipy.special
      def betabinomial_pmf(x, size, a, b):
          return scipy.special.comb(size, x) * scipy.special.beta(x + a, size - x + b) / scipy.special.beta(a, b)
      # calling function
      betabinomial_pmf(x, 10, 1, 1)
      
      ===== BetaBinomial | Stan | pdf =====
      exp(beta_binomial_lpmf(x| 10, 1, 1))
      
      ===== BetaBinomial | Matlab | pdf =====
      % calling function
      betabinomialpdf(x, 10, 1, 1)
       
      function f = betabinomialpdf(x, n, alpha, beta1)
          if abs(x - round(x)) > 0
              f = 0;
          elseif x < 0 || x > n
              f = 0;
          else
              f = nchoosek(n, x) * beta(x + alpha, n - x + beta1) / beta(alpha, beta1);
          end
      end
      
      ===== BetaBinomial | Mathematica | pdf =====
      PDF[BetaBinomialDistribution[1, 1, 10], x]
      
      ===== BetaBinomial | Julia | pdf =====
      using Random, Distributions
      pdf(BetaBinomial(10, 1, 1), x)
      
      ===== BetaBinomial | LaTeX | pdf =====
      \mathrm{E}(X) = \frac{n\alpha}{\alpha+\beta}
      
      ===== Binomial | R | pdf =====
      dbinom(x, 10, 0.5)
      
      ===== Binomial | Python | pdf =====
      import scipy.stats
      scipy.stats.binom.pmf(x, 10, 0.5)
      
      ===== Binomial | Stan | pdf =====
      exp(binomial_lpmf(x| 10, 0.5))
      
      ===== Binomial | Matlab | pdf =====
      binopdf(x, 10, 0.5)
      
      ===== Binomial | Mathematica | pdf =====
      PDF[BinomialDistribution[10, 0.5], x]
      
      ===== Binomial | Julia | pdf =====
      using Random, Distributions
      pdf(Binomial(10, 0.5), x)
      
      ===== Binomial | LaTeX | pdf =====
      \mathrm{E}(X) = np
      
      ===== DiscreteUniform | R | pdf =====
      # function definition
      ddiscreteuniform <- function(x, min=0, max=1, log=FALSE){
      if(x >= min & x <= max & round(x) == x)
        if(!log)
          return(1 / (max - min + 1))
        else
          return(-log(max - min + 1))
      else
        if(!log)
          return(0.0)
        else
          return(-Inf)
      }
      # calling function
      ddiscreteuniform(x, 0, 1)
      
      ===== DiscreteUniform | Python | pdf =====
      import scipy.stats
      scipy.stats.randint.pmf(x, 0, 2)
      
      ===== DiscreteUniform | Stan | pdf =====
      functions{
        real discreteuniform_lpmf(int x, int lower, int upper){
          if(upper < lower)
            return(log(0));
          if(x < lower)
            return(log(0));
          if(x > upper)
            return(log(0));
          return(-log(upper - lower + 1));
        }
      }
      // calling function
      exp(discreteuniform_lpmf(x| 0, 1))
      
      ===== DiscreteUniform | Matlab | pdf =====
      % calling function
      discreteuniformpdf(x, 0, 1)
       
      function f = discreteuniformpdf(x, lower, upper)
        if lower <= 0
          diff = 1 - lower;
        elseif lower > 1
          diff = -(lower - 1);
        else
          diff = 0;
        end
        f = unidpdf(x + diff, upper + diff);
      end
      
      ===== DiscreteUniform | Mathematica | pdf =====
      PDF[DiscreteUniformDistribution[{0, 1}], x]
      
      ===== DiscreteUniform | Julia | pdf =====
      using Random, Distributions
      pdf(DiscreteUniform(0, 1), x)
      
      ===== DiscreteUniform | LaTeX | pdf =====
      \mathrm{E}(X) = \frac{a + b}{2}
      
      ===== NegativeBinomial | R | pdf =====
      dnbinom(x, mu=10, size=3)
      
      ===== NegativeBinomial | Python | pdf =====
      import scipy.stats
      def negativebinomial_pmf(x, mu, kappa):
          n = kappa
          p = float(kappa) / (kappa + mu)
          return scipy.stats.nbinom.pmf(x, n, p)
      # calling function
      negativebinomial_pmf(x, 10, 3)
      
      ===== NegativeBinomial | Stan | pdf =====
      exp(neg_binomial_2_lpmf(x| 10, 3))
      
      ===== NegativeBinomial | Matlab | pdf =====
      nbinpdf(x, 3, 0.230769230769231)
      
      ===== NegativeBinomial | Mathematica | pdf =====
      PDF[NegativeBinomialDistribution[3, 0.230769230769231], x]
      
      ===== NegativeBinomial | Julia | pdf =====
      using Random, Distributions
      pdf(NegativeBinomial(3, 0.230769230769231), x)
      
      ===== NegativeBinomial | LaTeX | pdf =====
      \mathrm{E}(X) = \lambda
      
      ===== Poisson | R | pdf =====
      dpois(x, 10)
      
      ===== Poisson | Python | pdf =====
      import scipy.stats
      scipy.stats.poisson.pmf(x, 10)
      
      ===== Poisson | Stan | pdf =====
      exp(poisson_lpmf(x| 10))
      
      ===== Poisson | Matlab | pdf =====
      poisspdf(x, 10)
      
      ===== Poisson | Mathematica | pdf =====
      PDF[PoissonDistribution[10], x]
      
      ===== Poisson | Julia | pdf =====
      using Random, Distributions
      pdf(Poisson(10), x)
      
      ===== Poisson | LaTeX | pdf =====
      \mathrm{E}(X) = \lambda
      
      ===== Dirichlet | R | pdf =====
      library(LaplacesDemon)
      ddirichlet(x, c(2, 2))
      
      ===== Dirichlet | Python | pdf =====
      import scipy.stats
      scipy.stats.dirichlet.pdf(x, [2, 2])
      
      ===== Dirichlet | Stan | pdf =====
      exp(dirichlet_lpdf(x| to_vector([2, 2])))
      
      ===== Dirichlet | Matlab | pdf =====
      % calling function
      dirichletpdf(x, [2, 2])
       
      function f = beta_long(alpha_vec)
          a_prod = prod(gamma(alpha_vec));
          f = a_prod / gamma(sum(alpha_vec));
      end
       
      function f = dirichletpdf(x_vec, alpha_vec)
          beta_denom = beta_long(alpha_vec);
          a_prod = prod(x_vec.^(alpha_vec-1));
          f = a_prod / beta_denom;
      end
      
      ===== Dirichlet | Mathematica | pdf =====
      (* Note that x is without last (redundant) dimension *)
      PDF[DirichletDistribution[{2, 2}], x]
      
      ===== Dirichlet | Julia | pdf =====
      using Random, Distributions
      pdf(Dirichlet([2, 2]), x)
      
      ===== Dirichlet | LaTeX | pdf =====
      \mathrm{E}(X_i) = \frac{\alpha_i}{\sum_{k=1}^{d}\alpha_k}
      
      ===== InverseWishart | R | pdf =====
      library(LaplacesDemon)
      # note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)
      dinvwishart(x, 8, S)
      
      ===== InverseWishart | Python | pdf =====
      import scipy.stats
      # note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)
      scipy.stats.invwishart.pdf(x, 8, S)
      
      ===== InverseWishart | Stan | pdf =====
      // S is symmetric and positive-definite
      exp(inv_wishart_lpdf(x| 8, S))
      
      ===== InverseWishart | Matlab | pdf =====
      % calling function (S must be symmetric and positive definite)
      inversewishartpdf(x, 8, S)
       
      function g = multivariate_gamma(p, a)
          j = 1:p;
          g = gamma(a + (1 - j) / 2);
          g = pi^(p * (p - 1) / 4) * prod(g);
      end
       
      function f = inversewishartpdf(x, nu, S)
          m = size(x);
          d = m(1);
          if nu < d - 1
              f = 0;
          else
              f = det(S)^(nu / 2) * det(x)^(-(nu + d + 1) / 2) * exp(-trace(S * inv(x)) / 2) * 1 / (2^(nu * d / 2) * multivariate_gamma(d, nu /2));
          end
      end
      
      ===== InverseWishart | Mathematica | pdf =====
      (* S must be symmetric and positive definite *)
      multivariateGamma[p_, a_] := \[Pi]^(p (p - 1)/4) Product[Gamma[a + (1 - j)/2], {j, 1, p}]
       
      inverseWishartPDF[X_, \[Nu]_ /; \[Nu] > 0, S_] := Module[{d = Dimensions[X][[1]]},
        Det[S]^(\[Nu]/ 2) Det[X]^(-(\[Nu] + d + 1)/2) Exp[-Tr[S.Inverse[X]]/2]
        1 / (2^(\[Nu] d/2) multivariateGamma[d, \[Nu]/2])]
       
      inverseWishartPDF[x, 8, S]
      
      ===== InverseWishart | Julia | pdf =====
      using Random, Distributions
      # S must be symmetric and positive definite and of size 4x4
      aDist=InverseWishart(8, S)
      pdf(aDist, x)
      
      ===== InverseWishart | LaTeX | pdf =====
      \mathrm{E}(X) = \begin{cases}
      \frac{\Psi}{\nu-d-1}, & \nu>d+1 \\
      \text{undefined}, & \text{otherwise}
      \end{cases}
      
      ===== LKJ | R | pdf =====
      dlkj <- function(x, nu, log=FALSE){
        d <- nrow(x)
        if(sum(diag(x)) != d | x[upper.tri(x)] != x[lower.tri(x)])
          return(ifelse(!log, 0.0, -Inf))
        det_a <- det(x) ^ (nu - 1)
        a_sum <- 0
        b_sum <- 1
        k <- 1
        for(i in 1:(d - 1)){
          a_sum <- a_sum + (2 * nu - 2 + d - k) * (d - k)
          b_sum <- b_sum * (beta(nu + 0.5 * (d - k - 1), nu + 0.5 * (d - k - 1)) ^ (d - k))
          k <- k + 1
        }
        a_sum <- 2 ^ a_sum
        if(!log)
          return(a_sum * b_sum * det_a)
        else
          return(log(a_sum) + log(b_sum) + log(det_a))
      }
      # calling function
      dlkj(x, 1)
      
      ===== LKJ | Python | pdf =====
      import scipy.special
      import numpy
      def lkj_pdf(x, nu):
          d = len(x)
          if numpy.sum(numpy.diag(x)) != d or not numpy.all(numpy.linalg.eigvals(x) > 0) or numpy.sum(numpy.triu(x)) != numpy.sum(numpy.tril(x)):
              return 0.0
          det = numpy.linalg.det(x)**(nu - 1)
          a_sum = 0
          b_sum = 1
          k = 1
          for i in range(d - 1):
              a_sum += (2 * nu - 2 + d - k) * (d - k)
              b_sum *= scipy.special.beta(nu + 0.5 * (d - k - 1), nu + 0.5 * (d - k - 1))**(d - k)
              k += 1
          a_sum = 2**a_sum
          return a_sum * b_sum * det
      # calling function
      lkj_pdf(x, 1)
      
      ===== LKJ | Stan | pdf =====
      exp(lkj_corr_lpdf(x| 1))
      
      ===== LKJ | Matlab | pdf =====
      % calling function
      lkjpdf(x, 1)
       
      function f = lkjpdf(x, nu)
          m = size(x);
          d = m(1);
          n = numel(x);
          if sum(diag(x)) ~= d || sum(sum(tril(x)))~=sum(sum(triu(x)))
              f = 0.0;
              return;
          end
          a_sum = 0;
          a_prod = 1;
          for k = 1:(d-1)
              a_sum = a_sum + (2 * nu - 2 + d - k) * (d - k);
              a_prod = a_prod * beta(nu + 0.5 * (d - k - 1), nu + 0.5 * (d - k - 1));
          end
          a_sum = 2^a_sum;
          f = a_sum * a_prod * det(x)^(nu - 1);
      end
      
      ===== LKJ | Mathematica | pdf =====
      LKJPDF[X_, \[Nu]_] := Module[{d = Dimensions[X][[1]], aSum, bProd},
        If[Or[Or[Total[Diagonal[X]] != d, Total[Total[LowerTriangularize[X]]] != Total[Total[UpperTriangularize[X]]]], !PositiveDefiniteMatrixQ[X]], Return[0.0]];
        aSum = 2^Sum[(2 \[Nu] - 2 + d - k) (d - k), {k, 1, d - 1}];
        bProd = Product[Beta[\[Nu] + 0.5 (d - k - 1), \[Nu] + 0.5 (d - k - 1)], {k, 1, d - 1}];
        aSum bProd Det[X]^(\[Nu] - 1)]
       
      (*Calling function*)
      LKJPDF[x, 1]
      
      ===== LKJ | Julia | pdf =====
      using Random, Distributions
      using LinearAlgebra
      using GSL
      function LKJ_pdf(X, nu)
          d = size(X)[1]
          if sum(Diagonal(X)) != d || sum(LowerTriangular(X)) != sum(UpperTriangular(X)) || !isposdef(X) || nu < 0
              return 0.0;
          end
      
          a_sum = 0.0;
          a_prod = 1.0;
          for k = 1:(d - 1)
              a_sum += (2 * nu - 2 + d - k) * (d - k);
              a_prod *= sf_beta(nu + 0.5 * (d - k - 1), nu + 0.5 * (d - k - 1));
          end
          a_sum = 2^a_sum;
          return a_sum * a_prod * det(X)^(nu - 1);
      end
      LKJ_pdf(x, 1)
      
      ===== LKJ | LaTeX | pdf =====
      \mathrm{E}(X) = \mathcal{I}_d
      
      ===== Multinomial | R | pdf =====
      dmultinom(x, prob=c(0.5, 0.5, 0.5))
      
      ===== Multinomial | Python | pdf =====
      import scipy.stats
      scipy.stats.multinomial.pmf(x, 6, [0.333333333333333, 0.333333333333333, 0.333333333333333])
      
      ===== Multinomial | Stan | pdf =====
      exp(multinomial_lpmf(x| to_vector([0.333333333333333, 0.333333333333333, 0.333333333333333])))
      
      ===== Multinomial | Matlab | pdf =====
      mnpdf(x, [0.5, 0.5, 0.5] / sum([0.5, 0.5, 0.5]))
      
      ===== Multinomial | Mathematica | pdf =====
      aDist=MultinomialDistribution[6, {0.333333333333333, 0.333333333333333, 0.333333333333333}]
      PDF[aDist, x]
      
      ===== Multinomial | Julia | pdf =====
      using Random, Distributions
      aDist=Multinomial(6, [0.333333333333333, 0.333333333333333, 0.333333333333333])
      pdf(aDist, x)
      
      ===== Multinomial | LaTeX | pdf =====
      \mathrm{E}(X_i) = n p_i \text{, }\forall i
      
      ===== MultivariateNormal | R | pdf =====
      # 2d mvt normal pdf
      library(mvtnorm)
      dmvrnorm2D <- function(x, mux, muy, sigmax, sigmay, rho, log=FALSE){
        return(dmvnorm(x, c(mux, muy),
                       matrix(c(sigmax^2, sigmax * sigmay * rho,
                                sigmax * sigmay * rho, sigmay^2),
                               ncol = 2),
                       log))
      }
      # calling function (note x must be 2d)
      dmvrnorm2D(x, 0, 0, 1, 1, 0)
      
      ===== MultivariateNormal | Python | pdf =====
      import scipy.stats
      def normal2d_pdf(x, mux, muy, sigmax, sigmay, rho):
          return scipy.stats.multivariate_normal.pdf(x, [mux, muy], [[sigmax**2, sigmax * sigmay * rho], [sigmax * sigmay * rho, sigmay**2]])
      # calling function
      normal2d_pdf(x, 0, 0, 1, 1, 0)
      
      ===== MultivariateNormal | Stan | pdf =====
      // non-Cholesky
      exp(multi_normal_lpdf(x| [0, 0], [[1, 0], [0, 1]]))
      // Cholesky (faster)
      exp(multi_normal_cholesky_lpdf(x| [0, 0], [[1, 0], [0, 1]]))
      
      ===== MultivariateNormal | Matlab | pdf =====
      mvnpdf(x, [0, 0], [[1, 0]; [0, 1]])
      
      ===== MultivariateNormal | Mathematica | pdf =====
      aDist=MultinormalDistribution[{0, 0}, {{1, 0}, {0, 1}}]
      PDF[aDist, x]
      
      ===== MultivariateNormal | Julia | pdf =====
      using Random, Distributions
      aDist=MvNormal(float([0, 0]), float([[1 0]; [0 1]]))
      pdf(aDist, x)
      
      ===== MultivariateNormal | LaTeX | pdf =====
      \mathrm{E}(X) = \mu
      
      ===== MultivariateT | R | pdf =====
      # 2d mvt Student-t distribution pdf
      library(mvtnorm)
      dmvt2D <- function(x, mux, muy, sigmax, sigmay, rho, df, log=FALSE){
        return(dmvt(x, c(mux, muy),
                    matrix(c(sigmax^2, sigmax * sigmay * rho,
                             sigmax * sigmay * rho, sigmay^2),
                           ncol = 2),
                    df,
                    log))
      }
      # calling function (note x must be 2d)
      dmvt2D(x, 0, 0, 1, 1, 0, 10)
      
      ===== MultivariateT | Python | pdf =====
      import scipy.special
      import numpy
      # general Student t
      def studentt_pdf(x, mu, sigma, nu):
          p = len(mu)
          first = scipy.special.gamma(0.5 * (nu + p)) / (scipy.special.gamma(nu / 2.0) * nu**(float(p) / 2) * numpy.pi**(float(p) / 2) * numpy.sqrt(numpy.linalg.det(sigma)))
          x_minus_mu = numpy.array(x) - numpy.array(mu)
          sigma_inv = numpy.linalg.inv(sigma)
          second = (1 + (1.0 / float(nu)) * numpy.matmul(numpy.matmul(x_minus_mu, sigma_inv), numpy.transpose(x_minus_mu)))**(-0.5 * (nu + p))
          return first * second
      # 2d Student t
      def studentt2d_pdf(x, mux, muy, sigmax, sigmay, rho, nu):
          return studentt_pdf(x, [mux, muy], [[sigmax**2, sigmax * sigmay * rho], [sigmax * sigmay * rho, sigmay**2]], nu)
      # calling function
      studentt2d_pdf(x, 0, 0, 1, 1, 0, 10)
      
      ===== MultivariateT | Stan | pdf =====
      exp(multi_student_t_lpdf(x| 10, [0, 0], [[1, 0], [0, 1]]))
      
      ===== MultivariateT | Matlab | pdf =====
      % calling function
      multivariatetpdf(x, [0, 0], [[1, 0]; [0, 1]], 10)
       
      function f = multivariatetpdf(x, mu, Sigma, nu)
          d = length(mu);
          x_minus_mu = reshape(x - mu, d, 1);
          f = gamma((nu + d) / 2) / (gamma(nu / 2) * nu^(d / 2) * pi^(d / 2) * det(Sigma)^0.5) * (1 + (1 / nu) * x_minus_mu' * inv(Sigma) * x_minus_mu)^(-(nu + d) / 2);
      end
      
      ===== MultivariateT | Mathematica | pdf =====
      aDist=MultivariateTDistribution[{0, 0}, {{1, 0}, {0, 1}}, 10]
      PDF[aDist, x]
      
      ===== MultivariateT | Julia | pdf =====
      using Random, Distributions
      aDist=MvTDist(10, [0, 0], float([[1 0]; [0 1]]))
      pdf(aDist, x)
      
      ===== MultivariateT | LaTeX | pdf =====
      \mathrm{E}(X) = \begin{cases}
      \mu, & \nu>1 \\
      \text{undefined}, & \text{otherwise}
      \end{cases}
      
      ===== Wishart | R | pdf =====
      library(LaplacesDemon)
      # note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)
      dwishart(x, 8, S)
      
      ===== Wishart | Python | pdf =====
      import scipy.stats
      # note x should be symmetric positive-definite matrix of same dimensions as S (also sym, pos-def)
      scipy.stats.wishart.pdf(x, 8, S)
      
      ===== Wishart | Stan | pdf =====
      // S is symmetric and positive-definite
      exp(wishart_lpdf(x| 8, S))
      
      ===== Wishart | Matlab | pdf =====
      % calling function (S must be symmetric and positive definite)
      wishartpdf(x, 8, S)
       
      function g = multivariate_gamma(p, a)
          j = 1:p;
          g = gamma(a + (1 - j) / 2);
          g = pi^(p * (p - 1) / 4) * prod(g);
      end
       
      function f = wishartpdf(x, nu, S)
          m = size(x);
          d = m(1);
          if nu < d - 1
              f = 0;
          else
              f = det(x)^((nu - d - 1) / 2) * exp(-trace(inv(S) * x) / 2) * 1 / (2^(nu * d / 2) * det(S)^(nu / 2) * multivariate_gamma(d, nu / 2));
          end
      end
      
      ===== Wishart | Mathematica | pdf =====
      (* S must be symmetric and positive definite *)
      Needs[ "MultivariateStatistics`"];
      aDist=WishartDistribution[S, 8]
      PDF[aDist, x]
      
      ===== Wishart | Julia | pdf =====
      using Random, Distributions
      # S must be symmetric and positive definite and of size 4x4
      aDist=Wishart(8, S)
      pdf(aDist, x)
      
      ===== Wishart | LaTeX | pdf =====
      \mathrm{E}(X) = \nu \Psi
      

# generated R/Python/Stan code is stable across all properties

    Code
      cat(dump, sep = "\n")
    Output
      ===== Normal | R | pdf =====
      dnorm(x, 0, 1)
      
      ===== Normal | Python | pdf =====
      import scipy.stats
      scipy.stats.norm.pdf(x, 0, 1)
      
      ===== Normal | Stan | pdf =====
      exp(normal_lpdf(x| 0, 1))
      
      ===== Normal | R | log_pdf =====
      dnorm(x, 0, 1, log=TRUE)
      
      ===== Normal | Python | log_pdf =====
      import scipy.stats
      scipy.stats.norm.logpdf(x, 0, 1)
      
      ===== Normal | Stan | log_pdf =====
      normal_lpdf(x| 0, 1)
      
      ===== Normal | R | random =====
      rnorm(n, 0, 1)
      
      ===== Normal | Python | random =====
      import scipy.stats
      scipy.stats.norm.rvs(0, 1, n)
      
      ===== Normal | Stan | random =====
      // repeat following n times (for example, in for loop)
      normal_rng(0, 1)
      
      ===== Uniform | R | pdf =====
      dunif(x, 0, 1)
      
      ===== Uniform | Python | pdf =====
      import scipy.stats
      scipy.stats.uniform.pdf(x, 0, 1)
      
      ===== Uniform | Stan | pdf =====
      exp(uniform_lpdf(x| 0, 1))
      
      ===== Uniform | R | log_pdf =====
      dunif(x, 0, 1, log=TRUE)
      
      ===== Uniform | Python | log_pdf =====
      import scipy.stats
      scipy.stats.uniform.logpdf(x, 0, 1)
      
      ===== Uniform | Stan | log_pdf =====
      uniform_lpdf(x| 0, 1)
      
      ===== Uniform | R | random =====
      runif(n, 0, 1)
      
      ===== Uniform | Python | random =====
      import scipy.stats
      scipy.stats.uniform.rvs(0, 1, n)
      
      ===== Uniform | Stan | random =====
      // repeat following n times (for example, in for loop)
      uniform_rng(0, 1)
      
      ===== LogNormal | R | pdf =====
      dlnorm(x, 0, 1)
      
      ===== LogNormal | Python | pdf =====
      import scipy.stats
      scipy.stats.lognorm.pdf(x, scale=1, s=1)
      
      ===== LogNormal | Stan | pdf =====
      exp(lognormal_lpdf(x| 0, 1))
      
      ===== LogNormal | R | log_pdf =====
      dlnorm(x, 0, 1, log=TRUE)
      
      ===== LogNormal | Python | log_pdf =====
      import scipy.stats
      scipy.stats.lognorm.logpdf(x, scale=1, s=1)
      
      ===== LogNormal | Stan | log_pdf =====
      lognormal_lpdf(x| 0, 1)
      
      ===== LogNormal | R | random =====
      rlnorm(n, 0, 1)
      
      ===== LogNormal | Python | random =====
      import scipy.stats
      scipy.stats.lognorm.rvs(scale=1, s=1, size=n)
      
      ===== LogNormal | Stan | random =====
      // repeat following n times (for example, in for loop)
      lognormal_rng(0, 1)
      
      ===== Exponential | R | pdf =====
      dexp(x, 0.5)
      
      ===== Exponential | Python | pdf =====
      import scipy.stats
      scipy.stats.expon.pdf(x, loc=0, scale=2)
      
      ===== Exponential | Stan | pdf =====
      exp(exponential_lpdf(x| 0.5))
      
      ===== Exponential | R | log_pdf =====
      dexp(x, 0.5, log=TRUE)
      
      ===== Exponential | Python | log_pdf =====
      import scipy.stats
      scipy.stats.expon.logpdf(x, loc=0, scale=2)
      
      ===== Exponential | Stan | log_pdf =====
      exponential_lpdf(x| 0.5)
      
      ===== Exponential | R | random =====
      rexp(n, 0.5)
      
      ===== Exponential | Python | random =====
      import numpy
      numpy.random.exponential(2, n)
      
      ===== Exponential | Stan | random =====
      // repeat following n times (for example, in for loop)
      exponential_rng(0.5)
      
      ===== Gamma | R | pdf =====
      dgamma(x, 1, 0.5)
      
      ===== Gamma | Python | pdf =====
      import scipy.stats
      scipy.stats.gamma.pdf(x, a=1, loc=0, scale=2)
      
      ===== Gamma | Stan | pdf =====
      exp(gamma_lpdf(x| 1, 0.5))
      
      ===== Gamma | R | log_pdf =====
      dgamma(x, 1, 0.5, log=TRUE)
      
      ===== Gamma | Python | log_pdf =====
      import scipy.stats
      scipy.stats.gamma.logpdf(x, a=1, loc=0, scale=2)
      
      ===== Gamma | Stan | log_pdf =====
      gamma_lpdf(x| 1, 0.5)
      
      ===== Gamma | R | random =====
      rgamma(n, 1, 0.5)
      
      ===== Gamma | Python | random =====
      import numpy
      numpy.random.gamma(1, 2, n)
      
      ===== Gamma | Stan | random =====
      // repeat following n times (for example, in for loop)
      gamma_rng(1, 0.5)
      
      ===== t | R | pdf =====
      library(LaplacesDemon)
      dst(x, 0, 1, 3)
      
      ===== t | Python | pdf =====
      import scipy.stats
      scipy.stats.t.pdf(x, 3, 0, 1)
      
      ===== t | Stan | pdf =====
      exp(student_t_lpdf(x| 3, 0, 1))
      
      ===== t | R | log_pdf =====
      library(LaplacesDemon)
      dst(x, 0, 1, 3, log=TRUE)
      
      ===== t | Python | log_pdf =====
      import scipy.stats
      scipy.stats.t.logpdf(x, 3, 0, 1)
      
      ===== t | Stan | log_pdf =====
      student_t_lpdf(x| 3, 0, 1)
      
      ===== t | R | random =====
      library(LaplacesDemon)
      rst(n, 0, 1, 3)
      
      ===== t | Python | random =====
      import scipy.stats
      scipy.stats.t.rvs(3, 0, 1, n)
      
      ===== t | Stan | random =====
      // repeat following n times (for example, in for loop)
      student_t_rng(3, 0, 1)
      
      ===== Beta | R | pdf =====
      dbeta(x, 1, 1)
      
      ===== Beta | Python | pdf =====
      import scipy.stats
      scipy.stats.beta.pdf(x, 1, 1)
      
      ===== Beta | Stan | pdf =====
      exp(beta_lpdf(x| 1, 1))
      
      ===== Beta | R | log_pdf =====
      dbeta(x, 1, 1, log=TRUE)
      
      ===== Beta | Python | log_pdf =====
      import scipy.stats
      scipy.stats.beta.logpdf(x, 1, 1)
      
      ===== Beta | Stan | log_pdf =====
      beta_lpdf(x| 1, 1)
      
      ===== Beta | R | random =====
      rbeta(n, 1, 1)
      
      ===== Beta | Python | random =====
      import numpy
      numpy.random.beta(1, 1, n)
      
      ===== Beta | Stan | random =====
      // repeat following n times (for example, in for loop)
      beta_rng(1, 1)
      
      ===== Cauchy | R | pdf =====
      dcauchy(x, 0, 1)
      
      ===== Cauchy | Python | pdf =====
      import scipy.stats
      scipy.stats.cauchy.pdf(x, 0, 1)
      
      ===== Cauchy | Stan | pdf =====
      exp(cauchy_lpdf(x| 0, 1))
      
      ===== Cauchy | R | log_pdf =====
      dcauchy(x, 0, 1, log=TRUE)
      
      ===== Cauchy | Python | log_pdf =====
      import scipy.stats
      scipy.stats.cauchy.logpdf(x, 0, 1)
      
      ===== Cauchy | Stan | log_pdf =====
      cauchy_lpdf(x| 0, 1)
      
      ===== Cauchy | R | random =====
      rcauchy(n, 0, 1)
      
      ===== Cauchy | Python | random =====
      import scipy.stats
      scipy.stats.cauchy.rvs(0, 1, n)
      
      ===== Cauchy | Stan | random =====
      // repeat following n times (for example, in for loop)
      cauchy_rng(0, 1)
      
      ===== HalfCauchy | R | pdf =====
      dhalfcauchy <- function(x, location, scale, log=FALSE){
        if(x >= 0)
          val <- 1.0 / (pi * (1 + ((x - location) / scale)^2) * scale * (0.5 + atan(location / scale) / pi))
        else
          val <- 0.0
        if(!log)
          return(val)
        else
          return(log(val))
      }
      # calling function
      dhalfcauchy(x, 0, 1)
      
      ===== HalfCauchy | Python | pdf =====
      import numpy
      def halfcauchy_pdf(x, location, scale):
          if x >= 0:
              val = 1.0 / (numpy.pi * (1 + ((x - location) / scale)**2) * scale * (0.5 + numpy.arctan(location / scale) / numpy.pi))
          else:
              val = 0.0
          return val
      # calling function
      halfcauchy_pdf(x, 0, 1)
      
      ===== HalfCauchy | Stan | pdf =====
      exp(cauchy_lpdf(x| 0, 1) - cauchy_lccdf(0| 0, 1))
      
      ===== HalfCauchy | R | log_pdf =====
      dhalfcauchy <- function(x, location, scale, log=FALSE){
        if(x >= 0)
          val <- 1.0 / (pi * (1 + ((x - location) / scale)^2) * scale * (0.5 + atan(location / scale) / pi))
        else
          val <- 0.0
        if(!log)
          return(val)
        else
          return(log(val))
      }
      # calling function
      dhalfcauchy(x, 0, 1, log=TRUE)
      
      ===== HalfCauchy | Python | log_pdf =====
      import numpy
      def halfcauchy_logpdf(x, location, scale):
          if x >= 0:
              val = 1.0 / (numpy.pi * (1 + ((x - location) / scale)**2) * scale * (0.5 + numpy.arctan(location / scale) / numpy.pi))
          else:
              val = 0.0
          return numpy.log(val)
      # calling function
      halfcauchy_logpdf(x, 0, 1)
      
      ===== HalfCauchy | Stan | log_pdf =====
      cauchy_lpdf(x| 0, 1)  -  cauchy_lccdf(0| 0, 1)
      
      ===== HalfCauchy | R | random =====
      rhalfcauchy <- function(n, location, scale){
        r <- vector(length=n)
        for(i in 1:n){
          r_1 <- rcauchy(1, location, scale)
          while(r_1 < 0)
            r_1 <- rcauchy(1, location, scale)
          r[i] <- r_1
        }
        return(r)
        }
      # calling function
      rhalfcauchy(n, 0, 1)
      
      ===== HalfCauchy | Python | random =====
      import scipy.stats
      import numpy
      def halfcauchy_rvs(location, scale, n):
          x = numpy.zeros(n)
          for i in range(n):
              z = scipy.stats.cauchy.rvs(location, scale, 1)
              while z < 0:
                  z = scipy.stats.cauchy.rvs(location, scale, 1)
              x[i] = z
          return x
      # calling function
      halfcauchy_rvs(0, 1, n)
      
      ===== HalfCauchy | Stan | random =====
      // repeat following n times (for example, in for loop)
      real x_rng = cauchy_rng(0, 1);
      while(x_rng < 0)
        x_rng = cauchy_rng(0, 1);
      
      ===== InverseGamma | R | pdf =====
      library(actuar)
      dinvgamma(x, 2, 1)
      
      ===== InverseGamma | Python | pdf =====
      import scipy.stats
      scipy.stats.invgamma.pdf(x, a=2, loc=0, scale=1)
      
      ===== InverseGamma | Stan | pdf =====
      exp(inv_gamma_lpdf(x| 2, 1))
      
      ===== InverseGamma | R | log_pdf =====
      library(actuar)
      dinvgamma(x, 2, 1, log=TRUE)
      
      ===== InverseGamma | Python | log_pdf =====
      import scipy.stats
      scipy.stats.invgamma.logpdf(x, a=2, loc=0, scale=1)
      
      ===== InverseGamma | Stan | log_pdf =====
      inv_gamma_lpdf(x| 2, 1)
      
      ===== InverseGamma | R | random =====
      library(actuar)
      rinvgamma(n, 2, 1)
      
      ===== InverseGamma | Python | random =====
      import scipy.stats
      scipy.stats.invgamma.rvs(a=2, loc=0, scale=1, size=n)
      
      ===== InverseGamma | Stan | random =====
      // repeat following n times (for example, in for loop)
      inv_gamma_rng(2, 1)
      
      ===== InverseChiSquared | R | pdf =====
      library(LaplacesDemon)
      dinvchisq(x, 3)
      
      ===== InverseChiSquared | Python | pdf =====
      import numpy
      import scipy.special
      def inversechisquared_pdf(x, df):
          temp = df / 2.0
          val = (2**(-temp) / scipy.special.gamma(temp)) * x**-(temp + 1) * numpy.exp(-1.0 / (2 * x))
          return val
      # calling function
      inversechisquared_pdf(x, 3)
      
      ===== InverseChiSquared | Stan | pdf =====
      exp(inv_chi_square_lpdf(x| 3))
      
      ===== InverseChiSquared | R | log_pdf =====
      library(LaplacesDemon)
      dinvchisq(x, 3, log=TRUE)
      
      ===== InverseChiSquared | Python | log_pdf =====
      import numpy
      import scipy.special
      def inversechisquared_logpdf(x, df):
          temp = df / 2.0
          val = (2**(-temp) / scipy.special.gamma(temp)) * x**-(temp + 1) * numpy.exp(-1.0 / (2 * x))
          return numpy.log(val)
      # calling function
      inversechisquared_logpdf(x, 3)
      
      ===== InverseChiSquared | Stan | log_pdf =====
      inv_chi_square_lpdf(x| 3)
      
      ===== InverseChiSquared | R | random =====
      library(LaplacesDemon)
      rinvchisq(n, 3)
      
      ===== InverseChiSquared | Python | random =====
      import scipy.stats
      def inversechisquared_rvs(df, n=1):
          r = scipy.stats.chi2.rvs(df, 0, 1, n)
          return 1.0 / r
      # calling function
      inversechisquared_rvs(3, n)
      
      ===== InverseChiSquared | Stan | random =====
      // repeat following n times (for example, in for loop)
      inv_chi_square_rng(3)
      
      ===== LogitNormal | R | pdf =====
      library(logitnorm)
      dlogitnorm(x, 1, 1)
      
      ===== LogitNormal | Python | pdf =====
      import numpy
      import scipy.special
      def logitnormal_pdf(x, mu, sigma):
          temp = ((scipy.special.logit(x) - mu)**2 / (2 * sigma**2))
          return (1.0 / sigma) * (1.0 / numpy.sqrt(2 * numpy.pi)) * numpy.exp(-temp) * (1.0 / (x * (1.0 - x)))
      # calling function
      logitnormal_pdf(x, 1, 1)
      
      ===== LogitNormal | Stan | pdf =====
      functions{
        real logitnormal_lpdf(real x, real mu, real sigma){
          real temp = (logit(x) - mu)^2 / (2 * sigma^2);
          if(sigma < 0)
            return(log(0));
          return(-log(sigma) - 0.5 * log(2 * pi()) - temp - log(x) - log(1.0 - x));
        }
      }
      // calling function
      exp(logitnormal_lpdf(x| 1, 1))
      
      ===== LogitNormal | R | log_pdf =====
      library(logitnorm)
      dlogitnorm(x, 1, 1, log=TRUE)
      
      ===== LogitNormal | Python | log_pdf =====
      import numpy
      import scipy.special
      def logitnormal_logpdf(x, mu, sigma):
          temp = ((scipy.special.logit(x) - mu)**2 / (2 * sigma**2))
          return numpy.log((1.0 / sigma) * (1.0 / numpy.sqrt(2 * numpy.pi)) * numpy.exp(-temp) * (1.0 / (x * (1.0 - x))))
      # calling function
      logitnormal_logpdf(x, 1, 1)
      
      ===== LogitNormal | Stan | log_pdf =====
      functions{
        real logitnormal_lpdf(real x, real mu, real sigma){
          real temp = (logit(x) - mu)^2 / (2 * sigma^2);
          if(sigma < 0)
            return(log(0));
          return(-log(sigma) - 0.5 * log(2 * pi()) - temp - log(x) - log(1.0 - x));
        }
      }
      // calling function
      logitnormal_lpdf(x| 1, 1)
      
      ===== LogitNormal | R | random =====
      library(logitnorm)
      rlogitnorm(1, 1, n)
      
      ===== LogitNormal | Python | random =====
      import scipy.stats
      import numpy
      def logistic(x):
          return 1.0 / (1.0 + numpy.exp(-x))
      def logitnormal_rvs(mu, sigma, n=1):
          x = scipy.stats.norm.rvs(mu, sigma, n)
          p = [logistic(z) for z in x]
          return p
      # calling function
      logitnormal_rvs(1, 1, n)
      
      ===== LogitNormal | Stan | random =====
      // repeat following n times (for example, in for loop)
      inv_logit(normal_rng(1, 1))
      
      ===== Bernoulli | R | pdf =====
      dbinom(x, 1, 0.5)
      
      ===== Bernoulli | Python | pdf =====
      import scipy.stats
      scipy.stats.bernoulli.pmf(x, 0.5)
      
      ===== Bernoulli | Stan | pdf =====
      exp(bernoulli_lpmf(x| 0.5))
      
      ===== Bernoulli | R | log_pdf =====
      dbinom(x, 1, 0.5, log=TRUE)
      
      ===== Bernoulli | Python | log_pdf =====
      import scipy.stats
      scipy.stats.bernoulli.logpmf(x, 0.5)
      
      ===== Bernoulli | Stan | log_pdf =====
      bernoulli_lpmf(x| 0.5)
      
      ===== Bernoulli | R | random =====
      rbinom(n, 1, 0.5)
      
      ===== Bernoulli | Python | random =====
      import scipy.stats
      scipy.stats.bernoulli.rvs(0.5, 0, n)
      
      ===== Bernoulli | Stan | random =====
      // repeat following n times (for example, in for loop)
      bernoulli_rng(0.5)
      
      ===== BetaBinomial | R | pdf =====
      # function definition
      dbetabinom <- function(x, size, alpha, beta, log=FALSE){
        if(!log)
          return(choose(size, x) * beta(x + alpha, size - x + beta) / beta(alpha, beta))
        else
          return(lchoose(size, x) + lbeta(x + alpha, size - x + beta) - lbeta(alpha, beta))
      }
      # calling function
      dbetabinom(x, 10, 1, 1)
      
      ===== BetaBinomial | Python | pdf =====
      import scipy.special
      def betabinomial_pmf(x, size, a, b):
          return scipy.special.comb(size, x) * scipy.special.beta(x + a, size - x + b) / scipy.special.beta(a, b)
      # calling function
      betabinomial_pmf(x, 10, 1, 1)
      
      ===== BetaBinomial | Stan | pdf =====
      exp(beta_binomial_lpmf(x| 10, 1, 1))
      
      ===== BetaBinomial | R | log_pdf =====
      # function definition
      dbetabinom <- function(x, size, alpha, beta, log=FALSE){
        if(!log)
          return(choose(size, x) * beta(x + alpha, size - x + beta) / beta(alpha, beta))
        else
          return(lchoose(size, x) + lbeta(x + alpha, size - x + beta) - lbeta(alpha, beta))
      }
      # calling function
      dbetabinom(x, 10, 1, 1, log=TRUE)
      
      ===== BetaBinomial | Python | log_pdf =====
      from scipy.special import gammaln
      def betabinomial_logpmf(x, size, a, b):
          return (gammaln(size + 1) + gammaln(x + a) + gammaln(size - x + b) + gammaln(a + b) - 
              (gammaln(x + 1) + gammaln(size - x + 1) + gammaln(a) + gammaln(b) + gammaln(size + a + b)))
      # calling function
      betabinomial_logpmf(x, 10, 1, 1)
      
      ===== BetaBinomial | Stan | log_pdf =====
      beta_binomial_lpmf(x| 10, 1, 1)
      
      ===== BetaBinomial | R | random =====
      # function definition
      rbetabinom <- function(n, size, alpha, beta){
        theta <- rbeta(n, alpha, beta)
        return(rbinom(n, size, theta))
      }
      # calling function
      rbetabinom(n, 10, 1, 1)
      
      ===== BetaBinomial | Python | random =====
      import scipy.stats
      def betabinomial_rvs(size, a, b, n=1):
          thetas = scipy.stats.beta.rvs(a, b, 0, 1, n)
          x = [scipy.stats.binom.rvs(size, theta, 0, 1)[0] for theta in thetas]
          return x
      # calling function
      betabinomial_rvs(10, 1, 1, n)
      
      ===== BetaBinomial | Stan | random =====
      // repeat following n times (for example, in for loop)
      beta_binomial_rng(10, 1, 1)
      
      ===== Binomial | R | pdf =====
      dbinom(x, 10, 0.5)
      
      ===== Binomial | Python | pdf =====
      import scipy.stats
      scipy.stats.binom.pmf(x, 10, 0.5)
      
      ===== Binomial | Stan | pdf =====
      exp(binomial_lpmf(x| 10, 0.5))
      
      ===== Binomial | R | log_pdf =====
      dbinom(x, 10, 0.5, log=TRUE)
      
      ===== Binomial | Python | log_pdf =====
      import scipy.stats
      scipy.stats.binom.logpmf(x, 10, 0.5)
      
      ===== Binomial | Stan | log_pdf =====
      binomial_lpmf(x| 10, 0.5)
      
      ===== Binomial | R | random =====
      rbinom(n, 10, 0.5)
      
      ===== Binomial | Python | random =====
      import scipy.stats
      scipy.stats.binom.rvs(10, 0.5, 0, n)
      
      ===== Binomial | Stan | random =====
      // repeat following n times (for example, in for loop)
      binomial_rng(10, 0.5)
      
      ===== DiscreteUniform | R | pdf =====
      # function definition
      ddiscreteuniform <- function(x, min=0, max=1, log=FALSE){
      if(x >= min & x <= max & round(x) == x)
        if(!log)
          return(1 / (max - min + 1))
        else
          return(-log(max - min + 1))
      else
        if(!log)
          return(0.0)
        else
          return(-Inf)
      }
      # calling function
      ddiscreteuniform(x, 0, 1)
      
      ===== DiscreteUniform | Python | pdf =====
      import scipy.stats
      scipy.stats.randint.pmf(x, 0, 2)
      
      ===== DiscreteUniform | Stan | pdf =====
      functions{
        real discreteuniform_lpmf(int x, int lower, int upper){
          if(upper < lower)
            return(log(0));
          if(x < lower)
            return(log(0));
          if(x > upper)
            return(log(0));
          return(-log(upper - lower + 1));
        }
      }
      // calling function
      exp(discreteuniform_lpmf(x| 0, 1))
      
      ===== DiscreteUniform | R | log_pdf =====
      # function definition
      ddiscreteuniform <- function(x, min=0, max=1, log=FALSE){
      if(x >= min & x <= max & round(x) == x)
        if(!log)
          return(1 / (max - min + 1))
        else
          return(-log(max - min + 1))
      else
        if(!log)
          return(0.0)
        else
          return(-Inf)
      }
      # calling function
      ddiscreteuniform(x, 0, 1, log=TRUE)
      
      ===== DiscreteUniform | Python | log_pdf =====
      import scipy.stats
      scipy.stats.randint.logpmf(x, 0, 2)
      
      ===== DiscreteUniform | Stan | log_pdf =====
      functions{
        real discreteuniform_lpmf(int x, int lower, int upper){
          if(upper < lower)
            return(log(0));
          if(x < lower)
            return(log(0));
          if(x > upper)
            return(log(0));
          return(-log(upper - lower + 1));
        }
      }
      // calling function
      discreteuniform_lpmf(x| 0, 1)
      
      ===== DiscreteUniform | R | random =====
      # function definition
      rdiscreteuniform <- function(n, min=0, max=1){
        return(sample(min:max, n, replace=T))
      }
      rdiscreteuniform(n, 0, 1)
      
      ===== DiscreteUniform | Python | random =====
      import scipy.stats
      scipy.stats.randint.rvs(0, 2, loc=0, size=n)
      
      ===== DiscreteUniform | Stan | random =====
      functions{
        int discreteuniform_rng(int lower, int upper){
          int diff;
          int cat;
          int d = 0;
          if(upper < lower)
            return(not_a_number());
          diff = upper - lower + 1;
          cat = categorical_rng(rep_vector(1.0 / diff, diff));
          if(lower <= 0)
            d = 1 - lower;
          else if(lower > 1)
            d = -(lower - 1);
          return(cat - d);
        }
      }
      // calling function
      // repeat following n times (for example, in for loop)
      discreteuniform_rng(0, 1)
      
      ===== NegativeBinomial | R | pdf =====
      dnbinom(x, mu=10, size=3)
      
      ===== NegativeBinomial | Python | pdf =====
      import scipy.stats
      def negativebinomial_pmf(x, mu, kappa):
          n = kappa
          p = float(kappa) / (kappa + mu)
          return scipy.stats.nbinom.pmf(x, n, p)
      # calling function
      negativebinomial_pmf(x, 10, 3)
      
      ===== NegativeBinomial | Stan | pdf =====
      exp(neg_binomial_2_lpmf(x| 10, 3))
      
      ===== NegativeBinomial | R | log_pdf =====
      dnbinom(x, mu=10, size=3, log=TRUE)
      
      ===== NegativeBinomial | Python | log_pdf =====
      import scipy.stats
      def negativebinomial_logpmf(x, mu, kappa):
          n = kappa
          p = float(kappa) / (kappa + mu)
          return scipy.stats.nbinom.logpmf(x, n, p)
      # calling function
      negativebinomial_logpmf(x, 10, 3)
      
      ===== NegativeBinomial | Stan | log_pdf =====
      neg_binomial_2_lpmf(x| 10, 3)
      
      ===== NegativeBinomial | R | random =====
      rnbinom(n, mu=10, size=3)
      
      ===== NegativeBinomial | Python | random =====
      import scipy.stats
      def negativebinomial_rvs(mu, kappa, n=1):
          n1 = kappa
          p = float(kappa) / (kappa + mu)
          return scipy.stats.nbinom.rvs(n1, p, 0, n)
      # calling function
      negativebinomial_rvs(10, 3, n)
      
      ===== NegativeBinomial | Stan | random =====
      // repeat following n times (for example, in for loop)
      neg_binomial_2_rng(10, 3)
      
      ===== Poisson | R | pdf =====
      dpois(x, 10)
      
      ===== Poisson | Python | pdf =====
      import scipy.stats
      scipy.stats.poisson.pmf(x, 10)
      
      ===== Poisson | Stan | pdf =====
      exp(poisson_lpmf(x| 10))
      
      ===== Poisson | R | log_pdf =====
      dpois(x, 10, log=TRUE)
      
      ===== Poisson | Python | log_pdf =====
      import scipy.stats
      scipy.stats.poisson.logpmf(x, 10)
      
      ===== Poisson | Stan | log_pdf =====
      poisson_lpmf(x| 10)
      
      ===== Poisson | R | random =====
      rpois(n, 10)
      
      ===== Poisson | Python | random =====
      import scipy.stats
      scipy.stats.poisson.rvs(10, 0, n)
      
      ===== Poisson | Stan | random =====
      // repeat following n times (for example, in for loop)
      poisson_rng(10)
      

