# extension of onion method, using Cholesky instead of msqrt
# Lewandowski D, Kurowicka D and Joe H (2009).
# Generating random correlation matrices based on vines and extended
# Onion method. J. Multivariate Analysis, to appear.

#Ghosh, S., Henderson, S. G. (2003). Behavior of the NORTA method for 
#correlated random vector generation as the dimension increases. 
#ACM Transactions on Modeling and Computer Simulation (TOMACS)
#v 13 (3), 276-294.

# qq plot against beta(alp,alp) distribution
qqbeta=function(x,alp)
{ xs=sort(x)
  n=length(x)
  pp=(1:n)/(n+1)
  qq=qbeta(pp,alp,alp)
  plot(qq,xs,ylab="corr",xlab="beta quantile")
  title(paste("Beta quantile plot with a=b=",alp))
  0
}


# input d>=2, eta>0 (eta=1 for uniform)
# output correlation matrix rr[][], density proportional to
#   det(R)^{eta-1}
rcoronion<-function(d,eta=1)
{ 
  d<-as.integer(d)
  if(d<=0 || !is.integer(d))
  { stop("The dimension 'd' should be a positive integer!\n") }
  if(eta<=0)
  { stop("'eta' should be positive!\n") }

  #handling of d=1 and d=2
  if(d==1) 
  { rr<-matrix(1,1,1); return(rr) }
  if(d==2) 
  { rho<-2*rbeta(1,eta,eta)-1
    rr<-matrix(c(1,rho,rho,1),2,2); return(rr) 
  }
  rr<-matrix(0,d,d)
  beta<-eta+(d-2)/2
  # step 1
  r12<-2*rbeta(1,beta,beta)-1
  rr<-matrix(c(1,r12,r12,1),2,2)
  # iterative steps
  for(m in 2:(d-1))
  { beta<-beta-0.5
    y<-rbeta(1,m/2,beta)
    z<-rnorm(m,0,1)
    znorm<-sqrt(sum(z^2))
    # random on surface of unit sphere
    z<-z/znorm
    w=sqrt(y)*z
    # can spped up by programming incremental Cholesky?
    rhalf<-chol(rr)
    qq<-w%*%rhalf
    rr<-cbind(rr,t(qq))
    rr<-rbind(rr,c(qq,1))
  }
  # return rr
  rr
}

set.seed(1234)
nsim=5000
#nsim=200
d=5
eta=d/2
#eta=2.2
#eta=3.5
out5=matrix(0,nsim,d^2)
for(i in 1:nsim)
{ out5[i,]=c(rcoronion(d,eta)) }
#d=5, OK
par(mfrow=c(3,3))
qqbeta(out5[,2],eta)
qqbeta(out5[,3],eta)
qqbeta(out5[,4],eta)
qqbeta(out5[,5],eta)
qqbeta(out5[,8],eta)
qqbeta(out5[,9],eta)
qqbeta(out5[,10],eta)
qqbeta(out5[,14],eta)
qqbeta(out5[,15],eta)
lTest <- lapply(seq(1,10000,1), function(x) rcoronion(5,10))
lTest <- lapply(seq(1,10000,1), function(x) rcorcvine(5,1))
