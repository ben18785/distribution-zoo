# General Reference: 

# Lewandowski D, Kurowicka D and Joe H (2009).
# Generating random correlation matrices based on vines and extended
# Onion method. J. Multivariate Analysis, to appear.

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
rcorcvine<-function(d,eta=1)
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
  # matrix of partial correlations as generated
  prr<-matrix(0,d,d)
  diag(rr)<-1
  for(i in 2:d) 
  { alp<-eta+(d-2)/2
    rr[1,i]<-2*rbeta(1,alp,alp)-1
    rr[i,1]<-rr[1,i]
    prr[1,i]<-rr[1,i]
  }
  for(m in 2:(d-1))
  { alp<-eta+(d-1-m)/2
    for(i in (m+1):d)
    { prr[m,i]<-2*rbeta(1,alp,alp)-1
      # back calculate thru lower order partials
      tem<-prr[m,i]
      for(k in (m-1):1)
      { tem<-prr[k,m]*prr[k,i]+tem*sqrt((1-prr[k,m]^2)*(1-prr[k,i]^2)) }
      rr[m,i]<-tem
      rr[i,m]<-rr[m,i]
    }
  }
  return(rr)
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
{ out5[i,]=c(rcorcvine(d,eta)) }
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

