#include <malloc.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
// change to larger number for larger matrices
//#define M 51 
#define M 401 
#ifdef MAIN
main(int argc, char *argv[])
{ int d,nsim,isim,i,j,seed,iprint;
  double rh[M][M],rr[M][M],eta,s1[M],s2[M],tem;
  void rcoronion(int, double, double [][M]);

  if(argc<2) iprint=0; else iprint=atoi(argv[1]);
  //eta=1;
  printf("onion\n");
  scanf("%d %d %d %lf", &d,&nsim,&seed,&eta);
  while(d>2)
  { printf("\nd=%d nsim=%d seed=%d eta=%f\n", d,nsim,seed,eta);
    for(i=1;i<d;i++) { s1[i]=0.; s2[i]=0.; }
    srandom(seed);
    for(isim=1;isim<=nsim;isim++)
    { rcoronion(d,eta,rr);
      if(isim<=iprint)
      { printf("\nIteration %d\n", isim);
        for(i=1;i<=d;i++)
        { for(j=1;j<=d;j++) printf("%f ", rr[i][j]); printf("\n"); }
      }
      for(i=1;i<d;i++) { tem=rr[i][i+1]; s1[i]+=tem; s2[i]+=tem*tem; }
    }
    for(i=1;i<d;i++) 
    { s1[i]/=nsim; s2[i]/=nsim; 
      s2[i]-=s1[i]*s1[i];
    }
    //tem=1./(d+1.);
    tem=1./(2.*eta+d-1.);
    for(i=1;i<d;i++) 
    { printf("r[%d,%d]: sample mean %f, sample var %f, theor. var %f\n",
          i,i+1,s1[i],s2[i], tem);
    }
    scanf("%d %d %d %lf", &d,&nsim,&seed,&eta);
  }
}
#endif

/* General References: 
Lewandowski D, Kurowicka D and Joe H (2009).
Generating random correlation matrices based on vines and extended
Onion method. J. Multivariate Analysis, to appear

Ghosh, S., Henderson, S. G. (2003). Behavior of the NORTA method for 
correlated random vector generation as the dimension increases. 
ACM Transactions on Modeling and Computer Simulation (TOMACS)
13 (3), 276--294.
*/

// Random correlation matrix with generalization of onion method 
/* input d>=2, eta>0 (eta=1 for uniform)
   output correlation matrix rr[][], density proportional to
   det(R)^{eta-1}
*/
void rcoronion(int d, double eta, double rr[][M])
{ double rho,beta,tem,v,z[M],zz,ss,w[M];
  double A[M][M];  // for Cholesky
  double rbeta2(double,double); // from R 1997 version
  double snorm(void);  // from R 1997 version
  void chol(double [][M], double [][M], int);
  void chol2(double [][M], double [][M], int);
  int i,k,m,m1;
  for(i=1;i<=d;i++) rr[i][i]=1;
  if(d<0) { printf("d should be positive integer\n"); return; }
  for(i=1;i<=d;i++) rr[i][i]=1;
  if(d==1) { return; }
  if(d==2) 
  { rho=2.*rbeta2(eta,eta)-1.;
    rr[1][2]=rho; rr[2][1]=rho; return;
  }
  // initialization for d>=3
  beta=eta+(d-2.)/2.;
  rr[1][2]=2.*rbeta2(beta,beta)-1.;
  rr[2][1]=rr[1][2];
  chol(rr,A,2);
  for(m=2;m<=d-1;m++)
  { beta=beta-0.5;
    v=rbeta2(m/2.,beta); 
    v=sqrt(v);
    for(i=1,ss=0.;i<=m;i++) { zz=snorm(); ss+=zz*zz; z[i]=zz; }
    ss=sqrt(ss);
    for(i=1;i<=m;i++) w[i]=v*z[i]/ss;
    m1=m+1;
    for(i=1;i<=m;i++) 
    { for(k=1,tem=0;k<=i;k++) tem+=A[i][k]*w[k];
      rr[i][m1]=tem; rr[m1][i]=tem;
    }
    chol2(rr,A,m+1);
  }
}


/* Cholesky's decomp. of a sym. S into A*A^T where A is lower triangular */
/* 0 row and column not used */
void chol(double s[][M], double b[][M], int n)
{ int i,j,k;
  double sum;
  b[1][1]=sqrt(s[1][1]);
  for(k=2;k<=n;k++)
  { for(i=1;i<k;i++)
    { sum=0.;
      for(j=1;j<i;j++) sum+=b[i][j]*b[k][j];
      if(fabs(s[k][i]-sum)>1.e-5) b[k][i]=(s[k][i]-sum)/b[i][i];
      else b[k][i]=0.;
      b[i][k]=0.;
    }
    for(j=1,sum=0.;j<k;j++)  sum+=b[k][j]*b[k][j];
    if(s[k][k]-sum<=0.) b[k][k]=0.;
    else b[k][k]=sqrt(s[k][k]-sum);
  }
}

/* incremental version of Cholesky's decomp, making use of Cholesky on
submatrix */
/* 0 row and column not used */
void chol2(double s[][M], double b[][M], int n)
{ int i,j,k;
  double sum;
  //b[1][1]=sqrt(s[1][1]);
  //for(k=2;k<=n;k++)
  k=n;
  { for(i=1;i<k;i++)
    { sum=0.;
      for(j=1;j<i;j++) sum+=b[i][j]*b[k][j];
      if(fabs(s[k][i]-sum)>1.e-5) b[k][i]=(s[k][i]-sum)/b[i][i];
      else b[k][i]=0.;
      b[i][k]=0.;
    }
    for(j=1,sum=0.;j<k;j++)  sum+=b[k][j]*b[k][j];
    if(s[k][k]-sum<=0.) b[k][k]=0.;
    else b[k][k]=sqrt(s[k][k]-sum);
  }
}

