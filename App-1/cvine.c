#include <stdio.h>
#include <stdlib.h>
#include <math.h>
// change to larger number for larger matrices
//#define M 51
#define M 401
#ifdef MAIN
main(int argc, char *argv[])
{ int d,nsim,isim,i,j,seed,iprint;
  double rh[M][M],rr[M][M],eta,s1[M],s2[M],tem;
  void rcorcvine(int, double, double [][M]);

  if(argc<2) iprint=0; else iprint=atoi(argv[1]);
  //eta=1;
  printf("cvine\n");
  scanf("%d %d %d %lf", &d,&nsim,&seed,&eta);
  while(d>2)
  { printf("\nd=%d nsim=%d seed=%d eta=%f\n", d,nsim,seed,eta);
    for(i=1;i<d;i++) { s1[i]=0.; s2[i]=0.; }
    srandom(seed);
    for(isim=1;isim<=nsim;isim++)
    { rcorcvine(d,eta,rr);
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
    tem=1./(2*eta+d-1.);
    for(i=1;i<d;i++) 
    { printf("r[%d,%d]: sample mean %f, sample var %f, theor. var %f\n",
          i,i+1,s1[i],s2[i], tem);
    }
    scanf("%d %d %d %lf", &d,&nsim,&seed,&eta);
  }
}
#endif

/* Lewandowski D, Kurowicka D and Joe H (2009).
   Generating random correlation matrices based on vines and extended
   Onion method. J. Multivariate Analysis, to appear. 
*/

// Random correlation with the C-vine 
/* input d>=2, eta>0 (eta=1 for uniform)
   output correlation matrix rr[][], density proportional to
   det(R)^{eta-1}
*/
void rcorcvine(int d, double eta, double rr[][M])
{ double rho,alp,temm,temi,tem;
  double prr[M][M];  // for partial correlations
  double rbeta2(double,double); 
  int i,k,m;
  if(d<0) { printf("d should be positive integer\n"); return; }
  for(i=1;i<=d;i++) rr[i][i]=1;
  if(d==1) { return; }
  if(d==2) 
  { rho=2.*rbeta2(eta,eta)-1.;
    rr[1][2]=rho; rr[2][1]=rho; return;
  }
  // matrix of partial correlations as generated
  for(i=2;i<=d;i++)
  { alp=eta+(d-2.)/2.;
    rr[1][i]=2.*rbeta2(alp,alp)-1.;
    rr[i][1]=rr[1][i];
    prr[1][i]=rr[1][i];
  }
  for(m=2;m<=d-1;m++)
  { alp=eta+(d-1.-m)/2;
    for(i=m+1;i<=d;i++)
    { 
      prr[m][i]=2.*rbeta2(alp,alp)-1.;
      // back calculate thru lower order partials
      tem=prr[m][i];
      for(k=m-1;k>=1;k--)
      { temm=prr[k][m]; temi=prr[k][i];
        tem=temm*temi+tem*sqrt((1.-temm*temm)*(1.-temi*temi)); 
      }
      rr[m][i]=tem;
      rr[i][m]=rr[m][i];
    }
  }
}

