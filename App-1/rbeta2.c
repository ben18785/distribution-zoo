#include <stdio.h>
#include <values.h>
#include <math.h>
/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* Reference:
 * R. C. H. Cheng (1978).
 * Generating beta variates with nonintegral shape parameters.
 * Communications of the ACM 21, 317-322.
 * (Algorithms BB and BC)
 */

//#include "Mathlib.h"

static double expmax = 0.0;

#define repeat for(;;)

double rbeta2(double aa, double bb)
{ static double a, b, delta, r, s, t, u1, u2, v, w, y, z;
  static double alpha, beta, gamma, k1, k2;
  static double olda = -1.0;
  static double oldb = -1.0;
  int qsame;
  double fmax2(double,double);
  double fmin2(double,double);
  
  if (expmax == 0.0) expmax = log(DBL_MAX);
  
  qsame = (olda == aa) && (oldb == bb);
  if (!qsame)
  if (aa > 0.0 && bb > 0.0)
  { olda = aa; oldb = bb; }
  //else { DOMAIN_ERROR; }
  if (fmin2(aa, bb) <= 1.0)
  { /* Algorithm BC */
    if (!qsame)
    { a = fmax2(aa, bb);
      b = fmin2(aa, bb);
      alpha = a + b;
      beta = 1.0 / b;
      delta = 1.0 + a - b;
      k1 = delta * (0.0138889 + 0.0416667 * b) / (a * beta - 0.777778);
      k2 = 0.25 + (0.5 + 0.25 / delta) * b;
    }
    repeat
    { u1 = rand()/2147483648.0;
      u2 = rand()/2147483648.0;
      if (u1 < 0.5)
      { y = u1 * u2;
        z = u1 * y;
        if (0.25 * u2 + z - y >= k1) continue;
      }
      else
      { z = u1 * u1 * u2;
        if (z <= 0.25) break;
        if (z >= k2) continue;
      }
      v = beta * log(u1 / (1.0 - u1));
      if (v <= expmax) w = a * exp(v);
      else w = DBL_MAX;
      if (alpha * (log(alpha / (b + w)) + v) - 1.3862944 >= log(z))
          goto deliver;
    }
    v = beta * log(u1 / (1.0 - u1));
    if (v <= expmax) w = a * exp(v);
    else w = DBL_MAX;
  }
  else
  { /* Algorithm BB */
    if (!qsame)
    { a = fmin2(aa, bb);
      b = fmax2(aa, bb);
      alpha = a + b;
      beta = sqrt((alpha - 2.0) / (2.0 * a * b - alpha));
      gamma = a + 1.0 / beta;
    }
    do
    { u1 = rand()/2147483648.0;
      u2 = rand()/2147483648.0;
      v = beta * log(u1 / (1.0 - u1));
      if (v <= expmax) w = a * exp(v);
      else w = DBL_MAX;
      z = u1 * u1 * u2;
      r = gamma * v - 1.3862944;
      s = a + r - w;
      if (s + 2.609438 >= 5.0 * z) break;
      t = log(z);
      if (s > t) break;
    }
    while (r + alpha * log(alpha / (b + w)) < t);
  }
  
  deliver:
  return (aa != a) ? b / (b + w) : w / (b + w);
}

double fmax2(double x, double y)
{ return (x < y) ? y : x; }

double fmin2(double x, double y)
{ return (x < y) ? x : y; }


/*  Kinderman A. J. and Ramage J. G. (1976).
 *  Computer generation of normal random variables.
 *  JASA 71, 893-896.
 */

#define C1		0.398942280401433
#define C2		0.180025191068563
#define g(x)		(C1*exp(-x*x/2.0)-C2*(a-fabs(x)))

#define max(a,b)	(((a)>(b))?(a):(b))
#define min(a,b)	(((a)<(b))?(a):(b))
static double a =  2.216035867166471;

double snorm(void)
{ double t, u1, u2, u3;
  double sunif();
  
  //u1 = sunif();
  u1 = rand()/2147483648.0;
  if( u1<0.884070402298758 )
  { u2 = rand()/2147483648.0;
    return a*(1.13113163544180*u1+u2-1);
  }
  
  if( u1>=0.973310954173898 )
  { tail: u2 = rand()/2147483648.0;
    u3 = rand()/2147483648.0;
    t = (a*a-2*log(u3));
    if( u2*u2<(a*a)/t )
        return (u1<0.986655477086949) ? sqrt(t) : -sqrt(t) ;
    goto tail;
  }
  
  if( u1>=0.958720824790463 )
  { region3:
    u2 = rand()/2147483648.0;
    u3 = rand()/2147483648.0;
    t = a-0.630834801921960*min(u2,u3);
    if( max(u2,u3)<=0.755591531667601 )
        return (u2<u3) ? t : -t ;
    if( 0.034240503750111*fabs(u2-u3)<=g(t) )
        return (u2<u3) ? t : -t ;
    goto region3;
  }
  
  if( u1>=0.911312780288703 )
  { region2:
    u2 = rand()/2147483648.0;
    u3 = rand()/2147483648.0;
    t = 0.479727404222441+1.105473661022070*min(u2,u3);
    if( max(u2,u3)<=0.872834976671790 ) return (u2<u3) ? t : -t ;
    if( 0.049264496373128*fabs(u2-u3)<=g(t) ) return (u2<u3) ? t : -t ;
    goto region2;
  }
  
  region1:
  u2 = rand()/2147483648.0;
  u3 = rand()/2147483648.0;
  t = 0.479727404222441-0.595507138015940*min(u2,u3);
  if( max(u2,u3)<=0.805577924423817 ) return (u2<u3) ? t : -t ;
  goto region1;
}

