#include <stdio.h>
#include <limits.h>

/* Original code copied from 
   http://rosettacode.org/wiki/Evaluate_binomial_coefficients
*/
 
/* We go to some effort to handle overflow situations */
 
static unsigned long long gcd_ui(unsigned long long x, unsigned long long y) {
  unsigned long long t;
  if (y < x) { t = x; x = y; y = t; }
  while (y > 0) {
    t = y;  y = x % y;  x = t;  /* y1 <- x0 % y0 ; x1 <- y0 */
  }
  return x;
}
 
unsigned long long binomial(unsigned long long n, unsigned long long k) {
  unsigned long long d, g, r = 1;
  if (k == 0) return 1;
  if (k == 1) return n;
  if (k >= n) return (k == n);
  if (k > n/2) k = n-k;
  for (d = 1; d <= k; d++) {
    if (r >= ULLONG_MAX/n) {  /* Possible overflow */
      unsigned long long nr, dr;  /* reduced numerator / denominator */
      g = gcd_ui(n, d);  nr = n/g;  dr = d/g;
      g = gcd_ui(r, dr);  r = r/g;  dr = dr/g;
      if (r >= ULLONG_MAX/nr) return 0;  /* Unavoidable overflow */
      r *= nr;
      r /= dr;
      n--;
    } else {
      r *= n--;
      r /= d;
    }
  }
  return r;
}

unsigned long long binomial_(int *n, int *k)
{
  //  printf("n=%d  k=%d  %lu\n",*n,*k,binomial(*n,*k));
  return binomial(*n,*k);
}

double hypergeo_(int *x, int *NN, int *XX, int *s)
{    
  double a,b,c;
  a=(double)binomial(*XX, *x);
  b=(double)binomial(*NN-*XX, *s-*x);
  c=(double)binomial(*NN, *s);
  return a*b/c;
}
