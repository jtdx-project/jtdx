#include <stdio.h>
#include <limits.h>

/* Original code copied from 
   http://rosettacode.org/wiki/Evaluate_binomial_coefficients
*/
 
/* We go to some effort to handle overflow situations */
 
static unsigned long gcd_ui(unsigned long x, unsigned long y) {
  unsigned long t;
  if (y < x) { t = x; x = y; y = t; }
  while (y > 0) {
    t = y;  y = x % y;  x = t;  /* y1 <- x0 % y0 ; x1 <- y0 */
  }
  return x;
}
 
unsigned long binomial(unsigned long n, unsigned long k) {
  unsigned long d, g, r = 1;
  if (k == 0) return 1;
  if (k == 1) return n;
  if (k >= n) return (k == n);
  if (k > n/2) k = n-k;
  for (d = 1; d <= k; d++) {
    if (r >= ULONG_MAX/n) {  /* Possible overflow */
      unsigned long nr, dr;  /* reduced numerator / denominator */
      g = gcd_ui(n, d);  nr = n/g;  dr = d/g;
      g = gcd_ui(r, dr);  r = r/g;  dr = dr/g;
      if (r >= ULONG_MAX/nr) return 0;  /* Unavoidable overflow */
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
 
int main() {

  //Get test results
  printf("%lu\n", binomial(5, 3));     // 10
  printf("%lu\n", binomial(40, 19));   // 131282408400
  printf("%lu\n", binomial(67, 31));   // 11923179284862717872

  // Compute special cases for paper on TF soft-decision RS decoder:
  double a,b,c,p;
  a=(double)binomial(40, 35);
  b=(double)binomial(23, 5);
  c=(double)binomial(63, 40);
  p=a*b/c;
  printf("%e   %e   %e   %e\n",a,b,c,p);

  a=(double)binomial(40, 36);
  b=(double)binomial(23, 4);
  c=(double)binomial(63, 40);
  p=a*b/c;
  printf("%e   %e   %e   %e\n",a,b,c,p);

  a=(double)binomial(40, 37);
  b=(double)binomial(23, 8);
  c=(double)binomial(63, 45);
  p=a*b/c;
  printf("%e   %e   %e   %e\n",a,b,c,p);
  return 0;
}
