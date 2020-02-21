int igray_(int *n0, int *idir)
{
  int n;
  unsigned long sh;
  unsigned long nn;
  n=*n0;

  if(*idir>0) return (n ^ (n >> 1));

  sh = 1;
  nn = (n >> sh);
  while (nn > 0) {
    n ^= nn;
    sh <<= 1;
    nn = (n >> sh);
  }
  return (n);
}
