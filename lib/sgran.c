#include "init_random_seed.h"

/* Fortran wrapper to seed the C library rand */
void sgran_(void)
{
  init_random_seed();
}
