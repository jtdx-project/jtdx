#include "init_random_seed.h"

#include <stdlib.h>
#include <stdint.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h>
#include <unistd.h>

/* basic PRNG to use for improving the basic seed selection */
static unsigned lcg (uint64_t seed)
{
  if (0 ==seed)
    {
      seed = UINT64_C(104729);
    }
  else
    {
      seed %= UINT64_C(4294967296);
    }
  seed = (seed * UINT64_C(279470273)) % UINT64_C(4294967291);
  return seed % UINT64_MAX;
}

/* Generate a good PRNG seed value */
void init_random_seed(void)
{
  unsigned seed = 0u;
  int have_seed = 0;

  // try /dev/urandom for an initial seed
  int random_source;
  if ((random_source = open ("/dev/urandom", O_RDONLY)) >= 0)
    {
      size_t random_data_length = 0;
      have_seed = -1;
      while (random_data_length < sizeof seed)
        {
          ssize_t result = read (random_source, &seed + random_data_length, (sizeof seed) - random_data_length);
          if (result < 0)
            {
              // error, unable to read /dev/random
              have_seed = 0;
            }
          random_data_length += result;
        }
      close (random_source);
    }
  if (!have_seed)
    {
      // fallback to combining the time and PID in a fairly random way
      pid_t pid = getpid ();
      struct timeval tv;
      gettimeofday (&tv, NULL);
      seed = (unsigned)(((unsigned)pid << 16)
        ^ (unsigned)pid
        ^ (unsigned)tv.tv_sec
        ^ (unsigned)tv.tv_usec);
      seed = lcg (seed);
    }
  srand (seed);
}

#ifdef TEST
#include <stdio.h>

int main (int argc, char * argv[])
{
  init_random_seed ();
  int i, j;
  int r[10][4];
  for (i = 0; i < 10; ++i)
    {
      for (j = 0; j < 4; ++j)
        {
          printf ("%10d ", rand ());
        }
      printf ("\n");
    }
  return 0;
}
#endif
