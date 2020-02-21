#ifndef INIT_RANDOM_SEED_H__
#define INIT_RANDOM_SEED_H__

#ifdef __cplusplus
extern "C" {
#endif

  /*
   * Generate a seed for the RANDOM_NUMBER PRNG that is guaranteed to
   * be unique even if many processes are started simultaneously
   *
   * Not suitable for multi-threaded requirements
   */
  void init_random_seed (void);

#ifdef __cplusplus
}
#endif

#endif
