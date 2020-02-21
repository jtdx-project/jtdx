  integer :: level, onlevel(0:10)
  common/timer_private/ level, onlevel
  !$omp threadprivate(/timer_private/)
