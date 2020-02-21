subroutine timefft_opts(npatience,nthreads,linplace,lcomplex,nfft,    &
     problem,nflags)

  use FFTW3

  logical linplace,lcomplex
  character problem*9,arg*12

  nargs=iargc()
  if(nargs.lt.3) then
     print*,'Usage: timefft npatience maxthreads [[o|i][r|c]]nfft'
     print*,'       npatience  - 0 to 4'
     print*,'       maxthreads - suggest #CPUs or #CPUs-1'
     print*,'       o,i        - out-of-place or in-place (default=in-place)'
     print*,'       r,c        - real or complex (default=complex)'
     print*,' '
     print*,'Examples:'
     print*,'       timefft 1 1 32768     (1 thread, in-place, complex)'
     print*,'       timefft 2 3 or32768   (more patient, 3 threads,'
     print*,'                              out-of-place, real)'
     stop
  endif

  call getarg(1,arg)
  read(arg,*) npatience
  call getarg(2,arg)
  read(arg,*) nthreads
  call getarg(3,arg)
  linplace=arg(1:1).ne.'o' .and. arg(2:2).ne.'o'
  lcomplex=arg(1:1).ne.'r' .and. arg(2:2).ne.'r'
  k=3
  if(ichar(arg(2:2)).ge.48 .and. ichar(arg(2:2)).le.57) k=2
  if(ichar(arg(1:1)).ge.48 .and. ichar(arg(1:1)).le.57) k=1
  read(arg(k:),*) nfft

  write(problem,'(i9)') nfft
  problem='ic'//adjustl(problem)
  if(.not.linplace) problem(1:1)='o'
  if(.not.lcomplex) problem(2:2)='r'

  nflags=FFTW_ESTIMATE
  if(npatience.eq.1) nflags=FFTW_ESTIMATE_PATIENT
  if(npatience.eq.2) nflags=FFTW_MEASURE
  if(npatience.eq.3) nflags=FFTW_PATIENT
  if(npatience.eq.4) nflags=FFTW_EXHAUSTIVE

  return
end subroutine timefft_opts
