! This source code file was last time modified by Igor UA3DJY on January 18th, 2017.
! All changes are shown in the patch file coming together with the full JTDX source code.

  integer, parameter :: NTMAX=120
  integer, parameter :: NMAX=NTMAX*12000 !Total sample intervals (one minute)
  integer, parameter :: NDMAX=NTMAX*1500 !Sample intervals at 1500 Hz rate
  integer, parameter :: NSMAX=6827       !Max length of saved spectra
  integer, parameter :: MAXFFT3=16384