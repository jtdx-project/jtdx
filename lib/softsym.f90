! This source code file was last time modified by Igor UA3DJY on January 24th, 2018
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine softsym(npts8,nsps8,newdat,fpk,syncpk,snrdb,xdt,        &
     freq,drift,a3,schk,i1SoftSymbols,mrs,mrs2,s3,tryhint,xdtrxf,nmode)

! Compute the soft symbols

  use timer_module, only: timer

  parameter (NZ2=1512,NZ3=1360)
  logical, intent(inout) :: newdat
  complex c2(0:NZ2-1)
  complex c3(0:NZ3-1)
  complex c5(0:NZ3-1)
  real a(3), s3(0:7,69)
  integer*1 i1SoftSymbolsScrambled(207),i1SoftSymbols(207)
  integer mrs(69),mrs2(69)
  logical(1) tryhint
  include 'jt9sync.f90'

  nspsd=16
  ndown=nsps8/nspsd

! Mix, low-pass filter, and downsample to 16 samples per symbol
!  call timer('downsam9',0)
  call downsam9(npts8,newdat,fpk,c2,nmode)
!  call timer('downsam9',1)

  call peakdt9(c2,nsps8,nspsd,c3,xdt,tryhint,xdtrxf)  !Find DT

  fsample=1500.0/ndown
  a=0.
!  call timer('afc9    ',0)
  call afc9(c3,nz3,fsample,a,syncpk)  !Find deltaF, fDot, extra DT
!  call timer('afc9    ',1)
  freq=fpk - a(1)
  drift=-2.0*a(2)
!  write(*,3301) fpk,freq,a
!3301 format(2f9.3,3f10.4)
  a3=a(3)
  a(3)=0.

!  call timer('twkfreq ',0)
  call twkfreq(c3,c5,nz3,fsample,a)   !Correct for delta f, f1, f2 ==> a(1:3)
!  call timer('twkfreq ',1)

! Compute soft symbols (in scrambled order)
!  call timer('symspec2',0)
  call symspec2(c5,nz3,nsps8,nspsd,fsample,freq,drift,snrdb,schk,      &
       i1SoftSymbolsScrambled,mrs,mrs2,s3)
!  call timer('symspec2',1)

! Remove interleaving
  call interleave9(i1SoftSymbolsScrambled,-1,i1SoftSymbols)

  return
end subroutine softsym
