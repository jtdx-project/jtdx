! This source code file was last time modified by Igor UA3DJY on June 5th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine softsym10(npts8,nsps8,newdat,fpk,syncpk,snrdb,xdt,        &
     freq,drift,schk,i1SoftSymbols,qualf,syncro,mrs,mrs2,s3,tryhint,xdtrxf)

! Compute the soft symbols

  use timer_module, only: timer

  parameter (NZ2=1512,NZ3=1360,NZ3d=24192)
  logical, intent(inout) :: newdat
  complex c2(0:NZ2-1)
  complex c3(0:NZ3-1)
  complex c3d(0:NZ3d-1)
  complex c5(0:NZ3d-1)
  real a(3), s3(0:7,69)
  integer*1 i1SoftSymbolsScrambled(207),i1SoftSymbols(207)
  integer mrs(69),mrs2(69)
  logical(1) tryhint
  include 'jt9sync.f90'

  nspsd=16
  ndown=nsps8/nspsd

! Mix, low-pass filter, and downsample to 16 samples per symbol
!  call timer('downsam9',0)
  call downsam10(npts8,newdat,fpk,c2,c3d)
!  call timer('downsam9',1)

  call peakdt10(c2,nsps8,nspsd,c3,xdt,tryhint,xdtrxf)  !Find DT
!print *,'xdt',xdt
  fsample=1500.0/ndown
  fsampled=12000./27.
  a=0.
!  call timer('afc9    ',0)
  call afc10(c3,nz3,fsample,a,syncpk)  !Find deltaF, fDot, extra DT
!  call timer('afc9    ',1)
  freq=fpk - a(1)
  drift=-2.0*a(2)
!  write(*,3301) fpk,freq,a
!3301 format(2f9.3,3f10.4)
!print *,'---'
!print *,freq,syncpk
!print *,'xdt',xdt
!print *,a(1),a(2)
  a(3)=0.

!  call timer('twkfreq ',0)
  call twkfreq(c3d,c5,nz3d,fsampled,a)   !Correct for delta f, f1, f2 ==> a(1:3)
!  call timer('twkfreq ',1)

! Compute soft symbols (in scrambled order)
!  call timer('symspec2',0)
  call symspec10(c5,nz3d,nsps8,xdt,fsampled,freq,drift,snrdb,schk,      &
       i1SoftSymbolsScrambled,qualf,syncro,mrs,mrs2,s3)
!  call timer('symspec2',1)

! Remove interleaving
  call interleave9(i1SoftSymbolsScrambled,-1,i1SoftSymbols)

  return
end subroutine softsym10
