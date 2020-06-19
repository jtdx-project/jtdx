subroutine symspec(shared_data,k,ntrperiod,nsps,pxdb,s,df3,ihsym,npts8)

! Input:
!  k         pointer to the most recent new data
!  ntrperiod T/R sequence length, minutes
!  nsps      samples per symbol, at 12000 Hz
!  ndiskdat  0/1 to indicate if data from disk
!  nb        0/1 status of noise blanker (off/on)
!  nbslider  NB setting, 0-100

! Output:
!  pxdb      power (0-60 dB)
!  s()       current spectrum for waterfall display
!  ihsym     index number of this half-symbol (1-184) for 60 s modes

! jt9com
!  ss()      JT9 symbol spectra at half-symbol steps
!  savg()    average spectra for waterfall display

  use, intrinsic :: iso_c_binding, only: c_int, c_short, c_float, c_char
  !$ use omp_lib
  include 'jt9com.f90'

  type(dec_data) :: shared_data
  real*4 w3(MAXFFT3)
  real*4 s(NSMAX)
  real*4 ssum(NSMAX)
  real*4 xc(0:MAXFFT3-1)
  real*4 speak
  complex cx(0:MAXFFT3/2)

  data k0/99999999/,nfft3z/0/
  equivalence (xc,cx)
  save

  if(ntrperiod.eq.-999) stop                   !Silence compiler warning
!$omp single
  nfft3=16384                                  !df=12000.0/16384 = 0.732422
  jstep=nsps/2                                 !Step size = half-symbol in id2()
  if(k.gt.NMAX) go to 900
  if(k.lt.2048) then                !(2048 was nfft3)  (Any need for this ???)
     ihsym=0
     go to 900                                 !Wait for enough samples to start
  endif

  if(nfft3.ne.nfft3z) then
! Compute new window
     width=0.25*nsps
     do i=1,nfft3
        z=(i-nfft3/2)/width
        w3(i)=exp(-z*z)
     enddo
     nfft3z=nfft3
  endif

  if(k.lt.k0) then                             !Start a new data block
     ja=0
     ssum=0.
     ihsym=0
!     if(.not. shared_data%params%ndiskdat) shared_data%id2(k+1:)=0   !Needed to prevent "ghosts". Not sure why.
  endif
  gain=0.0063 !-22dB   =10**(0.1*(-22))

  speak=0.
  do i=k0+1,k
     kid2=shared_data%id2(i)
     if(abs(float(kid2)).gt.speak) speak=abs(float(kid2))
  enddo
  pxdb=0.
  if(speak.gt.0.0) pxdb=20.0*log10(speak)
  if(pxdb.gt.90.0) pxdb=90.0

  k0=k
  ja=ja+jstep                         !Index of first sample

  fac0=0.1
  do i=0,nfft3-1                      !Copy data into cx
     j=ja+i-(nfft3-1)
     xc(i)=0.
     if(j.ge.1 .and.j.le.NMAX) then
        nid2=shared_data%id2(j)
        xc(i)=fac0*nid2
     endif
  enddo
  ihsym=ihsym+1

  xc(0:nfft3-1)=w3(1:nfft3)*xc(0:nfft3-1)    !Apply window w3
  call four2a(cx,nfft3,1,-1,0)               !Real-to-complex FFT

  df3=12000.0/nfft3                   !JT9-1: 0.732 Hz = 0.42 * tone spacing
  iz=min(NSMAX,nint(5000.0/df3))
  fac1=1.0/nfft3
  fac2=1000.0*gain
  do i=1,iz
     j=i-1
     if(j.lt.0) j=j+nfft3
     sx=fac1*(abs(real(cx(j))) + abs(aimag(cx(j))))
     if(ihsym.le.184) shared_data%ss(ihsym,i)=sx
     ssum(i)=ssum(i) + sx
     s(i)=fac2*sx
  enddo

  shared_data%savg=ssum/ihsym

900 npts8=k/8
!$omp end single

  return
end subroutine symspec
