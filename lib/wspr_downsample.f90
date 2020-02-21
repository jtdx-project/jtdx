! last time modified by Igor UA3DJY on 20200202

subroutine wspr_downsample(id2,k)

! Input:
!  id2       raw 16-bit integer data, 12000 Hz sample rate
!  k         pointer to the most recent new data

! Output (in common/c0com)
!  c0        complex data downsampled to 1500 Hz

  parameter (NMAX=120*12000)         !Total sample intervals per 30 minutes
  parameter (NDMAX=120*1500)         !Sample intervals at 1500 Hz rate
!  parameter (NSMAX=1366)             !Max length of saved spectra
  parameter (NFFT1=1024)
  parameter (MAXFFT3=32768)
  real*4 w3(MAXFFT3)
  real*4 x0(NFFT1)
  integer*2 id2(NMAX)
  complex c0
  common/c0com/c0(NDMAX)
!  data rms/999.0/,k0/99999999/,nfft3z/0/,nsps/8192/,nbfo/1500/
  data k0/99999999/,nfft3z/0/,nsps/8192/,nbfo/1500/
  save

  nfft3=nsps/4
!  jstep=nsps/16
  if(k.gt.NMAX) go to 999
  if(k.lt.nfft3) go to 999                   !Wait for enough samples to start
  if(nfft3.ne.nfft3z) then
     pi=4.0*atan(1.0)
     do i=1,nfft3
        w3(i)=2.0*(sin(i*pi/nfft3))**2             !Window for nfft3
     enddo
     nfft3z=nfft3
  endif

  if(k.lt.k0) then
     k1=0
     k8=0
!     if(ndiskdat.eq.0) then
!        id2(k+1:)=0
!        c0=0.          !This is necessary to prevent "ghosts".  Not sure why.
!     endif
  endif
  k0=k
 
!  nzap=0
!  nbslider=0
!  sigmas=1.0*(10.0**(0.01*nbslider)) + 0.7
!  peaklimit=sigmas*max(10.0,rms)
!  px=0.

!  nwindow=2
!  kstep1=NFFT1
!  if(nwindow.ne.0) kstep1=NFFT1/2
  kstep1=NFFT1/2
  nblks=(k-k1)/kstep1
!  gain=1.0
!  nb=0
  do nblk=1,nblks
     do i=1,NFFT1
!        x0(i)=gain*id2(k1+i)
        x0(i)=float(id2(k1+i))
     enddo
!     call timf2(x0,k,NFFT1,nwindow,nb,peaklimit,x1,   &
!          slimit,lstrong,px,nzap)
! Mix at nbfo Hz, lowpass at +/-750 Hz, and downsample to 1500 Hz complex.
     call mixlpf(x0,nbfo,c0(k8+1))
     k1=k1+kstep1
     k8=k8+kstep1/8
  enddo

999 return
end subroutine wspr_downsample
