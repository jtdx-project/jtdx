subroutine wav12(d2,d1,npts,nbitsam2)

! Convert i*2 or i*1 data at 11025 Hz (from WSJT *.wav files)
! to i*2 data at 12000 Hz.

! Input:  i*2 d2(npts) or i*1 d1(npts)
!         i*2 nbitsam2 = 8 or 16 (bits per sample)

! Output: npts = (12000*npts)/11025
!         i*2 d2(npts)

  parameter (NZ11=60*11025,NZ12=60*12000)
  parameter (NFFT1=64*11025,NFFT2=64*12000)
  integer*1 d1(NZ11)
  integer*1 d1a(NZ11)
  integer*1 i1
  integer*2 i2
  integer*2 d2(NZ12)
  real x(NFFT2)
  complex cx(0:NFFT2/2)
  integer*2 nbitsam2
  equivalence (x,cx),(i1,i2)

  jz=min(NZ11,npts)
  if(nbitsam2.eq.8) then
     jz=min(NZ11,2*npts)
     d1a(1:jz)=d1(1:jz)            !d1 and d2 may be same array in calling prog 
     do i=1,jz                     !Move data from d1a into d2
        i2=0
        i1=d1a(i)
        d2(i)=10*(i2-128)
     enddo
  endif

  x(1:jz)=d2(1:jz)
  x(jz+1:)=0.0
  call four2a(cx,nfft1,1,-1,0)                    !Forwarxd FFT, r2c
  cx(nfft1/2:)=0.0
  call four2a(cx,nfft2,1,1,-1)                   !Inverse FFT, c2r

  npts=jz*12000.0/11025.0
  fac=1.e-6
!  if(nbitsam2.eq.16) fac=3.e-6
  x=fac*x
  d2(1:npts)=nint(x(1:npts))
  if(npts.lt.NZ12) d2(npts+1:NZ12)=0

  return
end subroutine wav12
