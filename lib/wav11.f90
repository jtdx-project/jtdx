subroutine wav11(d2,npts,dd)

! Convert i*2 data sampled at 12000 Hz to r*4 sampled at 11025 Hz.

  parameter (NZ11=60*11025,NZ12=60*12000)
  parameter (NFFT1=64*12000,NFFT2=64*11025)
  integer*2 d2(NZ12)
  real*4 dd(NZ11)
  real x(NFFT2)
  complex cx(0:NFFT1/2)
  equivalence (x,cx)
  save x,cx

  jz=min(NZ12,npts)
  x(1:jz)=d2(1:jz)
  x(jz+1:)=0.0
  call four2a(cx,nfft1,1,-1,0)                    !Forwarxd FFT, r2c
  df=12000.0/NFFT1
  ia=5000.0/df
  cx(ia:)=0.0
  call four2a(cx,nfft2,1,1,-1)                   !Inverse FFT, c2r
  npts=jz*11025.0/12000.0
  fac=1.e-6
  dd(1:npts)=fac*x(1:npts)
  if(npts.lt.NZ11) dd(npts+1:NZ11)=0.0

  return
end subroutine wav11
