! This source code file was last time modified by Igor UA3DJY on 20190429
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine agccft4()

  use ft4_mod1, only : dd4
  integer, parameter :: NFFT=8192,NSZ=3413 !3413 = NFFT*5000/12000
  real*4 x11(NFFT),w11(NFFT),ss33(NSZ)
  complex c11(0:NFFT/2)
  integer indx(NSZ)
  logical(1) first
  equivalence (x11,c11)
  data first/.true./
  save first,w11

  fac1=7.e-4

  if(first) then ! Compute the FFT window
    twopi=8.d0*atan(1.d0)
    do k=1,NFFT; w11(k)=sin(twopi*(k+2)/16384); enddo
    first=.false.
  endif

  x11=fac1*w11*dd4(1:8192); call four2a(c11,NFFT,1,-1,0) !r2c forward FFT
  do i=1,NSZ; s33=ABS(c11(i)); ss33(i)=SQRT(s33); enddo
  call indexx(ss33(1:nsz),nsz,indx)
  smed=ss33(indx(1707)); if(smed.gt.1.E-6) then; s3start=smed; else; s3start=1.0; endif

  x11=fac1*w11*dd4(64385:72576); call four2a(c11,NFFT,1,-1,0) !r2c forward FFT
  do i=1,NSZ; s33=ABS(c11(i)); ss33(i)=SQRT(s33); enddo
  call indexx(ss33(1:nsz),nsz,indx)
  smed=ss33(indx(1707)); if(smed.gt.1.E-6) then; s3end=smed; else; s3end=1.0; endif

  dd4(1:8192)=dd4(1:8192)/s3start; dd4(64385:73728)=dd4(64385:73728)/s3end

  return
end subroutine agccft4