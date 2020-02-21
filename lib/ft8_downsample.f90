! This source code file was last time modified by Igor UA3DJY on 20191105
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine ft8_downsample(newdat,f0,nqso,c1,c2,c3,lhighsens)

! Downconvert to complex data sampled at 200 Hz ==> 32 samples/symbol
  use ft8_mod1, only : dd8,windowc1,facc1
  parameter (NFFT1=192000,NFFT2=3200)      !192000/60 = 3200
  
  logical newdat
  complex, intent(out) :: c1(0:3199),c2(0:3199),c3(0:3199)
  complex cx(0:96000),cxx(0:96000) ! 0:NFFT1/2
  real x(NFFT1)
  real, intent(in) :: f0
  integer, intent(in) :: nqso
  logical(1), intent(in) :: lhighsens
  equivalence (x,cx)
  save cxx

  if(newdat) then
! Data in dd have changed, recompute the long FFT
     x(1:180000)=dd8
     x(180001:NFFT1)=0.                       !Zero-pad the x array
!possible usage at 2nd decoding attempt
!do i=1,179999; x(i)=(x(i)+x(i+1))/2; enddo +8% -22CQ +5% -30RR73
     call four2a(cx,NFFT1,1,-1,0)             !r2c FFT to freq domain
     newdat=.false.
     cxx=cx
  endif

  df=0.0625 ! 12000.0/NFFT1
!  baud=6.25 ! 12000.0/1920.
  i0=nint(f0/df)
  ft=f0+53.125 ! 8.5*baud
  it=min(nint(ft/df),96000)
  fb=f0-9.375  ! 1.5*baud
  ib=max(1,nint(fb/df))
  c1=cmplx(0.0,0.0)
  k=it-ib
  c1(0:k)=cxx(ib:it)
  c1(0:100)=c1(0:100)*windowc1(100:0:-1)
  c1(k-100:k)=c1(k-100:k)*windowc1
  c1=cshift(c1,i0-ib)
  if(lhighsens) then; c1(0)=c1(0)*1.9; c1(3199)=c1(3199)*1.9; endif
  call four2a(c1,NFFT2,1,1,1)            !c2c FFT back to time domain
  c1=facc1*c1
  if(nqso.gt.1) then; do i=0,3198; c2(i)=c1(i)+c1(i+1); enddo; c2(3199)=c1(3199); c2=c2/2; endif
  if(nqso.eq.3) then; do i=1,3199; c3(i)=c1(i-1)+c1(i); enddo; c3(0)=c1(0); c3=c3/2; endif

  return
end subroutine ft8_downsample
