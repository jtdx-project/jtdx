subroutine ft8_downsample(newdat1,f0,nqso,c0,c2,c3,lhighsens,lsubtracted,npos,freqsub)

! Downconvert to complex data sampled at 200 Hz ==> 32 samples/symbol
  use ft8_mod1, only : dd8,windowc1,facc1
  parameter (NFFT1=192000,NFFT2=3200)      !192000/60 = 3200
  
  logical newdat1
  complex, intent(out) :: c0(-800:4000),c2(-800:4000),c3(-800:4000)
  complex cx(0:96000),cxx(0:96000),c1(0:3199) ! 0:NFFT1/2
  real x(NFFT1)
  real, intent(in) :: f0
  integer, intent(in) :: nqso
  real freqsub(200)
  logical(1), intent(in) :: lhighsens
  logical(1) lsubtracted,ldofft
  equivalence (x,cx)
  save cxx

  ldofft=.false.
  if(lsubtracted) then
    do i=1,npos
      if(abs(f0-freqsub(i)).lt.50.) then; ldofft=.true.; lsubtracted=.false.; exit; endif
    enddo
  endif

  if(newdat1 .or. ldofft) then
! Data in dd have changed, recompute the long FFT
     x(1:180000)=dd8
     x(180001:NFFT1)=0.                       !Zero-pad the x array
     call four2a(cx,NFFT1,1,-1,0)             !r2c FFT to freq domain
     newdat1=.false.; ldofft=.false.; npos=0
     cxx=cx
  endif

  df=0.0625 ! 12000.0/NFFT1
!  tonewidth=6.25 Hz ! 12000.0/1920.
  i0=nint(f0/df)
  ft=f0+55.75; it=min(nint(ft/df),96000)
  fb=f0-5.75; ib=max(1,nint(fb/df))
  c1=cmplx(0.0,0.0)
  k=it-ib
  c1(0:k)=cxx(ib:it)
  c1(0:54)=c1(0:54)*windowc1(54:0:-1)
  c1(k-54:k)=c1(k-54:k)*windowc1
  c1=cshift(c1,i0-ib)
  if(lhighsens) then; c1(0)=c1(0)*1.93; c1(799)=c1(799)*1.7; c1(800)=c1(800)*1.7; c1(3199)=c1(3199)*1.93
  else; c1(45)=c1(45)*1.49; c1(54)=c1(54)*1.49; c1(3145)=c1(3145)*1.49; c1(3154)=c1(3154)*1.49
  endif ! 10346..10347
  call four2a(c1,NFFT2,1,1,1)            !c2c FFT back to time domain
  c0(-800:-1)=0.; c0(0:3199)=facc1*c1(0:3199); c0(3200:4000)=0.
  if(nqso.gt.1) then
    c2(-800:-1)=0.; c2(3200:4000)=0.
    do i=0,3198; c2(i)=c0(i)+c0(i+1); enddo
    c2(3199)=c0(3199); c2=c2/2
  endif
  if(nqso.eq.3) then
    c3(-800:-1)=0.; c3(3200:4000)=0.
    do i=1,3199; c3(i)=c0(i-1)+c0(i); enddo
    c3(0)=c0(0); c3=c3/2
  endif

  return
end subroutine ft8_downsample
