! This source code file was last time modified by Igor UA3DJY on January 18th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine symspec65(ss,ss22,nfdistort)

! Compute JT65 symbol spectra at half-symbol steps
  use jt65_mod5 !ref(3413)
  use jt65_mod6 !dd(NPTS) NPTS

  integer, parameter :: NFFT=8192,NSZ=3413,NHSYM=276 !3413 = NFFT*5000/12000
 ! nhsym=276 (npts-NFFT)/hstep !number of half-symbols 276.2566 = (52*12000-8192)/(2048*12000/11025)
  real*8 hstep
  real*4 ss(:,:),ss22(:,:),x(NFFT),x22(NFFT),w(NFFT),ref22(NSZ),stmp1(NSZ),ss1(NHSYM),stmp2(NSZ),ss2(NHSYM)
  complex c(0:NFFT/2),c22(0:NFFT/2)
  integer nfdistort
  logical(1) first
  equivalence (x,c),(x22,c22)
  data first/.true./
  save first,w
  ss=0.; ss22=0.
  hstep=2048.d0*12000.d0/11025.d0              !half-symbol = 2229.116 samples

  fac1=1.e-3
  twopi=6.28318531

  if(first) then ! Compute the FFT window
     do k=1,NFFT
     w(k)=sin(twopi*(k+2)/16384) ! 16384 = 2*NFFT
      enddo
     first=.false.
  endif

  do j=1,nhsym
     i0=(j-1)*hstep
     x=fac1*w*dd(i0+1:i0+NFFT)
     if(i0.lt.1115) then
        x22=0.
     else
        x22=fac1*w*dd(i0+1-1115:i0+NFFT-1115) !dd data shifted 1115 samples or quarter of the sybmol
     endif
     call four2a(c,NFFT,1,-1,0)                !r2c forward FFT
     call four2a(c22,NFFT,1,-1,0)

     if (nfdistort.eq.1) then
        do i=1,NSZ
          s=real(c(i))**2 + aimag(c(i))**2
          ss(j,i)=s
          s22=real(c22(i))**2 + aimag(c22(i))**2
          ss22(j,i)=s22
        enddo
     elseif(nfdistort.eq.2) then
        do i=1,NSZ
          s=SQRT(real(c(i))**2 + aimag(c(i))**2)
          ss(j,i)=s
          s22=SQRT(real(c22(i))**2 + aimag(c22(i))**2)
          ss22(j,i)=s22
        enddo
     elseif(nfdistort.eq.3) then
        do i=1,NSZ
          s=abs(real(c(i))) + abs(aimag(c(i)))
          ss(j,i)=s
          s22=abs(real(c22(i))) + abs(aimag(c22(i)))
          ss22(j,i)=s22
        enddo
     endif
  enddo

 !Flatten the 2d spectrum, saving

  npct=28  !Somewhat arbitrary
  nsmo=33 
  ia=nsmo/2 + 1
  ib=NSZ - nsmo/2 - 1
  
  do i=1,NSZ
     ss1(1:NHSYM)=ss(1:NHSYM,i)
     call pctile(ss1,nhsym,npct,stmp1(i))
  enddo
  do i=ia,ib
     call pctile(stmp1(i-nsmo/2),nsmo,npct,ref(i))
  enddo
  ref(:ia-1)=ref(ia)
  ref(ib+1:)=ref(ib)
  ref=4.0*ref

  do i=1,NSZ
     ss2=ss22(1:NHSYM,i)
     call pctile(ss2,nhsym,npct,stmp2(i))
  enddo
  do i=ia,ib
     call pctile(stmp2(i-nsmo/2),nsmo,npct,ref22(i))
  enddo
  ref22(:ia-1)=ref22(ia)
  ref22(ib+1:)=ref22(ib)
  ref22=4.0*ref22
  
  do j=1,nhsym
     ss(j,1:NSZ)=ss(j,1:NSZ)/ref
     ss22(j,1:NSZ)=ss22(j,1:NSZ)/ref22
  enddo

  return
end subroutine symspec65
