! This source code file was last time modified by Igor UA3DJY on August 1st, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine agcc(nmode,ntxmode)

  use jt65_mod6
! Compute JT65 symbol spectra at half-symbol steps
  integer, parameter :: NFFT=8192,NSZ=3413,NHSYM=276 !3413 = NFFT*5000/12000
 ! nhsym=276 (npts-NFFT)/hstep !number of half-symbols 276.2566 = (52*12000-8192)/(2048*12000/11025)
  real*4 x11(NFFT),w11(NFFT),b1(NHSYM),b2(NHSYM),b3(NHSYM),bband(3),bdeltamax(3)
  integer jmax(3)
  complex c11(0:NFFT/2)
  logical(1) first
  equivalence (x11,c11)
  data first/.true./
  save first,w11

  real, DIMENSION(:,:), ALLOCATABLE :: ss33
  allocate(ss33(276,3413), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"

  bdeltamax=0.1; jmax=1; bband=1.0; nhstep=2229 ! 2048*(12000./11025.) JT65 half-symbol = 2229.116 samples

  fac1=1.e-3
  twopi=6.28318531

  if(first) then ! Compute the FFT window
     do k=1,NFFT
     w11(k)=sin(twopi*(k+2)/16384) ! 16384 = 2*NFFT
      enddo
     first=.false.
  endif

  do j=1,nhsym
     i0=(j-1)*nhstep
     x11=fac1*w11*dd(i0+1:i0+NFFT)
     call four2a(c11,NFFT,1,-1,0)                !r2c forward FFT
     do i=1,NSZ
        s33=ABS(c11(i))
        ss33(j,i)=SQRT(s33)
     enddo
  enddo

  do k=1,3 ! nulling three strongest bins
     do j=1,65
        maxbindex=maxloc(ss33(j,341:410),dim=1)
        is1=340+maxbindex
        ss33(j,is1)=0.
        maxbindex=maxloc(ss33(j,853:922),dim=1)
        is1=852+maxbindex
        ss33(j,is1)=0.
        maxbindex=maxloc(ss33(j,1706:1775),dim=1)
        is1=1705+maxbindex
        ss33(j,is1)=0.
     enddo
  enddo
  
  do j=1,nhsym
     if(j.le.65 .or. j.ge.255) then
        b1(j)=sum(ss33(j,341:410))
        b2(j)=sum(ss33(j,853:922))
        b3(j)=sum(ss33(j,1706:1775))
     endif
  enddo

  deallocate (ss33, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'

  if(sum(b1(255:256)).gt.0.0 .and. sum(b2(255:256)).gt.0.0 .and. sum(b3(255:256)).gt.0.0) then
     bend=max(sum(b1(275:276))/sum(b1(255:256)),sum(b2(275:276))/sum(b2(255:256)), &
          sum(b3(275:276))/sum(b3(255:256)))
  else
     bend=1.0
  endif

  do j=12,60
     bdelta1=b1(j-1)-b1(j)
     if(bdelta1.gt.bdeltamax(1)) then; bdeltamax(1)=bdelta1; jmax(1)=j; endif
     bdelta2=b2(j-1)-b2(j)
     if(bdelta2.gt.bdeltamax(2)) then; bdeltamax(2)=bdelta2; jmax(2)=j; endif
     bdelta3=b3(j-1)-b3(j)
     if(bdelta3.gt.bdeltamax(3)) then; bdeltamax(3)=bdelta3; jmax(3)=j; endif
  enddo

  if(jmax(1).gt.5) then
     b1p=b1(jmax(1)+3) + b1(jmax(1)+4) +  b1(jmax(1)+5); b1m=b1(jmax(1)-3) + b1(jmax(1)-4) +  b1(jmax(1)-5)
  else
     b1p=1.0; b1m=1.0
  endif
  if(jmax(2).gt.5) then
     b2p=b2(jmax(2)+3) + b2(jmax(2)+4) +  b2(jmax(2)+5); b2m=b2(jmax(2)-3) + b2(jmax(2)-4) +  b2(jmax(2)-5)
  else
     b2p=1.0; b2m=1.0
  endif
  if(jmax(3).gt.5) then
     b3p=b3(jmax(3)+3) + b3(jmax(3)+4) +  b3(jmax(3)+5); b3m=b3(jmax(3)-3) + b3(jmax(3)-4) +  b3(jmax(3)-5)
  else
     b3p=1.0; b3m=1.0
  endif

  if(b1p.ge.1.01) bband(1)=b1m/b1p
  if(b2p.ge.1.01) bband(2)=b2m/b2p
  if(b3p.ge.1.01) bband(3)=b3m/b3p

!do i=1,3
!print *,jmax(i),bband(i)
!enddo

  maxbandindex=maxloc(bband,dim=1)
  bstart=bband(maxbandindex)

  if(maxbandindex.eq.1) jslope=jmax(1); if(maxbandindex.eq.2) jslope=jmax(2); if(maxbandindex.eq.3) jslope=jmax(3)

!print *,jslope,bstart,bend
  facagc=2.0 
  nslope=2229*jslope
  nend=605708 ! = 26000+6000+12000+2229*252 where 6000 => 0.5sec DT
  if(nmode.eq.10 .or. nmode.eq.9) nend=605520 ! 6000+12000+6912*85
  if(nmode.eq.65+9 .and. ntxmode.eq.9) nend=624000
  if(bstart.gt.1.5) dd(1:nslope)=dd(1:nslope)/(bstart*facagc)
  if(bend.gt.1.5) dd(nend:)=dd(nend:)/(bend*facagc)
  return
end subroutine agcc
