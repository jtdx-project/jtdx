subroutine agccft8(nfa,nfb,lforcesync,forcedt)

  use ft8_mod1, only : dd8,twopi,lagccbail
  integer, parameter :: NFFT=1024,NSZ=426,NHSYM=178,NHSTEP=960 !3413 = NFFT*5000/12000
 ! nhsym=(npts-NFFT)/nhstep !number of half-symbols 178.96(6) = (180000-8192)/(1920/2)
  real*4 x11(NFFT),w11(NFFT),ss33(NSZ),s3(NHSYM)
  real, DIMENSION(:), ALLOCATABLE :: sa33
  integer, DIMENSION(:), ALLOCATABLE :: indx
  integer, intent(in) :: nfa,nfb
  complex c11(0:NFFT/2)
  logical(1) first
  logical(1), intent(in) :: lforcesync
  equivalence (x11,c11)
  data first/.true./
  save first,w11

  if(first) then ! FFT window
    do k=1,NFFT
      w11(k)=sin(twopi*(k+2)/2048) ! 16384 = 2*NFFT
    enddo
    first=.false.
  endif

  fac1=1.1e-3 ! 7548, 9.6e-4 7540, 9.7e-4 7546, 9.8e-4 7543, 1.0e-3 7539, 1.2e-3 7534..7538, 1.3e-3 7547, 2.4e-3 7543

  if(nfb.lt.5000) then; nfblim=nfb; else; nfblim=4999; endif ! ss33 top index protection
  nf1=NFFT*nfa/12000+1; nf2=NFFT*nfblim/12000
  nmed=(nf2-nf1)/2; nsb=nf2-nf1+1

  if(lforcesync) then
    jmin=0; specmin=1.E10
    do j=1,175
      i0=(j-1)*NFFT; x11=fac1*w11*dd8(i0+1:i0+NFFT)
      call four2a(c11,NFFT,1,-1,0)                !r2c forward FFT
      spec=0.
      do i=nf1,nf2
        s33=abs(c11(i)); spec=spec+s33
      enddo
      if(spec.gt.0.001 .and. spec.lt.specmin) then; specmin=spec; jmin=j; endif
    enddo
    forcedt=15.*jmin/175; if(forcedt.gt.7.5) forcedt=forcedt-15.
  else
    allocate(sa33(nsb), STAT = nAllocateStatus1)
    if(nAllocateStatus1.ne.0) STOP "Not enough memory"
    allocate(indx(nsb), STAT = nAllocateStatus1)
    if(nAllocateStatus1.ne.0) STOP "Not enough memory"

    do j=1,10
      i0=(j-1)*nhstep; x11=fac1*w11*dd8(i0+1:i0+NFFT)
      call four2a(c11,NFFT,1,-1,0)                !r2c forward FFT
      do i=nf1,nf2
        s33=ABS(c11(i)); ss33(i)=SQRT(s33)
      enddo
      sa33(1:nsb)=ss33(nf1:nf2)
      call indexx(sa33(1:nsb),nsb,indx)
      smed=sa33(indx(nmed)); if(smed.gt.1.e-6) then; s3(j)=smed; else; s3(j)=1.0; endif
    enddo

    do j=169,178
      i0=(j-1)*nhstep; x11=fac1*w11*dd8(i0+1:i0+NFFT)
      call four2a(c11,NFFT,1,-1,0)                !r2c forward FFT
      do i=nf1,nf2
        s33=ABS(c11(i)); ss33(i)=SQRT(s33)
      enddo
      sa33(1:nsb)=ss33(nf1:nf2)
      call indexx(sa33(1:nsb),nsb,indx)
      smed=sa33(indx(nmed)); if(smed.gt.1.e-6) then; s3(j)=smed; else; s3(j)=1.0; endif
    enddo

    deallocate (sa33, STAT = nDeAllocateStatus1)
    if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'
    deallocate (indx, STAT = nDeAllocateStatus1)
    if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'

    s3min1=minval(s3(1:10)); s3min2=minval(s3(169:178)); s3min=min(s3min1,s3min2)
    s3max1=maxval(s3(1:10)); s3max2=maxval(s3(169:178)); s3max=max(s3max1,s3max2)
    if(s3min.lt.0.1) then; s3min=1.0; s3max=1.0; endif
    s3ratio=s3max/s3min

    lagccbail=.false.
! 2.0dB delta
    if(s3ratio.lt.1.26) then; lagccbail=.true.; return; endif

    k=1
    do j=1,10
      dd8(k:k+959)=dd8(k:k+959)/s3(j); k=k+960
    enddo
    k=1+168*960
    do j=169,178
      dd8(k:k+959)=dd8(k:k+959)/s3(j); k=k+960
    enddo
    dd8(170881:171650)=dd8(170881:171650)/s3(178); dd8(171651:180000)=0.
  endif

  return
end subroutine agccft8