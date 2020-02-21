subroutine timf2(x0,k,nfft,nwindow,nb,peaklimit,x1,     &
     slimit,lstrong,px,nzap)

! Sequential processing of time-domain I/Q data, using Linrad-like
! "first FFT" and "first backward FFT", treating frequencies with
! strong signals differently.  Noise blanking is applied to weak
! signals only.

!  x0       - real input data
!  nfft     - length of FFTs
!  nwindow  - 0 for no window, 2 for sin^2 window
!  x1       - real output data

! Non-windowed processing means no overlap, so kstep=nfft.  
! Sin^2 window has 50% overlap, kstep=nfft/2.

! Frequencies with strong signals are identified and separated.  Back
! transforms are done separately for weak and strong signals, so that
! noise blanking can be applied to the weak-signal portion.  Strong and
! weak are finally re-combined, in the time domain.

  parameter (MAXFFT=1024,MAXNH=MAXFFT/2)
  parameter (MAXSIGS=100)
  real x0(0:nfft-1),x1(0:nfft-1)
  real x(0:MAXFFT-1),xw(0:MAXFFT-1),xs(0:MAXFFT-1)
  real xwov(0:MAXNH-1),xsov(0:MAXNH-1)
  complex cx(0:MAXFFT-1),cxt(0:MAXFFT-1)
  complex cxs(0:MAXFFT-1)                     !Strong signals
  complex cxw(0:MAXFFT-1)                     !Weak signals
  real*4 w(0:MAXFFT-1)
  real*4 s(0:MAXNH)
  logical*1 lstrong(0:MAXNH),lprev
  integer ia(MAXSIGS),ib(MAXSIGS)
  logical first
  equivalence (x,cx),(xw,cxw),(xs,cxs)
  data first/.true./
  data k0/99999999/
  save

  if(first) then
     pi=4.0*atan(1.0)
     do i=0,nfft-1
        w(i)=(sin(i*pi/nfft))**2
     enddo
     s=0.
     nh=nfft/2
     kstep=nfft
     if(nwindow.eq.2) kstep=nh
     fac=1.0/nfft
     slimit=1.e30
     first=.false.
  endif

  if(k.lt.k0) then
     xsov=0.
     xwov=0.
  endif
  k0=k

  x(0:nfft-1)=x0
  if(nwindow.eq.2) x(0:nfft-1)=w(0:nfft-1)*x(0:nfft-1)
  call four2a(x,nfft,1,-1,0)                       !First forward FFT, r2c
  cxt(0:nh)=cx(0:nh)

! Identify frequencies with strong signals.
  do i=0,nh
     p=real(cxt(i))**2 + aimag(cxt(i))**2
     s(i)=p
  enddo
  ave=sum(s(0:nh))/nh
  lstrong(0:nh)=s(0:nh).gt.10.0*ave

  nsigs=0
  lprev=.false.
  iwid=1
  ib=-99
  do i=0,nh
     if(lstrong(i) .and. (.not.lprev)) then
        if(nsigs.lt.MAXSIGS) nsigs=nsigs+1
        ia(nsigs)=i-iwid
        if(ia(nsigs).lt.0) ia(nsigs)=0
     endif
     if(.not.lstrong(i) .and. lprev) then
        ib(nsigs)=i-1+iwid
        if(ib(nsigs).gt.nh) ib(nsigs)=nh
     endif
     lprev=lstrong(i)
  enddo

  if(nsigs.gt.0) then
     do i=1,nsigs
        ja=ia(i)
        jb=ib(i)
        if(ja.lt.0 .or. ja.gt.nh .or. jb.lt.0 .or. jb.gt.nh) then
           cycle
        endif
        if(jb.eq.-99) jb=ja + min(2*iwid,nh)
        lstrong(ja:jb)=.true.
     enddo
  endif

! Copy frequency-domain data into array cs (strong) or cw (weak).
  do i=0,nh
     if(lstrong(i)) then
        cxs(i)=fac*cxt(i)
        cxw(i)=0.
     else
        cxw(i)=fac*cxt(i)
        cxs(i)=0.
     endif
  enddo

  call four2a(cxw,nfft,1,1,-1)           !Transform weak and strong back
  call four2a(cxs,nfft,1,1,-1)           !to time domain, separately (c2r)

  if(nwindow.eq.2) then
     xw(0:nh-1)=xw(0:nh-1)+xwov(0:nh-1)     !Add previous segment's 2nd half
     xwov(0:nh-1)=xw(nh:nfft-1)             !Save 2nd half
     xs(0:nh-1)=xs(0:nh-1)+xsov(0:nh-1)     !Ditto for strong signals
     xsov(0:nh-1)=xs(nh:nfft-1)
  endif

! Apply noise blanking to weak data
  if(nb.ne.0) then
     do i=0,kstep-1
        peak=abs(xw(i))
        if(peak.gt.peaklimit) then
           xw(i)=0.
           nzap=nzap+1
        endif
     enddo
  endif

! Compute power levels from weak data only
  do i=0,kstep-1
     px=px + xw(i)**2
  enddo

  x1(0:kstep-1)=xw(0:kstep-1) + xs(0:kstep-1)     !Recombine weak + strong

  return
end subroutine timf2
