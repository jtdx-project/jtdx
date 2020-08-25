subroutine getcandidates4(fa,fb,syncmin,nfqso,maxcand,candidate,ncand)

  use ft4_mod1, only : dd4
  include 'ft4_params.f90'
  real s(NH1,NHSYM)
  real savg(NH1),savsm(NH1)
  real sbase(NH1)
  real x(NFFT1)
  real window(256)
  complex cx(0:NH1)
  real candidate(2,maxcand),candidatet(2,maxcand)
  equivalence (x,cx)
  logical first
  data first/.true./
  save first,window

  if(first) then
    first=.false.
    pi=4.0*atan(1.)
    do i=1,256
      window(i)=SQRT((1.0+cos(i*pi/256))/2)
    enddo
  endif

! Compute symbol spectra, stepping by NSTEP steps.  
  savg=0.
  df=12000.0/NFFT1
  fac=1.0/300.0
  do j=1,NHSYM
     ia=(j-1)*NSTEP + 1
     ib=ia+2303 ! ib=ia+NFFT1-1
     if(ib.gt.NMAX) exit
     x(129:2432)=fac*dd4(ia:ib)!; x(129)=x(129)*1.9; x(2432)=x(2432)*1.9
     if(j.ne.1) then; x(1:128)=fac*dd4(ia-128:ia-1); else; x(1:128)=0.; endif
     if(j.ne.NHSYM) then; x(2433:NFFT1)=fac*dd4(ib+1:ib+128); else; x(2433:NFFT1)=0.; endif
     x(1:128)=x(1:128)*window(1:128); x(2433:NFFT1)=x(2433:NFFT1)*window(129:256)
     call four2a(cx,NFFT1,1,-1,0)              !r2c FFT
     s(1:NH1,j)=abs(cx(1:NH1))**2
     savg=savg + s(1:NH1,j)                   !Average spectrum
  enddo
  savg=savg/NHSYM

  savsm=0.
  do i=9,NH1-8; savsm(i)=sum(savg(i-8:i+8))/17.; enddo
  do i=1,8; k=i-1; savsm(i)=sum(savg(i-k:i+k))/(2*k+1); enddo
!  do i=NH1-7,NH1; k=NH1-i; savsm(i)=sum(savg(i-k:i+k))/(2*k+1); enddo

  nfa=max(1,nint(fa/df)); nfb=min(nint(4910.0/df),nint(fb/df))
  if(nfa.lt.43) then; nfaa=43; else; nfaa=nfa; endif
  call ft4_baseline(savg,nfa,nfb,sbase); if(any(sbase(nfaa:nfb).le.0)) return

  if(nfa.ge.43) then; savsm(nfa:nfb)=savsm(nfa:nfb)/sbase(nfa:nfb)
  else; savsm(1:42)=savsm(1:42)/sbase(43); savsm(43:nfb)=savsm(43:nfb)/sbase(43:nfb)
  endif
  f_offset = -1.5*12000.0/NSPS
  ncand=0; candidatet=0
  do i=nfa+1,nfb-1
     if(savsm(i).ge.savsm(i-1) .and. savsm(i).ge.savsm(i+1) .and.      &
          savsm(i).ge.syncmin) then
        den=savsm(i-1)-2*savsm(i)+savsm(i+1)
        del=0.
        if(den.ne.0.0)  del=0.5*(savsm(i-1)-savsm(i+1))/den
        fpeak=(i+del)*df+f_offset
!        if(fpeak.lt.200.0 .or. fpeak.gt.4910.0) cycle
        if(fpeak.gt.4910.0) cycle
        speak=savsm(i) - 0.25*(savsm(i-1)-savsm(i+1))*del
        ncand=ncand+1
        candidatet(1,ncand)=fpeak
        candidatet(2,ncand)=speak
        if(ncand.eq.maxcand) exit
     endif
  enddo
  candidate=0
  nq=count(abs(candidatet(1,1:ncand)-nfqso).le.20.0)
  n1=1; n2=nq+1 
  do i=1,ncand
     if(abs(candidatet(1,i)-nfqso).le.20.0) then
        candidate(1:2,n1)=candidatet(1:2,i)
        n1=n1+1
     else
        candidate(1:2,n2)=candidatet(1:2,i)
        n2=n2+1
     endif
  enddo 
return
end subroutine getcandidates4