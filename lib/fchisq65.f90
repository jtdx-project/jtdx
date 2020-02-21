! This source code file was last time modified by Igor UA3DJY on July 22nd, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

real function fchisq65(c5x,npts,fsample,a,ccfmax,dtmax)

  use timer_module, only: timer
  use jt65_mod4 !prc(i) logical(1) nagcc real dtcand

  parameter (NMAX=60*12000)          !Samples per 60 s
  complex c5x(npts)
  real a(2)
  complex w,wstep,z
  real ss(3000)
  complex csx(0:NMAX/8)
  data twopi/6.283185307/a1,a2/99.,99./
!  save

!  call timer('fchisq65',0)
  baud=11025.0/4096
  nsps=nint(fsample/baud)                  !Samples per symbol
  ndiv=16                                  !Output ss() steps per symbol
  nout=ndiv*npts/nsps
  dtstep=1.0/(ndiv*baud)                   !Time per output step

 if(a(1).ne.a1 .or. a(2).ne.a2) then
     a1=a(1)
     a2=a(2)

! Mix and integrate the complex signal
     csx(0)=0.
     wstep=(0.0,0.0); w=(1.0,0.0)
     x0=0.5*npts
     do i=1,npts
        if(mod(i,100).eq.1) then
           x=(float(i)-x0)/float(npts)
           dphi=(a(1) + x*a(2)) * (twopi/fsample)
           wstep=cmplx(cos(dphi),sin(dphi))
        endif
        w=w*wstep
        csx(i)=csx(i-1) + w*c5x(i)
     enddo
  endif
!print *,nout,nsps,npts 2411,64,9646
! Compute whole-symbol powers at 1/16-symbol steps.
  fac=1.e-4
  do i=1,nout
     j=nsps+(i-1)*nsps/16 !steps by 4 samples (1/16 of a symbol)
     k=j-nsps
     ss(i)=0.
     if(k.ge.0 .and. j.le.npts) then
        z=csx(j)-csx(k) ! difference over span of 64 pts
        ss(i)=fac*(abs(real(z)) + abs(aimag(z)))
     endif
  enddo

  ccfmax=0.
!  call timer('ccf2    ',0)
  call ccf2(ss,nout,ccf,xlagpk,lagpk)
!  call timer('ccf2    ',1)
  if(ccf.gt.ccfmax) then; ccfmax=ccf; dtmax=xlagpk*dtstep; endif !dtstep=0.02322
  fchisq65=-ccfmax

  s0=0.
  s1=0.
  do i=1,126
     j=16*(i-1)+1 + lagpk
     if(j.ge.1 .and. j.le.nout-8) then
        x=ss(j)**2
        if(.not.prc(i)) then
           s0=s0 + x
        else
           s1=s1 + x
        endif
     endif
  enddo
  ccf=s1-s0
  if(ccf.gt.0.0) ccfmax=ccf

  return
end function fchisq65
