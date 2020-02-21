! This source code file was last time modified by Igor UA3DJY on August 19th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine subtract65(freqa1,dt,a2,nfilt,jt65bc)

! Subtract a jt65 signal
!
! Measured signal  : dd(t)    = a(t)cos(2*pi*f0*t+theta(t))
! Reference signal : cref(t)  = exp( j*(2*pi*f0*t+phi(t)) )
! Complex amp      : cfilt(t) = LPF[ dd(t)*CONJG(cref(t)) ]
! Subtract         : dd(t)    = dd(t) - 2*REAL{cref*cfilt}

  use packjt
!  use timer_module, only: timer
  use jt65_mod2, only : correct
  use jt65_mod4 ! prc(126)
  use jt65_mod6 ! dd(NPTS)

  complex, DIMENSION(:), ALLOCATABLE :: cref,camp,cfilt
  complex cw(720000)
  integer k,nsym,ns,nref,id,ind
  real*4 window(-nfilt/2:nfilt/2)
  real*4 freqa1,dt,dphi,phi,twopi,omega,a2,a22,x0,x1,x10,fk,fc
  logical(1) first,jt65bc(2)
  data first/.true./
  save first,cw

  allocate(cref(720000), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"
  allocate(camp(720000), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"
  allocate(cfilt(720000), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"
  twopi=6.28318531
  pi512=1608.49548 !pi*512
! Symbol duration is 4096/11025 s.
! Sample rate is 12000/s, so 12000*(4096/11025)=4458.23 samples/symbol.
  nstart=nint(dt*12000.0)+1
  nsym=126
  ns=4458
  nref=561737 ! nsym*ns+29
  phi=0.0
  ind=1
  isym=1
  id=1

!  call timer('subtr_1 ',0)
  x0=672000.0/2 ! 672000 samples are used for freq drift calculation, centering it
  x1=dt*12000.0+1.0
  x10=x1-x0
  do k=1,nsym
    fk=float(k)
    a22=a2*(fk*4458.23129+x10)/672000.0
    if(prc(k)) then
        omega=pi512*(freqa1+a22)/256 ! =2*pi*f0
    else
        fc=float((correct(isym)+2))
        omega=pi512*(freqa1+a22+2.6916504*fc)/256 ! =2*pi*(f0+...
        if(jt65bc(1)) then
           fcb=float((2*correct(isym)+4))
           omega=pi512*(freqa1+a22+2.6916504*fcb)/256
        endif
        if(jt65bc(2)) then
           fcc=float((3*correct(isym)+6))
           omega=pi512*(freqa1+a22+2.6916504*fcc)/256
        endif
        isym=isym+1
    endif
    dphi=omega/12000.0
    do i=1,ns
        cref(ind)=cexp(cmplx(0.0,phi))
        phi=mod(phi+dphi,twopi)
        id=nstart-1+ind
        if(id.gt.npts) exit
        if(id.ge.1) camp(ind)=dd(id)*conjg(cref(ind))
        ind=ind+1
    enddo
    if(mod(k,5).eq.0) then !mod
    id=id+1
    if(id.gt.npts) exit
    if(id.ge.1) camp(ind)=dd(id)*conjg(cref(ind))
    ind=ind+1
    endif
  enddo
!  call timer('subtr_1 ',1)

!  call timer('subtr_2 ',0)
! Smoothing filter: do the convolution by means of FFTs. Ignore end-around 
! cyclic effects for now.

  nfft=564480
  if(first) then
! Create and normalize the filter
     sum1=0.0
     do j=-nfilt/2,nfilt/2
        window(j)=cos(twopi*j/(2*nfilt))
!        window(j)=abs(cos(twopi*j/(2*nfilt)))
        sum1=sum1+window(j)
     enddo
     cw=0.
     do l=-nfilt/2,nfilt/2
        j1=l+1
        if(j1.lt.1) j1=j1+nfft
        cw(j1)=window(l)/sum1
     enddo
     call four2a(cw,nfft,1,-1,1)
     first=.false.
  endif

  cfilt(1:nref)=camp(1:nref)
  cfilt(nref+1:nfft)=0.
  call four2a(cfilt,nfft,1,-1,1)
  fac=1.0/float(nfft)
  cfilt(1:nfft)=fac*cfilt(1:nfft)*cw(1:nfft)
  call four2a(cfilt,nfft,1,1,1)
!  call timer('subtr_2 ',1)

! Subtract the reconstructed signal
!  call timer('subtr_3 ',0)
  do m=1,nref
     j2=nstart+m-1
     if(j2.ge.1 .and. j2.le.npts) dd(j2)=dd(j2)-2*REAL(cfilt(m)*cref(m))
  enddo
!  call timer('subtr_3 ',1)

  deallocate (cfilt, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'
  deallocate (camp, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'
  deallocate (cref, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'

  return
end subroutine subtract65 
