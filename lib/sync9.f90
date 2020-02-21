! This source code file was last time modified by Igor UA3DJY on July 20th, 2017.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine sync9(nzhsym,lag1,lag2,ia,ib,ccfred,red2)

  use jt9_mod1 ! dd9(0:NPTS9-1) NPTS9=618624
  include 'constants.f90' ! NSMAX=6827 MAXFFT3=16384
  include 'jt9sync.f90'
  real, DIMENSION(:), ALLOCATABLE :: dd1
  real, DIMENSION(:,:), ALLOCATABLE :: ss
  real ccfred(NSMAX),savg(NSMAX),savg2(NSMAX),sq(NSMAX),red2(NSMAX),ss1(184),smo(-5:25)
  real x(MAXFFT3),w(MAXFFT3)
  complex c(0:MAXFFT3/2)
  logical(1) first
  equivalence (x,c)
  data first/.true./
  save first,w

  fac1=0.1 
  fac2=(1.0/MAXFFT3)**2; twopi=6.28318531
  nhstep=3456 ! 6912/2 

  if(first) then ! Compute the FFT window
     do k=1,MAXFFT3
        w(k)=sin(twopi*(k+2)/32768) ! 32768 = 2*MAXFFT3
     enddo
     first=.false.
  endif

  allocate(dd1(631552), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"
  allocate(ss(184,NSMAX), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"

  dd1(1:NPTS9)=dd9(0:NPTS9-1) ! NPTS9=618624
  dd1(NPTS9+1:631552)=0.

  do j=1,nzhsym !nhsym
     i0=(j-1)*nhstep
     x=fac1*w*dd1(i0+1:i0+MAXFFT3)
     call four2a(c,MAXFFT3,1,-1,0)                !r2c forward FFT
     do i=ia,ib
       s=fac2*SQRT(real(c(i))**2 + aimag(c(i))**2)
       ss(j,i)=s
     enddo
  enddo
  deallocate (dd1, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'


  ipk=0; ccfred=0.

  do i=ia,ib                         !Loop over freq range
     ss1=ss(1:184,i)
     call pctile(ss1,nzhsym,40,xmed)

     ss1=ss1/xmed - 1.0
     do j=1,nzhsym
        if(ss1(j).gt.3.0) ss1(j)=3.0
     enddo

     call pctile(ss1,nzhsym,45,sbase)
     ss1=ss1-sbase

     smax=0.
     do lag=lag1,lag2                !DT = 2.5 to 5.0 s
        sum1=0.
        do j=1,16                    !Sum over 16 sync symbols
           k=ii2(j) + lag
           if(k.ge.1 .and. k.le.nzhsym) then
              sum1=sum1 + ss1(k)
           endif
        enddo
        if(sum1.gt.smax) then
           smax=sum1
           ipk=i 
        endif
     enddo
     ccfred(i)=smax                        !Best at this freq, over all lags
  enddo

  call pctile(ccfred(ia),ib-ia+1,50,xmed)
  if(xmed.le.0.0) xmed=1.0
  ccfred=2.0*ccfred/xmed 

  savg=0.
  do j=1,nzhsym
     savg(ia:ib)=savg(ia:ib) + ss(j,ia:ib)
  enddo

  deallocate (ss, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'

  savg(ia:ib)=savg(ia:ib)/nzhsym
  smo(0:20)=1.0/21.0
  smo(-5:-1)=-(1.0/21.0)*(21.0/10.0)
  smo(21:25)=smo(-5)

  do i=ia,ib
     sm=0.
     do j=-5,25
        if(i+j.ge.1 .and. i+j.lt.NSMAX) sm=sm + smo(j)*savg(i+j)
     enddo
     savg2(i)=sm
     sq(i)=sm*sm
  enddo

  call pctile(sq(ia:ib),ib-ia+1,20,sq0)
  rms=sqrt(sq0)
  savg2(ia:ib)=savg2(ia:ib)/(5.0*rms)

  red2=0.
  do i=ia+11,ib-10
     ref=max(savg2(i-10),savg2(i+10))
     red2(i)=savg2(i)-ref
     if(red2(i).lt.-99.0) red2(i)=-99.0
     if(red2(i).gt.99.0) red2(i)=99.0
  enddo

  return
end subroutine sync9
