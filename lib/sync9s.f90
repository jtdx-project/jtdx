! This source code file was last time modified by Igor UA3DJY on July 10th, 2017.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine sync9s(ca,carxf,nzhsym,ia,ib,irxf,ndepth,nagain,nagainfil,ipass,filter)

  use jt65_mod6
  include 'constants.f90'
  integer, parameter :: MAXCANDRXF=5, MAXCAND=700, NFFT=16384
  real, DIMENSION(:), ALLOCATABLE :: dd1,ccfred1
  real, DIMENSION(:,:), ALLOCATABLE :: ss
  real x(NFFT),w(NFFT),ccfred(NSMAX),savg(NSMAX),savg2(NSMAX),sq(NSMAX),red2(NSMAX), &
       ssc(NSMAX),sscw(NSMAX),smo(-5:25),ss1(184)
  complex c(0:NFFT/2)
  logical nagain
  logical(1) nagainfil,filter,first
  equivalence (x,c)
  type candrxf
     real ccfred
     integer i
  end type candrxf
  type(candrxf) carxf(MAXCANDRXF)
  type candidate
     real ccfred
     integer i
  end type candidate
  type(candidate) ca(MAXCAND)
  include 'jt9sync.f90'
  data first/.true./
  save ccfred,red2,first,w

  ipk=0; ccfred=0.; carxf%ccfred=0.; carxf%i=-1; ca%ccfred=0.; ca%i=-1
  lag1=-9    !=-int(2.5/tstep + 0.9999)  tstep=0.5*nsps/12000.0  !Half-symbol step (seconds)
  lag2=18    !=int(5.0/tstep + 0.9999)
  nhstep=3456 ! 6912/2 

  red2limw=1.6
  if(ndepth.eq.1) ccflimw=3.0
  if(ndepth.eq.2) ccflimw=2.7
  if(ndepth.ge.3) ccflimw=2.5
  ccflimrxf=0.4; red2limrxf=0.6
  if(nagain) ccflimrxf=0.3
  if(nagainfil) then; ccflimw=2.3; ccflimrxf=0.3; endif

  fac1=0.1 
  fac2=(1.0/NFFT)**2; twopi=6.28318531

  if(first) then ! Compute the FFT window
     do k=1,NFFT
        w(k)=sin(twopi*(k+2)/32768) ! 32768 = 2*NFFT
     enddo
     first=.false.
  endif
  
  allocate(dd1(631552), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"
  allocate(ss(184,NSMAX), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"

  dd1(1:624000)=dd(1:624000)
  dd1(624001:631552)=0.

  do j=1,nzhsym !nhsym
     i0=(j-1)*nhstep
     x=fac1*w*dd1(i0+1:i0+NFFT)
     call four2a(c,NFFT,1,-1,0)                !r2c forward FFT
     do i=ia,ib
       s=fac2*SQRT(real(c(i))**2 + aimag(c(i))**2)
       ss(j,i)=s
     enddo
  enddo
  deallocate (dd1, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'

  if(ipass.eq.1) then
    do i=ia,ib ! df3=0.732421875 Hz
       if(i.le.NSMAX-22) then; ssc(i)=sum(ss(1:nzhsym,i:i+22)); else; ssc(i)=1.0; endif
    enddo
    do i=ia,ib
       if(i.ge.ia+22 .and. i.le.ib-22) sscw(i)=ssc(i)/(0.5*(ssc(i-22)+ssc(i+22)))
       if(i.lt.ia+22) sscw(i)=ssc(i)/ssc(i+22)
       if(i.gt.ib-22) sscw(i)=ssc(i)/ssc(i-22)
    enddo
  endif
  
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
     if(ipass.eq.1) ccfred(i)=smax*sscw(i)                !Best at this freq, over all lags
     if(ipass.eq.2) ccfred(i)=smax
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

  allocate(ccfred1(NSMAX), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"
  ccfred1=ccfred

  if(.not.nagainfil) then
     ncandrxf=0
     do m=1,maxcandrxf
        if(irxf.gt.(ia+3) .and. irxf.lt.(ib-3)) then
           nmaxloc=MAXLOC(ccfred1((irxf-3):(irxf+3)),dim=1)   !getting index of ccfred with maximum value for RX freq
           nmaxloc=nmaxloc+irxf-4
!print *,nmaxloc,ccfred1(nmaxloc),red2(nmaxloc)
!           if(abs(ccfred1(nmaxloc)).gt.ccflimrxf .and. red2(nmaxloc).gt.red2limrxf) then !applying thresholds
           if(ccfred1(nmaxloc).lt.ccflimrxf .or. red2(nmaxloc).lt.red2limrxf) then !applying thresholds
              ccfred1(nmaxloc)=0.0
              cycle
           endif
!print *,'add rxf cand'
           ncandrxf=ncandrxf+1
           carxf(ncandrxf)%ccfred=ccfred1(nmaxloc)
           carxf(ncandrxf)%i=nmaxloc
           ccfred1(nmaxloc)=0.
        endif
     enddo
  endif

! df3=0.732421875 Hz
  if(filter) then; ccfred1(:(irxf-69))=0.0; ccfred1((irxf+69):)=0.0; endif ! +-50Hz BW
  if(nagainfil) then; ccfred1(:(irxf-29))=0.0; ccfred1((irxf+29):)=0.0; endif ! +-20Hz BW

  if(.not.nagain) then
     ncandw=0
     do m=1,maxcand
        nmaxloc=MAXLOC(ccfred1,dim=1)   !getting index of ccfred with maximum value
        if(ccfred1(nmaxloc).lt.ccflimw) then ! .or. red2(nmaxloc).lt.red2limw) then !applying thresholds
           ccfred1(nmaxloc)=0.0
           cycle
        endif
        ncandw=ncandw+1
        ca(ncandw)%ccfred=ccfred1(nmaxloc)
        ca(ncandw)%i=nmaxloc
        ccfred1(nmaxloc)=0.
     enddo
  endif
!print *,ncandw,ncandrxf  
  deallocate (ccfred1, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'

  return
end subroutine sync9s
