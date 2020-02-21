! This source code file was last time modified by Igor UA3DJY on June 29th, 2017.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine sync10(nzhsym,ia,ib,irxf,th_wide,th_rxf,ncand0,newdat,carxf,ca)

!  use jt9_mod2
  use jt65_mod6
!  include 'constants.f90'
  include 'jt10sync.f90'
  integer, parameter :: NSMAX10=11520, NFFT=27648, NHSYM=184, MAXCANDRXF=5, MAXCAND=200
  real ss(NHSYM,NSMAX10)
  real, DIMENSION(:), ALLOCATABLE :: dd1,ccfred1
  real ccfred(NSMAX10),psum(ia:ib),x(NFFT),w(NFFT)!,syncrio(NSMAX10), syncpwr(16)
  complex c(0:NFFT/2)
  integer ncand0
  logical newdat
  logical(1) first
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
  data first/.true./
  save first,w,ccfred,ss

 ! ipk=0
  ccfred=0.; psum=0.; nmaxcand=200; carxf%ccfred=0.; carxf%i=-1; ca%ccfred=0.; ca%i=-1 !; lagpk=0
  syncrio=1.0  
!  tstep=0.5*nsps/12000.0                      !Half-symbol step (seconds)
  lag1=-9 !-int(2.5/tstep + 0.9999) ! -9
  lag2=18 !int(5.0/tstep + 0.9999)  ! 18
  pfloor=0.; psavg=0.; fac1=0.1; twopi=6.28318531!; icandcount=0
  nhstep=3456 ! 6912/2
  fac2=(1.0/NFFT)**2

  if(first) then ! Compute the FFT window
     do k=1,NFFT
        w(k)=sin(twopi*(k+2)/55296.) ! 27648 = 2*NFFT
     enddo
     first=.false.
  endif
  if(newdat) then
     allocate(dd1(660096), STAT = nAllocateStatus1)
     if(nAllocateStatus1.ne.0) STOP "Not enough memory"
     dd1(1:624000)=dd(1:624000)
     dd1(624001:660096)=0.

     do j=1,nhsym
        i0=(j-1)*nhstep
        x=fac1*w*dd1(i0+1:i0+NFFT)
        call four2a(c,NFFT,1,-1,0)                !r2c forward FFT
        do i=ia,ib
!          s=fac2*(real(c(i))**2 + aimag(c(i))**2)
          s=fac2*SQRT(real(c(i))**2 + aimag(c(i))**2)
          ss(j,i)=s
        enddo
     enddo
     deallocate (dd1, STAT = nDeAllocateStatus1)
     if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'

     do i=ia,ib
        psum(i)=sum(ss(1:NHSYM,i))
        pfloor=pfloor+psum(i)
     enddo
     pfloor=pfloor/(ib-ia)
     facps=0.025
     psavg=facps*pfloor/float(NHSYM)

     do j=1,nhsym
        do i=1,ib
           ss(j,i)=ss(j,i)+psavg
        enddo
     enddo
  endif
  
!do k=1,NHSYM
!sssum=0.
!do i=ia,ib
! sssum=sssum+ss(k,i)
!enddo
!enddo
!print *,sssum
     
  do i=ia,ib
     psum(i)=sum(ss(1:NHSYM,i)) ! have to calculate psum again with corrected ss()
  enddo

!  pave=0.
  do i=ia,ib                         !Loop over freq range
     smax=0.
     do lag=lag1,lag2                !DT = -2.5 to 5.0s, lag -9..18
        sum1=0.
        do j=1,16                    !Sum over 16 sync symbols
           k=ii2(j) + lag
           if(k.ge.1 .and. k.le.nzhsym) then
              if(lsync(j)) sum1=sum1 + ss(k,i)
              if(.not.lsync(j) .and. i.lt.(ib-16)) sum1=sum1 + ss(k,i+16)
           endif
        enddo
        if(sum1.gt.smax) then
           smax=sum1
!           lagpk=lag
!           ipk=i 
        endif
     enddo


!     syncpwr=0.
!     do j=1,16
!        k=ii2(j) + lagpk
!        if(k.ge.1 .and. k.le.nzhsym) then
!           if(lsync(j)) syncpwr(j)=ss(k,i)
!           if(.not.lsync(j) .and. i.lt.(ib-16)) syncpwr(j)=ss(k,i+16)
!        endif
!     enddo
!     syncav516=0.
!     syncav516=(2.*sum(syncpwr(5:16)))/12.
!     do j=1,4
!        if(syncpwr(j).gt.syncav516) syncpwr(j)=syncav516
!     enddo
!     sync1=0.; sync2=0.
!     do j=1,16
!        if(lsync(j)) sync1=sync1+syncpwr(j)
!        if(.not.lsync(j) .and. i.lt.(ib-16)) sync2=sync2+syncpwr(j)
!     enddo
!    syncrio(i)=sync2/sync1
!    if(syncrio.gt.0.25 .and. syncrio.lt.4.0) fac=1.



     psig=0.; pnoise=0.
     htnoise=float(2*(NHSYM-16)) ! number of noise halftones for 2-tone sync pattern
     if(i.lt.(ib-16)) then
        psig=smax/16.
        pnoise=abs(psum(i)+psum(i+16)-2.*smax)/htnoise
     endif
     if(i.ge.(ib-16)) then
        psig=smax/8.
        pnoise=2.*abs(psum(i)-2.*smax)/htnoise
     endif
	 
!     if(i.lt.(ib-9)) sm0=(smax/16.- (psum(i)+psum(i+9)-2.*smax)/336.)/(psum(i)+psum(i+9))
!     if(i.ge.(ib-9)) sm0=(smax/8.- (psum(i)-2.*smax)/168.)/psum(i)

!     ccfred(i)=8.*fac*sm0                        !Best at this freq, over all lags
!     ccfred(i)=1.2*psig/(pnoise+psavg)              !Best at this freq, over all lags

!     ccfred(i)=1.2*psig/pnoise
     ccfred(i)=1.5*psig/pnoise

!print *,psavg
!f=(i-1)*12000.0/27648.0
!if(i.lt.50 .or. i.gt.3916) print *,f,ccfred(i)
!print *,f,ccfred(i)
!print *,psig,pnoise
!print *,'-'
!print *,psig,(pnoise+psavg)
!print *,psavg
!     pave=pave+ccfred(i)
!if(ccfred(i).gt.th_wide) icandcount=icandcount+1
  enddo
  
!print *,icandcount
!  pave=pave/(ib-ia)
!print *,pave
!do i=ia,ib
!f=(i-1)*1500./2048.
!if(abs(f-1710.).lt.20.) then
!print *,f,ccfred(i)
!endif
!enddo
  allocate(ccfred1(NSMAX10), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"
  do i=ia+9,ib-8
    qual=ccfred(i)*2.0/(ccfred(i-8)+ccfred(i+8))
!if(syncrio(i).gt.1.0) ccfred1(i)=qual*ccfred(i)/sqrt(syncrio(i))
!if(syncrio(i).le.1.0) ccfred1(i)=qual*sqrt(syncrio(i))*ccfred(i)
    ccfred1(i)=qual*ccfred(i)
  enddo
  ccfred1(ia:ia+8)=ccfred(ia:ia+8)
  ccfred1(ib-8:ib)=ccfred(ib-8:ib)  

!  ccfred1=ccfred
  ncand1=0
  do m=1,maxcandrxf
     if(irxf.gt.3 .and. irxf.lt.(ib-3)) then
        nmaxloc=MAXLOC(ccfred1((irxf-3):(irxf+3)),dim=1)   !getting index of ccfred with maximum value for RX freq
        i=nmaxloc+irxf-4
        if(abs(ccfred1(i)).lt.th_rxf) cycle !lowest ccf threshold
        ncand1=ncand1+1
        carxf(ncand1)%ccfred=ccfred1(i)
        carxf(ncand1)%i=i
        ccfred1(i)=0.
     endif
  enddo

  ncand0=0
  do m=1,maxcand
     nmaxloc=MAXLOC(ccfred1,dim=1)   !getting index of ccfred with maximum value
     if(abs(ccfred1(nmaxloc)).lt.th_wide) cycle !lowest ccf threshold
     ncand0=ncand0+1
     ca(ncand0)%ccfred=ccfred1(nmaxloc)
     ca(ncand0)%i=nmaxloc
     ccfred1(nmaxloc)=0.
  enddo
  deallocate (ccfred1, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'
!print *,ncand0

!do i=1,200
!print *,(ca(i)%i-1)*12000.0/27648.0, ca(i)%ccfred
!enddo

  return
end subroutine sync10