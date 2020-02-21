! This source code file was last time modified by Igor UA3DJY on July 26th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine sync65(ss,ss22,fmask,nfa,nfb,ncand,npass1, &
                  nfqso,hint_on,nfhinted,thresh0,ncandth25,ipass)

  use jt65_mod11
  use jt65_mod4

  implicit none
  integer, parameter :: NSZ=3413,NFFT=8192,NHSYM=276,lag1=-21,lag2=75  
!  real ss(:,:),ss22(:,:)
  real ccfblue(lag1:lag2),ccfblue22(lag1:lag2)           !CCF with pseudorandom sequence
  real ccfred(0:NSZ),ccfred22(0:NSZ)                 !Peak of ccfblue, as function of freq
  real fmask(15)
  real xlag,freq_i,tp,dx2,xmed,xmed22,df,dtx,ccfmax,ccfmax22,thresh0,dtxsync,fmaskdelta, &
       ffreq,fmasklow,fmaskhigh,deltaf,fnfqa,fnfqb,ccfpwrh,ccfpwr1h,ccfpwr2h
  integer k,l,m,nfqa,nfqb,nmaxloc,ntail_highhs,ntail_lowhs,ncandtmp, &
          nmaxcand,ia,ib,nfa,nfb,lagpk,lagpk22,ipass,ii, &
          nfqso,ncand,ncandth25,i,ifreq,nfhinted,itry,j,lagpeak
  logical(1) hint_on,qsofreq,ccfredth1,ccfredth08,ccfred22th08,ccfredth0,ccfred22th0, &
             npass1,lskip1

  type tmpcandidate
     real freq
     real dt
     real sync
     real tail_pwr
     real dtsync
  end type tmpcandidate
  type(tmpcandidate) catemp(MAXCAND)

  real, DIMENSION(:,:), ALLOCATABLE :: ss
  real, DIMENSION(:,:), ALLOCATABLE :: ss22

  interface
    subroutine xcor(ss,ss22,ifreq,ccfblue,lagpk,ccfblue22,lagpk22)
      real ss(:,:)
      real ss22(:,:)
      integer ifreq,lagpk,lagpk22
      real ccfblue(-21:75),ccfblue22(-21:75)
    end subroutine xcor
  end interface
  
  nfqa=nfqso-nfhinted
  nfqb=nfqso+nfhinted
  df=12000.0/NFFT                            !df = 12000.0/8192 = 1.465 Hz
  ia=max(2,nint(nfa/df)) ! 2 if lowest watefall frequesncy set to 0 Hz
  ib=min(NSZ-1,nint(nfb/df)) ! min(3412, nfb/1.465)  not more than 3412, if nfb 3740 ib=2552
  ncand=0; ncandtmp=0; ncandth25=0; ccfred=0.; ccfblue=0.; dtxsync=0.
  ccfmax=0.; ccfred22=0.; ccfblue22=0.; ccfmax22=0.; xlag=0.; catemp%tail_pwr=0.
  ca%freq=0.; ca%dt=0.; ca%sync=0.; catemp%freq=0.; catemp%dt=0.; catemp%sync=0.; catemp%dtsync=0.
  qsofreq=.false.; lagpeak=0

  fmaskdelta=2.0
  
  do i=ia,ib ! i=2,2552 for my waterfall settings or every 1.465 Hz

     if(ipass.eq.2 .or. ipass.eq.3) then
        lskip1=.true.
        do ii=1,15
           if(fmask(ii).lt.0.0) exit
           ffreq=float(i)*df
           fmasklow=fmask(ii)-fmaskdelta; fmaskhigh=fmask(ii)+fmaskdelta
           if(ffreq.gt.fmasklow .and. ffreq.lt.fmaskhigh) then
              lskip1=.false.; exit
           endif
        enddo
        if(lskip1) cycle
     endif

     ifreq=i
     call xcor(ss,ss22,ifreq,ccfblue,lagpk,ccfblue22,lagpk22)
     ! Remove best-fit slope from ccfblue and normalize so baseline rms=1.0
     call slope(ccfblue(lag1:lag2),lag2-lag1+1,lagpk-lag1+1.0)
     call slope(ccfblue22(lag1:lag2),lag2-lag1+1,lagpk22-lag1+1.0)
     ccfred(i)=ccfblue(lagpk)
     if(ccfred(i).gt.ccfmax) then
        ccfmax=ccfred(i)
     endif
     ccfred22(i)=ccfblue22(lagpk22)
     if(ccfred22(i).gt.ccfmax22) then
        ccfmax22=ccfred22(i)
     endif
  enddo
 
  call pctile(ccfred(ia:ib),ib-ia+1,35,xmed)
  ccfred(ia:ib)=ccfred(ia:ib)-xmed
  ccfred(ia-1)=ccfred(ia)
  ccfred(ib+1)=ccfred(ib)
  
  call pctile(ccfred22(ia:ib),ib-ia+1,35,xmed22)
  ccfred22(ia:ib)=ccfred22(ia:ib)-xmed22
  ccfred22(ia-1)=ccfred22(ia)
  ccfred22(ib+1)=ccfred22(ib)

  do i=ia,ib

     if(ipass.eq.2 .or. ipass.eq.3) then
        lskip1=.true.
        do ii=1,15
           if(fmask(ii).lt.0.0) exit
           ffreq=float(i)*df
           fmasklow=fmask(ii)-fmaskdelta; fmaskhigh=fmask(ii)+fmaskdelta
           if(ffreq.gt.fmasklow .and. ffreq.lt.fmaskhigh) then
              lskip1=.false.; exit
           endif
        enddo
        if(lskip1) cycle
     endif

     ifreq=i
     freq_i=float(i)*df
     itry=0
     qsofreq=.false.
 deltaf=abs(freq_i-float(nfqso))
 if(hint_on .and. deltaf.lt.1.0) then
    if(ccfred(i).gt.0.4 .or. ccfred22(i).gt.0.4) then
            if(ccfred(i).ge.ccfred22(i)) then
                itry=2
            else
                itry=4
            endif
        ncandtmp=ncandtmp+1
        qsofreq=.true.
        go to 32
    endif
 endif

  ccfredth1=.false.
  if(ccfred(i).gt.1.0 .or. ccfred22(i).gt.1.0) ccfredth1=.true.
  deltaf=abs(freq_i-float(nfqso))
  if(.not.hint_on .and. deltaf.lt.1.0 .and. ccfredth1) then
            if(ccfred(i).ge.ccfred22(i)) then
                itry=2
            else
                itry=4
            endif
        ncandtmp=ncandtmp+1
        qsofreq=.true.
        go to 32
 endif

 fnfqa=float(nfqa); fnfqb=float(nfqb)
 if(hint_on .and. freq_i.gt.fnfqa .and. freq_i.lt.fnfqb) then
    ccfredth08=.false.
    ccfred22th08=.false.
    if(ccfred(i).gt.0.8 .and. ccfred(i).gt.ccfred(i-1) .and.          &
        ccfred(i).gt.ccfred(i+1)) ccfredth08=.true.
    if(ccfred22(i).gt.0.8 .and. ccfred22(i).gt.ccfred22(i-1) .and.    &
        ccfred22(i).gt.ccfred22(i+1)) ccfred22th08=.true.
    if(ccfredth08 .or. ccfred22th08) then
            if(ccfred(i).ge.ccfred22(i)) then
                itry=2
            else
                itry=4
            endif
        ncandtmp=ncandtmp+1
        qsofreq=.true.
        go to 32
    endif
 endif

 ccfredth0=.false.
 ccfred22th0=.false.
 if(ccfred(i).ge.thresh0 .and. ccfred(i).gt.ccfred(i-1) .and.       &
             ccfred(i).gt.ccfred(i+1)) ccfredth0=.true.
 if(ccfred22(i).ge.thresh0 .and. ccfred22(i).gt.ccfred22(i-1) .and. &
 ccfred22(i).gt.ccfred22(i+1)) ccfred22th0=.true.
 if(ccfredth0 .or. ccfred22th0) then
   if(ccfred(i).ge.ccfred22(i)) then
     itry=2
   else
     itry=4
   endif
 ncandtmp=ncandtmp+1
 ! number of candidates above ccf threshold 2.5
 if(ccfred(i).gt.2.5 .or. ccfred22(i).gt.2.5) ncandth25=ncandth25+1
 endif

32   if(itry.ne.0) then
        call xcor(ss,ss22,ifreq,ccfblue,lagpk,ccfblue22,lagpk22)
        if(itry.eq.2) then
            call slope(ccfblue(lag1:lag2),lag2-lag1+1,lagpk-lag1+1.0)
            xlag=lagpk; lagpeak=lagpk
            if(lagpk.gt.lag1 .and. lagpk.lt.lag2) then
               call peakup(ccfblue(lagpk-1),ccfmax,ccfblue(lagpk+1),dx2)
               xlag=lagpk+dx2
            endif
        endif
        if(itry.eq.4) then    
            call slope(ccfblue22(lag1:lag2),lag2-lag1+1,lagpk22-lag1+1.0)
            xlag=lagpk22; lagpeak=lagpk22
            if(lagpk22.gt.lag1 .and. lagpk22.lt.lag2) then
               call peakup(ccfblue22(lagpk22-1),ccfmax22,ccfblue22(lagpk22+1),dx2)
               xlag=lagpk22+dx2
            endif
        endif

        dtx=xlag*2048.0/11025.0
        ccfblue(lag1)=0.; ccfblue(lag2)=0.; ccfblue22(lag1)=0.; ccfblue22(lag2)=0.
        tp=0.0
        if(dtx.gt.-2.4 .and. dtx.lt.8.4) then ! DT range limited to -5.3....5.3 sec
           ntail_lowhs=253+int((dtx-3.0)/0.186)
           ntail_highhs=269+int((dtx-3.0)/0.186)
           if(ntail_highhs.gt.322) ntail_highhs=322
           if(ntail_highhs.le.276) then
              tp=sum(ss(ntail_lowhs:ntail_highhs,i))
           else
              tp=sum(ss(ntail_lowhs:276,i))
             do k=277,ntail_highhs
               tp=tp+ss(276,i)
             enddo
           endif
           tp=tp*ccfred(i)

           !one more criterion to sort candidate list, -0.5...+1.0 DT range have highest priority 
           if(dtx.gt.2.4 .and. dtx.lt.4.1) dtxsync=ccfred(i)*5.31
           if(dtx.ge.4.1) dtxsync=ccfred(i)*(1.0+(8.41-dtx))
           if(dtx.le.2.4 .and. dtx.ge.0.0) dtxsync=ccfred(i)*(5.31-(2.4-dtx))
           if(dtx.lt.0.0) dtxsync=ccfred(i)*(2.91+dtx*0.795)

           ccfpwrh=0.; ccfpwr1h=0.; ccfpwr2h=0.
           do k=1,126 !number of symbols
              j=2*k-1+lagpeak
              if(j.ge.1 .and. j.le.nhsym) then
                 if(prc(k)) then
                    if(k.le.63) then
                       ccfpwr1h=ccfpwr1h + ss(j,i)
                    else
                       ccfpwr2h=ccfpwr2h + ss(j,i)
                    endif
                 endif
              endif
           enddo
           ccfpwrh=max(ccfpwr1h,ccfpwr2h)
           tp=tp+ccfpwrh

           catemp(ncandtmp)%freq=freq_i
           catemp(ncandtmp)%dt=dtx
           catemp(ncandtmp)%sync=ccfred(i)
           catemp(ncandtmp)%tail_pwr=tp
           catemp(ncandtmp)%dtsync=dtxsync
        else
           ncandtmp=ncandtmp-1
        endif
  endif
    if(ncandtmp.eq.MAXCAND) exit
  enddo
! now making candidate list
! put QSO frequency candidates at first positions of the list
   if(qsofreq) then
    fnfqa=float(nfqa); fnfqb=float(nfqb)
    do l=1,ncandtmp
     if(catemp(l)%freq.gt.fnfqb) exit
     if(catemp(l)%freq.gt.fnfqa .and. catemp(l)%freq.lt.fnfqb .and. catemp(l)%sync.gt.0.5) then
       ncand=ncand+1
       ca(ncand)%freq=catemp(l)%freq
       ca(ncand)%dt=catemp(l)%dt
       ca(ncand)%sync=catemp(l)%sync
       catemp(l)%tail_pwr=0.
       catemp(l)%sync=0.
       catemp(l)%dtsync=0.
     endif
    enddo
   endif

    nmaxcand=0
    nmaxloc=0

    if(npass1) nmaxcand=min(150,ncandtmp)
    if(.not.npass1) nmaxcand=min(35,ncandtmp)

! make candidate list for the first pass if band is crowded using (ccf * tail power) as criterion
if(npass1 .and. ncandth25.gt.8) then
    do m=1,nmaxcand
       nmaxloc=MAXLOC(catemp%tail_pwr,dim=1)   !getting index of tail_pwr with maximum value
       if(abs(catemp(nmaxloc)%tail_pwr).lt.0.001) cycle !if value of tail_pwr eq 0. then cycle loop
          ncand=ncand+1
          ca(ncand)%freq=catemp(nmaxloc)%freq
          ca(ncand)%dt=catemp(nmaxloc)%dt
          ca(ncand)%sync=catemp(nmaxloc)%sync
          catemp(nmaxloc)%tail_pwr=0.
          catemp(nmaxloc)%sync=0.
    enddo
    if(qsofreq) then !put qso frequency candidate also at end of the list to benefit from subtraction 
       ncand=ncand+1
       ca(ncand)%freq=ca(1)%freq
       ca(ncand)%dt=ca(1)%dt
       ca(ncand)%sync=ca(1)%sync
    endif
    go to 2
endif

! make candidate list for the first pass if band is not crowded using ccf value as criterion
if(npass1 .and. ncandth25.le.8) then
  do m=1,nmaxcand
    nmaxloc=MAXLOC(catemp%sync,dim=1)   !getting index of sync with maximum value
    if(abs(catemp(nmaxloc)%sync).lt.0.001) cycle !if value of sync eq 0. then cycle loop
       ncand=ncand+1
       ca(ncand)%freq=catemp(nmaxloc)%freq
       ca(ncand)%dt=catemp(nmaxloc)%dt
       ca(ncand)%sync=catemp(nmaxloc)%sync
       catemp(nmaxloc)%sync=0.
  enddo
    go to 2
endif

! make candidate list for passes 2...4 using (ccf * dt) criterion
if(.not.npass1) then
  do m=1,nmaxcand
    nmaxloc=MAXLOC(catemp%dtsync,dim=1)   !getting index of sync with maximum value
    if(abs(catemp(nmaxloc)%sync).lt.0.001) cycle !if value of sync eq 0. then cycle loop
    if(abs(catemp(nmaxloc)%dtsync).lt.0.001) cycle !if value of dtsync eq 0. then cycle loop
       ncand=ncand+1
       ca(ncand)%freq=catemp(nmaxloc)%freq
       ca(ncand)%dt=catemp(nmaxloc)%dt
       ca(ncand)%sync=catemp(nmaxloc)%sync
       catemp(nmaxloc)%sync=0.
       catemp(nmaxloc)%dtsync=0.
  enddo
! put first QSO freq candidate also at end of the list to benefit from subtraction 
    if(qsofreq .and. ncandth25.gt.8) then 
       ncand=ncand+1
       ca(ncand)%freq=ca(1)%freq
       ca(ncand)%dt=ca(1)%dt
       ca(ncand)%sync=ca(1)%sync
    endif
endif
2 continue
 return
end subroutine sync65

