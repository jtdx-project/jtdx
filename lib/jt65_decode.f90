! last time modified by Igor UA3DJY on 20200104

module jt65_decode

  integer, parameter :: NSZ=3413, NFFT=1000, NHSYM=276
  type :: jt65_decoder
     procedure(jt65_decode_callback), pointer :: callback => null()
   contains
     procedure :: decode
  end type jt65_decoder

  !
  ! Callback function to be called with each decode
  !
  abstract interface
     subroutine jt65_decode_callback (this, utc, snr, dt, nfreq, decoded, servis)
       import jt65_decoder
       implicit none
       class(jt65_decoder), intent(inout) :: this
       integer, intent(in) :: utc
       integer, intent(in) :: snr
       real, intent(in) :: dt
       integer, intent(in) :: nfreq
       character(len=26), intent(in) :: decoded
       character(len=1), intent(in) :: servis
     end subroutine jt65_decode_callback
  end interface

contains

  subroutine decode(this,callback,nutc,nf1,nf2,nfqso,nagainfil,ntrials,naggressive, &
                    nhint,mycall,hiscall,hisgrid,nprepass,swl,filter,stophint,nlasttx,nsdecatt, &
                    fmaskact,ntxmode,ntopfreq65,nharmonicsdepth,showharmonics)

    !  Process dd() data to find and decode JT65 signals.
    use jt65_mod6 ! dd(NPTS) NPTS=624000
    use jt65_mod8 !  dynhint(300), dynhintcq(150), rxfreq(20), rxfchg(1), ninterval, freq, maxsync2, maxsync2freq
    use jt65_mod11 !ca and dtchk data structures
    use timer_module, only: timer

    include 'constants.f90'

    class(jt65_decoder), intent(inout) :: this
    procedure(jt65_decode_callback) :: callback
    integer, intent(in) :: nutc,nf1,nf2,nfqso,ntrials,naggressive,nprepass,nlasttx,nsdecatt,ntxmode,ntopfreq65, &
                           nharmonicsdepth
    logical, intent(in) :: nagainfil
    logical(1), intent(in) :: filter, swl, stophint, fmaskact, nhint, showharmonics
    character(len=12), intent(in) :: mycall, hiscall
    character(len=6), intent(in) :: hisgrid

    real a(2)
    character*22 decoded

    type accepted_decode
       real freq
       character*22 decoded
       character*1 hint
       logical(1) freemsg
    end type accepted_decode
    type(accepted_decode) dec(200)

    logical(1) hintedrxfreq,hint_on,dupe,npass1,ncand_lt9,sync1_lt1,newfft,first,nstophint
    integer ntrials0,ntrials1,ncand,nvec,ncandth25,nfdistort,nfhinted,nft,ipass, &
            ncandcut,imax1,imax2,m,icand,nfa,nfb,nint_prev,nint_newday
    integer h0(0:11),d0(0:11)
    real r0(0:11)
    real thresh3,syncmax1,syncmax2,syncmax3,dtx,sync1
    real snrd(31)

!            0  1  2  3  4  5  6  7  8  9 10 11
    data h0/41,42,43,43,44,45,46,47,48,48,49,49/
    data d0/71,72,73,74,76,77,78,80,81,82,83,83/

!             0    1    2    3    4    5    6    7    8    9   10   11
    data r0/0.70,0.72,0.74,0.76,0.78,0.80,0.82,0.84,0.86,0.88,0.90,0.90/
!              -1   -2   -3   -4   -5   -6   -7   -8    -9   -10   -11   -12   -13  -14  -15  -16  -17
    data snrd/-1.0,-1.2,-1.5,-1.5,-1.5,-1.6,-1.7,-1.75,-1.8,-1.85,-1.95,-2.15,-2.2,-2.3,-2.3,-2.3,-2.35, &
!    -18   -19   -20  -21  -22  -23  -24  -25  -26  -27  -28  -29  -30  -31
    -2.35,-2.35,-2.4,-2.4,-2.4,-2.4,-2.5,-2.5,-2.5,-2.6,-2.6,-2.7,-2.7,-2.7/
    data first/.true./
    data nint_prev/-1/
    data nint_newday/0/
    save

    character msg26*26,callcq*6,callncq*6,hint*1,isfalse*1,servis*1
    logical(1) freemsg,bypass_ftrsd,jt65bc(2)
    real fmask(15)

  real, DIMENSION(:,:), ALLOCATABLE :: ss
  real, DIMENSION(:,:), ALLOCATABLE :: ss22

  interface
    subroutine symspec65(ss,ss22,nfdistort)
      real ss(:,:)
      real ss22(:,:)
      integer nfdistort
    end subroutine symspec65
    subroutine sync65(ss,ss22,fmask,nfa,nfb,ncand,npass1,nfqso,hint_on, &
                      nfhinted,thresh0,ncandth25,ipass)
      real ss(:,:)
      real ss22(:,:)
      real fmask(15)
      integer nfa,nfb,ncand,nfqso,nfhinted,ncandth25,ipass
      logical(1) hint_on,npass1
      real thresh0
    end subroutine sync65
  end interface

    this%callback => callback
    nfhinted=3 ! +- frequency offset for hinted decoding
    ncandcount=0 ! common count for all four decoding passes, used in decoded65a
! initialize data structures for each time interval before decoding
    ca%freq=0.; ca%dt=0.; ca%sync=0.
    dec%freq=0.; dec%decoded='                      '; dec%hint='*'
    dtchk%cand_dtbest=-10.0; dtchk%cand_decoded=.false.
    fmask=-1.0
    ndecoded=0; ncandth25=0; ndecpass1=0; ncandipass1=64
    hint_on=.false.; bypass_ftrsd=.false.

    nstophint=stophint; if(ntxmode.eq.9) nstophint=.true.
    if(nhint) hint_on=.true.
! calculate unique time interval number from the current time or wav file interval time
    ninterval=(nutc/100)*60 + mod(nutc,100)
    nminutes=1440 ! number of minutes a day
    do i=1,7 ! smooth one week time change for experience based Hint decoders
       nminmin=1435+(i-1)*nminutes
       nminmax=nminmin+4
       if(nint_prev.ge.nminmin .and. nint_prev.le.nminmax .and. &
          ninterval.ge.0 .and. ninterval.le.5) then
          nint_newday=nint_newday+nminutes
          exit
       endif
    enddo
    ninterval=ninterval+nint_newday ! smooth day time change for experience based Hint decoders

   if(first) then !
      dynhint%ninterval=-1
      dynhintcq%ninterval=-1
      rxfreq%ninterval=-1
      rxfchg%ninterval=-1
      rxfchg%nfqso=nfqso
      rxfchg%rxfchanged=.false.
      first=.false.
   endif

   if(ninterval.ne.rxfchg(1)%ninterval) then
      if(nfqso.le.ntopfreq65 .and. nfqso.ne.rxfchg(1)%nfqso) then
         rxfchg(1)%rxfchanged=.true.
         go to 2
      endif
   endif
   if(rxfchg(1)%ninterval.ge.0) then
      if(abs(ninterval-rxfchg(1)%ninterval).gt.6 .and. rxfchg(1)%rxfchanged) &
         rxfchg(1)%rxfchanged=.false.
   endif
2  if(nfqso.ne.rxfchg(1)%nfqso) then
      rxfchg(1)%ninterval=ninterval
      rxfchg(1)%nfqso=nfqso
   endif
   
   do i=1,20
      if(rxfreq(i)%ninterval.eq.-1) cycle
      deltaf=abs(float(nfqso)-rxfreq(i)%freq)
      if(deltaf.gt.4.0) rxfreq(i)%ninterval=-1
   enddo

   maxsync2=0.; maxsync2freq=0.

    do ipass=1,nprepass+5                          ! up to 4-pass decoding loop
       newfft=.true.
          if(ipass.eq.1) then  !first-pass parameters, fmask being filled in
              thresh0=2.3
              nfdistort=2
          elseif(ipass.eq.2) then !fmask decoding pass
              thresh0=1.5
              nfdistort=1
              if(ncandth25.gt.17) hint_on=.false. ! turn off Hint
          elseif(ipass.eq.3) then !fmask decoding pass
              thresh0=1.5
              nfdistort=3
              if(nhint) hint_on=.true. ! recover Hint after 1st fmask decoding pass
              if(ncandth25.gt.17) hint_on=.false. ! turn off Hint
          elseif(ipass.eq.4) then !single decode pass subpass3
              if(nfqso.lt.4) cycle
              thresh0=1.2
              nfdistort=1
              hint_on=.false. ! turn off Hint for single decode passes
          elseif(ipass.eq.5) then !single decode pass subpass4
              if(nfqso.lt.4) cycle
              thresh0=1.2
              nfdistort=3
          elseif(ipass.eq.6) then !single decode pass subpass5
              if(nfqso.lt.4) cycle
              thresh0=1.2
              nfdistort=2
          elseif(ipass.eq.7) then !second-pass parameters 2+5
              thresh0=1.5
              nfdistort=1
              if(nhint) hint_on=.true. ! recover Hint after the single decode
          elseif(ipass.eq.8) then !third-pass parameters 3+5
              thresh0=1.5
              nfdistort=3
          elseif(ipass.eq.9) then !fourth-pass parameters 4+5
              thresh0=1.3
              nfdistort=2
          endif

       if(ipass.eq.2 .or. ipass.eq.3) then
          if(.not.fmaskact .and. ncandth25.gt.17) cycle
       endif

       npass1=.false.
       if(ipass.eq.1) npass1=.true.

       allocate(ss(NHSYM,NSZ), STAT = nAllocateStatus1)
       if (nAllocateStatus1 .ne. 0) STOP "Not enough memory"
       allocate(ss22(NHSYM,NSZ), STAT = nAllocateStatus1)
       if (nAllocateStatus1 .ne. 0) STOP "Not enough memory"

!       call timer('symsp65 ',0)
       call symspec65(ss,ss22,nfdistort)    !Get normalized symbol spectra
!       call timer('symsp65 ',1)

       if(.not.filter .and. .not.nagainfil) then
          nfa=nf1
          nfb=min(ntopfreq65,nf2)     ! bottom freq defined by waterfall, upper one limited to 3000Hz
       endif
       if(filter .or. nagainfil) then  ! +- 200Hz bandwidth for FILTER
          nfa=max(0,nfqso-200)
          nfb=min(ntopfreq65,nfqso+200) 
       endif

       if(ipass.ge.4 .and. ipass.le.6) then
          nfa=nfqso-3; nfb=nfqso+3
       endif
	   
!       call timer('sync65  ',0)
       call sync65(ss,ss22,fmask,nfa,nfb,ncand,npass1,nfqso,hint_on, &
                      nfhinted,thresh0,ncandth25,ipass)
!       call timer('sync65  ',1)

       if(ipass.eq.1 .and. ncand.lt.15) ncandipass1=ncand
       if(ncand.lt.15) nfilt=1600
       if(ncand.ge.15) nfilt=1580


  deallocate (ss, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'
  deallocate (ss22, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'

ntrials0=1; ntrials1=1
if(ntrials.gt.1) then
   if(.not.swl) then
     ntrials1=ntrials-1
   else 
     ntrials1=min(ntrials-1+3,7) ! defining number of decoding attempts for SWL mode
   endif
endif

if(ntrials.eq.1) then
   if(.not.swl) then
     ntrials0=ntrials
   else
     ntrials0=ntrials+3          ! defining number of decoding attempts for SWL mode
   endif
endif

if(npass1) then
    thresh3=3.0
    if(npass1 .and. ncand.gt.0 .and. ncandth25.lt.9) then
       syncmax1=0.      ! candidate with the highiest sync value
       syncmax2=0.      ! candidate with the 2nd most sync value
       syncmax3=0.      ! candidate with the 3rd most sync value
       do m=1,ncand
         if(ca(m)%sync.gt.syncmax1) then 
            syncmax1=ca(m)%sync
            imax1=m
         endif
       enddo
       do m=1,ncand
         if(ca(m)%sync.gt.syncmax2 .and. m.ne.imax1) then
            syncmax2=ca(m)%sync
            imax2=m
         endif
       enddo
       do m=1,ncand
         if(ca(m)%sync.gt.syncmax3 .and. m.ne.imax1 .and. m.ne.imax2) syncmax3=ca(m)%sync
       enddo
       if(syncmax1.lt.3.5 .and. ncandth25.lt.5) thresh3=2.3
       if(ncand.gt.1 .and. syncmax2.lt.3.5 .and. ncandth25.lt.6) thresh3=2.3
       if(ncand.gt.2 .and. syncmax3.lt.3.5 .and. ncandth25.lt.7) thresh3=2.3
    endif


    if(ncandth25.lt.9) then
       ncandcut=ncand
       do m=1,ncand
         if(ca(m)%sync.lt.thresh3) then
            fup=float(nfqso+nfhinted); fdown=float(nfqso-nfhinted)
            if(ca(m)%freq.ge.fup .or. ca(m)%freq.le.fdown) &
               ncandcut=ncandcut-1   ! number non QSOfreq candidates above thresh3
         endif
       enddo
    endif

    kp1=0 ! counter for fmask candidates

    do j=1,2

      if(ncand.eq.0) exit
  
      if(ntrials.gt.1) then
         if(ncandth25.eq.9) nvec=(10375*(2**(ntrials1-1)))/(4*ncandth25) 
         if(ncandth25.ge.10) nvec=(10375*(2**(ntrials1-1)))/(10*ncandth25) 
         if((ncandth25.gt.0 .and. ncandth25.lt.9) .or. (ncandth25.gt.0 .and. ncandcut.eq.0)) &
            nvec=(10375*(2**(ntrials1-1)))/ncandth25
      endif
      if(ntrials.eq.1) then
         if(ncandth25.eq.9) nvec=(5180*(2**(ntrials0-1)))/(4*ncandth25) 
         if(ncandth25.ge.10) nvec=(5180*(2**(ntrials0-1)))/(10*ncandth25) 
         if((ncandth25.gt.0 .and. ncandth25.lt.9) .or. (ncandth25.gt.0 .and. ncandcut.eq.0)) &
            nvec=(5180*(2**(ntrials0-1)))/ncandth25
      endif

      if(ncandth25.gt.0 .and. ncandcut.gt.0 .and. ncandth25.lt.9) then 
         if(.not.swl) then
            if(ntrials.eq.1) nvec=38000/ncandcut
            if(ntrials.eq.2) nvec=52000/ncandcut
            if(ntrials.eq.3) nvec=66000/ncandcut
            if(ntrials.eq.4) nvec=90000/ncandcut 
            if(ntrials.eq.5) nvec=140000/ncandcut
            if(ntrials.eq.6) nvec=264000/ncandcut
            if(ntrials.eq.7) nvec=390000/ncandcut
            if(ntrials.eq.8) nvec=480000/ncandcut
         else
            ntrialswl=min(ntrials+2,8)
            if(ntrialswl.eq.3) nvec=66000/ncandcut
            if(ntrialswl.eq.4) nvec=90000/ncandcut 
            if(ntrialswl.eq.5) nvec=140000/ncandcut
            if(ntrialswl.eq.6) nvec=264000/ncandcut
            if(ntrialswl.eq.7) nvec=390000/ncandcut
            if(ntrialswl.eq.8) nvec=480000/ncandcut
         endif
      endif

      do icand=1,ncand
        if(ca(icand)%freq.lt.0.0) cycle
        if(.not.swl) then
           if(j.eq.1 .and. ca(icand)%sync.lt.thresh3 .and. ncandth25.lt.9) then
              fup=float(nfqso+nfhinted); fdown=float(nfqso-nfhinted)
              if(ca(icand)%freq.ge.fup .or. ca(icand)%freq.le.fdown) cycle
           endif
        endif
        hintedrxfreq=.false.; ncand_lt9=.false.; sync1_lt1=.false.
        freq=ca(icand)%freq
        dtx=ca(icand)%dt
        sync1=ca(icand)%sync
        fup=float(nfqso+nfhinted); fdown=float(nfqso-nfhinted)
        if(freq.gt.fdown .and. freq.lt.fup) hintedrxfreq=.true.
        if(ncandth25.lt.9) ncand_lt9=.true. ! if number of candidates less then 9
        if(sync1.lt.1.0) sync1_lt1=.true.
        servis=' '; jt65bc=.false.
!        call timer('decod65a',0);
        call decode65a(newfft,freq,nvec,naggressive,hint_on,mycall,hiscall, &
             hisgrid,sync2,a,dtx,nft,decoded,ncand_lt9,hintedrxfreq,nfqso,hint,npass1, &
             sync1_lt1,ncandcount,nstophint,freemsg,nlasttx,ipass,nsdecatt,isfalse, &
             bypass_ftrsd,ncand,jt65bc,nharmonicsdepth,showharmonics)
!        call timer('decod65a',1)
        if(ncandcount.gt.MAXINTCAND) go to 4 ! input audio signal is distorted
        newfft=.false.; bypass_ftrsd=.false.
        if(j.eq.2 .and. decoded.eq.'                      ' .and. ca(icand)%sync.gt.thresh3) then
           if(ncandth25.lt.7 .and. kp1.gt.4) go to 8
           if(ncandth25.lt.15 .and. kp1.gt.9) go to 8
           if(ncandth25.ge.15 .and. kp1.gt.14) go to 8
           kp1=kp1+1
           fmask(kp1)=ca(icand)%freq
8       endif
        if(decoded.ne.'                      ') then
           newfft=.true.
           freqa1=freq+a(1)
           a2=a(2)
           nfreq=nint(freqa1)
           ndrift=nint(a(2))
           s2db=10.0*log10(sync2)
           if(s2db.lt.-0.51 .and. s2db.gt.-31.49) then
              nsnr=nint(s2db+snrd(nint(abs(s2db))))
           else
              nsnr=nint(s2db)
           endif
           if(nsnr.lt.-30) nsnr=-30
           if(nsnr.gt.-1) nsnr=-1

           ca(icand)%freq=-1.0

           dupe=.false. ! de-dedupe
           callcq='AAAAAA'
           callncq='BBBBBB'
           if(ndecoded.gt.0) then
              if(decoded(1:3).eq.'CQ ' .and. hint.eq.'*') then
                 call splitdupecq(decoded,callcq)
              endif
              do i=1, ndecoded
                if(decoded.eq.dec(i)%decoded) then
                   dupe=.true.
                   exit
                endif
! hide Hint CQ decode if there was FTRSD decode with the same call
              if(decoded(1:3).eq.'CQ ' .and. hint.eq.'*') then
                 if(.not.dec(i)%freemsg) then
                    call splitmsgdupe(dec(i)%decoded,callncq)
                    if(callncq.eq.callcq) then
                       dupe=.true.
                       exit
                    endif
                 endif
              endif
! hide Hint decode if there was FTRSD decode with the same calls done before on QSOfreq
                if(hint.eq.'*' .and. decoded(1:10).eq.dec(i)%decoded(1:10) .and. &
                   abs(freqa1-dec(i)%freq).le.10.0) then
                   dupe=.true.
                   exit
                endif
              enddo
           endif

!          call timer('subtr65 ',0)
           if(isfalse.ne.'?') then 
              if(hint.ne.'*' .or. (hint.eq.'*' .and. .not.dupe)) then
                 call subtract65(freqa1,dtx,a2,nfilt,jt65bc)
              endif
           endif
!          call timer('subtr65 ',1)

           if(decoded(2:5).eq.'....' .or. decoded(9:12).eq.'....') dupe=.true.
           if(filter .and. (nfreq.lt.nfqso-50 .or. nfreq.gt.nfqso+50)) dupe=.true.
           if(nagainfil .and. (nfreq.lt.nfqso-25 .or. nfreq.gt.nfqso+25)) dupe=.true.
           if(.not.dupe .and. .not.jt65bc(1) .and. .not.jt65bc(2)) then
! split and record data in the dynhint structure if it is not decoding again
              if(.not.freemsg .and. ninterval.ne.nint_prev .and. isfalse.ne.'?') &
                 call splitmsg(decoded,mycall,dtx,hintedrxfreq)
              if(decoded(1:3).eq."CQ ") call splitmsgcq(decoded,dtx)
              ndecpass1=ndecpass1+1
              ndecoded=ndecoded+1
              dec(ndecoded)%freq=freq+a(1)
              dec(ndecoded)%decoded=decoded
              dec(ndecoded)%hint=hint
              dec(ndecoded)%freemsg=freemsg
              if (associated(this%callback)) then
                  if(hint.eq.'*') servis=hint
                  if(isfalse.eq.'?') servis=isfalse
                  if(.not.hintedrxfreq .and. freemsg) servis='.'
                  if(hintedrxfreq .and. freemsg) servis=','
                  msg26=decoded//'    '
                  call this%callback(nutc,nsnr,dtx-1.0,nfreq,msg26,servis)
              endif
           endif
        endif
      enddo                                 !candidate loop
    enddo
endif

if(ipass.eq.2) then

   if(ncand.eq.0 .or. kp1.eq.0) cycle
   fmask=-1.0
   nveceq1=5180
   nvecgt1=10375
   if(ncandipass1.lt.15) then
      nveceq1=nveceq1*3
      if(ntrials.eq.2) nvecgt1=nvecgt1*1.7
   endif

   if(ntrials.gt.1) then
      if(ncand.gt.0) nvec=(nvecgt1*(2**(ntrials1-1)))/kp1 
   endif
   if(ntrials.eq.1) then   
      if(ncand.gt.0) nvec=(nveceq1*(2**(ntrials0-1)))/kp1 
   endif

   kp1=0 ! counter for fmask candidates

   do icand=1,ncand
      hintedrxfreq=.false.; ncand_lt9=.false.; sync1_lt1=.false.
      freq=ca(icand)%freq
      dtx=ca(icand)%dt
      sync1=ca(icand)%sync
      fup=float(nfqso+nfhinted); fdown=float(nfqso-nfhinted)
      if(freq.gt.fdown .and. freq.lt.fup) hintedrxfreq=.true.
      if(ncand.lt.9) ncand_lt9=.true. ! if number of candidates less then 9
      if(sync1.lt.1.0) sync1_lt1=.true.
      servis=' '; jt65bc=.false.
!        call timer('decod65a',0);
        call decode65a(newfft,freq,nvec,naggressive,hint_on,mycall,hiscall, &
             hisgrid,sync2,a,dtx,nft,decoded,ncand_lt9,hintedrxfreq,nfqso,hint,npass1, &
             sync1_lt1,ncandcount,nstophint,freemsg,nlasttx,ipass,nsdecatt,isfalse, &
             bypass_ftrsd,ncand,jt65bc,nharmonicsdepth,showharmonics)
!        call timer('decod65a',1)
        if(ncandcount.gt.MAXINTCAND) go to 4 ! input audio signal is distorted
        newfft=.false.; bypass_ftrsd=.false.
        if(decoded.eq.'                      ' .and. ca(icand)%sync.gt.thresh3) then
           if(ncandth25.lt.7 .and. kp1.gt.3) go to 16
           if(ncandth25.lt.15 .and. kp1.gt.7) go to 16
           if(ncandth25.ge.15 .and. kp1.gt.12) go to 16
           kp1=kp1+1
           fmask(kp1)=ca(icand)%freq
16       endif
        if(decoded.ne.'                      ') then
           newfft=.true.
           freqa1=freq+a(1)
           a2=a(2)
           nfreq=nint(freqa1)
           ndrift=nint(a(2))
           s2db=10.0*log10(sync2)
           if(s2db.lt.-0.51 .and. s2db.gt.-31.49) then
              nsnr=nint(s2db+snrd(nint(abs(s2db))))
           else
              nsnr=nint(s2db)
           endif
           if(nsnr.lt.-30) nsnr=-30
           if(nsnr.gt.-1) nsnr=-1

           ca(icand)%freq=-1.0

           dupe=.false. ! de-dedupe
           callcq='AAAAAA'
           callncq='BBBBBB'
           if(ndecoded.gt.0) then
              if(decoded(1:3).eq.'CQ ' .and. hint.eq.'*') then
                 call splitdupecq(decoded,callcq)
              endif
              do i=1, ndecoded
                if(decoded.eq.dec(i)%decoded) then
                   dupe=.true.
                   exit
                endif
! hide Hint CQ decode if there was FTRSD decode with the same call
              if(decoded(1:3).eq.'CQ ' .and. hint.eq.'*') then
                 if(.not.dec(i)%freemsg) then
                    call splitmsgdupe(dec(i)%decoded,callncq)
                    if(callncq.eq.callcq) then
                       dupe=.true.
                       exit
                    endif
                 endif
              endif
! hide Hint decode if there was FTRSD decode with the same calls done before on QSOfreq
                if(hint.eq.'*' .and. decoded(1:10).eq.dec(i)%decoded(1:10) .and. &
                   abs(freqa1-dec(i)%freq).le.10.0) then
                   dupe=.true.
                   exit
                endif
              enddo
           endif

!          call timer('subtr65 ',0)
           if(isfalse.ne.'?') then 
              if(hint.ne.'*' .or. (hint.eq.'*' .and. .not.dupe)) then
                 call subtract65(freqa1,dtx,a2,nfilt,jt65bc)
              endif
           endif
!          call timer('subtr65 ',1)

           if(decoded(2:5).eq.'....' .or. decoded(9:12).eq.'....') dupe=.true.
           if(filter .and. (nfreq.lt.nfqso-50 .or. nfreq.gt.nfqso+50)) dupe=.true.
           if(nagainfil .and. (nfreq.lt.nfqso-25 .or. nfreq.gt.nfqso+25)) dupe=.true.
           if(.not.dupe .and. .not.jt65bc(1) .and. .not.jt65bc(2)) then
! split and record data in the dynhint structure if it is not decoding again
              if(.not.freemsg .and. ninterval.ne.nint_prev .and. isfalse.ne.'?') &
                 call splitmsg(decoded,mycall,dtx,hintedrxfreq)
              if(decoded(1:3).eq."CQ ") call splitmsgcq(decoded,dtx)
              ndecoded=ndecoded+1
              dec(ndecoded)%freq=freq+a(1)
              dec(ndecoded)%decoded=decoded
              dec(ndecoded)%hint=hint
              dec(ndecoded)%freemsg=freemsg
              if (associated(this%callback)) then
                  if(hint.eq.'*') servis=hint
                  if(isfalse.eq.'?') servis=isfalse
                  if(.not.hintedrxfreq .and. freemsg) servis='.'
                  if(hintedrxfreq .and. freemsg) servis=','
                  msg26=decoded//'    '
                  call this%callback(nutc,nsnr,dtx-1.0,nfreq,msg26,servis)
              endif
           endif
        endif
      enddo                                 !candidate loop
endif



if(ipass.eq.3) then

   if(ncand.eq.0 .or. kp1.eq.0) cycle

   nveceq1=5180
   nvecgt1=10375
   if(ncandipass1.lt.15) then
      nveceq1=nveceq1*3
      if(ntrials.eq.2) nvecgt1=nvecgt1*1.7
   endif

   if(ntrials.gt.1) then
      if(ncand.gt.0) nvec=(nvecgt1*(2**(ntrials1-1)))/kp1 
   endif
   if(ntrials.eq.1) then   
      if(ncand.gt.0) nvec=(nveceq1*(2**(ntrials0-1)))/kp1 
   endif

   do icand=1,ncand
      hintedrxfreq=.false.; ncand_lt9=.false.; sync1_lt1=.false.
      freq=ca(icand)%freq
      dtx=ca(icand)%dt
      sync1=ca(icand)%sync
      fup=float(nfqso+nfhinted); fdown=float(nfqso-nfhinted)
      if(freq.gt.fdown .and. freq.lt.fup) hintedrxfreq=.true.
      if(ncand.lt.9) ncand_lt9=.true. ! if number of candidates less then 9
      if(sync1.lt.1.0) sync1_lt1=.true.
      servis=' '; jt65bc=.false.
!        call timer('decod65a',0);
        call decode65a(newfft,freq,nvec,naggressive,hint_on,mycall,hiscall, &
             hisgrid,sync2,a,dtx,nft,decoded,ncand_lt9,hintedrxfreq,nfqso,hint,npass1, &
             sync1_lt1,ncandcount,nstophint,freemsg,nlasttx,ipass,nsdecatt,isfalse, &
             bypass_ftrsd,ncand,jt65bc,nharmonicsdepth,showharmonics)
!        call timer('decod65a',1)
        if(ncandcount.gt.MAXINTCAND) go to 4 ! input audio signal is distorted
        newfft=.false.; bypass_ftrsd=.false.
        if(decoded.ne.'                      ') then
           newfft=.true.
           freqa1=freq+a(1)
           a2=a(2)
           nfreq=nint(freqa1)
           ndrift=nint(a(2))
           s2db=10.0*log10(sync2)
           if(s2db.lt.-0.51 .and. s2db.gt.-31.49) then
              nsnr=nint(s2db+snrd(nint(abs(s2db))))
           else
              nsnr=nint(s2db)
           endif
           if(nsnr.lt.-30) nsnr=-30
           if(nsnr.gt.-1) nsnr=-1

           ca(icand)%freq=-1.0

           dupe=.false. ! de-dedupe
           callcq='AAAAAA'
           callncq='BBBBBB'
           if(ndecoded.gt.0) then
              if(decoded(1:3).eq.'CQ ' .and. hint.eq.'*') then
                 call splitdupecq(decoded,callcq)
              endif
              do i=1, ndecoded
                if(decoded.eq.dec(i)%decoded) then
                   dupe=.true.
                   exit
                endif
! hide Hint CQ decode if there was FTRSD decode with the same call
              if(decoded(1:3).eq.'CQ ' .and. hint.eq.'*') then
                 if(.not.dec(i)%freemsg) then
                    call splitmsgdupe(dec(i)%decoded,callncq)
                    if(callncq.eq.callcq) then
                       dupe=.true.
                       exit
                    endif
                 endif
              endif
! hide Hint decode if there was FTRSD decode with the same calls done before on QSOfreq
                if(hint.eq.'*' .and. decoded(1:10).eq.dec(i)%decoded(1:10) .and. &
                   abs(freqa1-dec(i)%freq).le.10.0) then
                   dupe=.true.
                   exit
                endif
              enddo
           endif

!          call timer('subtr65 ',0)
           if(isfalse.ne.'?') then 
              if(hint.ne.'*' .or. (hint.eq.'*' .and. .not.dupe)) then
                 call subtract65(freqa1,dtx,a2,nfilt,jt65bc)
              endif
           endif
!          call timer('subtr65 ',1)

           if(decoded(2:5).eq.'....' .or. decoded(9:12).eq.'....') dupe=.true.
           if(filter .and. (nfreq.lt.nfqso-50 .or. nfreq.gt.nfqso+50)) dupe=.true.
           if(nagainfil .and. (nfreq.lt.nfqso-25 .or. nfreq.gt.nfqso+25)) dupe=.true.
           if(.not.dupe .and. .not.jt65bc(1) .and. .not.jt65bc(2)) then
! split and record data in the dynhint structure if it is not decoding again
              if(.not.freemsg .and. ninterval.ne.nint_prev .and. isfalse.ne.'?') &
                 call splitmsg(decoded,mycall,dtx,hintedrxfreq)
              if(decoded(1:3).eq."CQ ") call splitmsgcq(decoded,dtx)
              ndecoded=ndecoded+1
              dec(ndecoded)%freq=freq+a(1)
              dec(ndecoded)%decoded=decoded
              dec(ndecoded)%hint=hint
              dec(ndecoded)%freemsg=freemsg
              if (associated(this%callback)) then
                  if(hint.eq.'*') servis=hint
                  if(isfalse.eq.'?') servis=isfalse
                  if(.not.hintedrxfreq .and. freemsg) servis='.'
                  if(hintedrxfreq .and. freemsg) servis=','
                  msg26=decoded//'    '
                  call this%callback(nutc,nsnr,dtx-1.0,nfreq,msg26,servis)
              endif
           endif
        endif
      enddo                                 !candidate loop
endif



! single decode 3 subpasses
if(ipass.ge.4 .and. ipass.le.6) then

   if(.not.swl) then
      if(ntrials.eq.1) nvec=3800
      if(ntrials.eq.2) nvec=5200
      if(ntrials.eq.3) nvec=6600
      if(ntrials.eq.4) nvec=9000
      if(ntrials.eq.5) nvec=14000
      if(ntrials.eq.6) nvec=26400
      if(ntrials.eq.7) nvec=39000
      if(ntrials.eq.8) nvec=48000
   else
      ntrialswl=min(ntrials+2,8)
      if(ntrialswl.eq.3) nvec=6600
      if(ntrialswl.eq.4) nvec=9000
      if(ntrialswl.eq.5) nvec=14000
      if(ntrialswl.eq.6) nvec=26400
      if(ntrialswl.eq.7) nvec=39000
      if(ntrialswl.eq.8) nvec=48000
   endif

   nvec=nvec*(nsdecatt**2)

   do icand=1,ncand
      if(icand.gt.2) cycle
!      if(ca(icand)%freq.lt.0.0) cycle
      hintedrxfreq=.true.; ncand_lt9=.true.; sync1_lt1=.false.
      freq=ca(icand)%freq
      dtx=ca(icand)%dt
      sync1=ca(icand)%sync
      if(sync1.lt.1.0) sync1_lt1=.true.
      servis=' '; jt65bc=.false.
!      call timer('decod65a',0);
      call decode65a(newfft,freq,nvec,naggressive,hint_on,mycall,hiscall, &
             hisgrid,sync2,a,dtx,nft,decoded,ncand_lt9,hintedrxfreq,nfqso,hint,npass1, &
             sync1_lt1,ncandcount,nstophint,freemsg,nlasttx,ipass,nsdecatt,isfalse, &
             bypass_ftrsd,ncand,jt65bc,nharmonicsdepth,showharmonics)
!      call timer('decod65a',1)
      if(ncandcount.gt.MAXINTCAND) go to 4 ! input audio signal is distorted
      newfft=.false.; bypass_ftrsd=.false.
      if(decoded.ne.'                      ') then
         newfft=.true.
         freqa1=freq+a(1)
         a2=a(2)
         nfreq=nint(freqa1)
         ndrift=nint(a(2))
         s2db=10.0*log10(sync2)
         if(s2db.lt.-0.51 .and. s2db.gt.-31.49) then
            nsnr=nint(s2db+snrd(nint(abs(s2db))))
         else
            nsnr=nint(s2db)
         endif
         if(nsnr.lt.-30) nsnr=-30
         if(nsnr.gt.-1) nsnr=-1

!         ca(icand)%freq=-1.0

         dupe=.false. ! de-dedupe
           callcq='AAAAAA'
           callncq='BBBBBB'
           if(ndecoded.gt.0) then
              if(decoded(1:3).eq.'CQ ' .and. hint.eq.'*') then
                 call splitdupecq(decoded,callcq)
              endif
              do i=1, ndecoded
                 if(decoded.eq.dec(i)%decoded) then
                    dupe=.true.
                    exit
                 endif
! hide Hint CQ decode if there was FTRSD decode with the same call
              if(decoded(1:3).eq.'CQ ' .and. hint.eq.'*') then
                 if(.not.dec(i)%freemsg) then
                    call splitmsgdupe(dec(i)%decoded,callncq)
                    if(callncq.eq.callcq) then
                       dupe=.true.
                       exit
                    endif
                 endif
              endif
! hide Hint decode if there was FTRSD decode with the same calls done before on QSOfreq
                 if(hint.eq.'*' .and. decoded(1:10).eq.dec(i)%decoded(1:10) .and. &
                    abs(freqa1-dec(i)%freq).le.10.0) then
                    dupe=.true.
                    exit
                 endif
              enddo
           endif

!        call timer('subtr65 ',0)
         if(isfalse.ne.'?') then 
            if(hint.ne.'*' .or. (hint.eq.'*' .and. .not.dupe)) then
               call subtract65(freqa1,dtx,a2,nfilt,jt65bc)
            endif
         endif
!        call timer('subtr65 ',1)

         if(decoded(2:5).eq.'....' .or. decoded(9:12).eq.'....') dupe=.true.
         if(.not.dupe .and. .not.jt65bc(1) .and. .not.jt65bc(2)) then
! split and record data in the dynhint structure if it is not decoding again
            if(.not.freemsg .and. ninterval.ne.nint_prev .and. isfalse.ne.'?') &
               call splitmsg(decoded,mycall,dtx,hintedrxfreq)
            if(decoded(1:3).eq."CQ ") call splitmsgcq(decoded,dtx)
            ndecoded=ndecoded+1
            dec(ndecoded)%freq=freq+a(1)
            dec(ndecoded)%decoded=decoded
            dec(ndecoded)%hint=hint
            dec(ndecoded)%freemsg=freemsg
            if (associated(this%callback)) then
                if(hint.eq.'*') servis=hint
                if(isfalse.eq.'?') servis=isfalse
                if(.not.hintedrxfreq .and. freemsg) servis='.'
                if(hintedrxfreq .and. freemsg) servis=','
                msg26=decoded//'    '
                call this%callback(nutc,nsnr,dtx-1.0,nfreq,msg26,servis)
            endif
         endif
      endif
   enddo                                 !candidate loop
endif

! decoding passes 2,3,4
if(ipass.ge.7) then

   if(ncand.eq.0) cycle

   if(ipass.eq.7) ndecodedk=ndecoded
   ncandk=0
   if(ndecodedk.le.6) then
      do kk=1,10
         if(ca(kk)%sync.gt.2.5) ncandk=ncandk+1
      enddo
   endif

   if(ntrials.gt.1) then
      if(ndecpass1.ge.5) nvec=(10375*(2**(ntrials1-1)))/ncand
      if(ndecpass1.lt.5) nvec=(10375*(2**(ntrials1-1)))/10
      if(ncandk.gt.0) nvec=(10375*(2**(ntrials1-1)))/ncandk
   endif
   if(ntrials.eq.1) then   
      if(ndecpass1.ge.5) nvec=(5180*(2**(ntrials0-1)))/ncand
      if(ndecpass1.lt.5) nvec=(5180*(2**(ntrials0-1)))/10
      if(ncandk.gt.0) nvec=(5180*(2**(ntrials0-1)))/ncandk
   endif

   bypass_ftrsd=.false.
  
   do icand=1,ncand

      if(.not.hint_on) then
         if(ndecpass1.lt.5 .and. icand.gt.10) exit
         if(ncandk.gt.0 .and. icand.gt.ncandk) exit
      endif
      if(hint_on) then
         if(ndecpass1.lt.5 .and. icand.gt.10) bypass_ftrsd=.true.
         if(ncandk.gt.0 .and. icand.gt.ncandk) bypass_ftrsd=.true.
      endif

      hintedrxfreq=.false.; ncand_lt9=.false.; sync1_lt1=.false.
      freq=ca(icand)%freq
      dtx=ca(icand)%dt
      sync1=ca(icand)%sync
      fup=float(nfqso+nfhinted); fdown=float(nfqso-nfhinted)
      if(freq.gt.fdown .and. freq.lt.fup) hintedrxfreq=.true.
      if(ncand.lt.9) ncand_lt9=.true. ! if number of candidates less then 9
      if(sync1.lt.1.0) sync1_lt1=.true.
      servis=' '; jt65bc=.false.
!      call timer('decod65a',0)
      call decode65a(newfft,freq,nvec,naggressive,hint_on,mycall,hiscall, &
           hisgrid,sync2,a,dtx,nft,decoded,ncand_lt9,hintedrxfreq,nfqso,hint,npass1, &
           sync1_lt1,ncandcount,nstophint,freemsg,nlasttx,ipass,nsdecatt,isfalse, &
           bypass_ftrsd,ncand,jt65bc,nharmonicsdepth,showharmonics)
!      call timer('decod65a',1)
      if(ncandcount.gt.MAXINTCAND) go to 4 ! input audio signal is distorted
      newfft=.false.; bypass_ftrsd=.false.
      if(decoded.ne.'                      ') then
         newfft=.true.
         freqa1=freq+a(1)
         a2=a(2)
         nfreq=nint(freqa1)
         ndrift=nint(a(2))
         s2db=10.0*log10(sync2)
         if(s2db.lt.-0.51 .and. s2db.gt.-31.49) then
            nsnr=nint(s2db+snrd(nint(abs(s2db))))
         else
            nsnr=nint(s2db)
         endif
         if(nsnr.lt.-30) nsnr=-30
         if(nsnr.gt.-1) nsnr=-1

         dupe=.false. ! de-dedupe
         callcq='AAAAAA'
         callncq='BBBBBB'
         if(ndecoded.gt.0) then
            if(decoded(1:3).eq.'CQ ' .and. hint.eq.'*') then
               call splitdupecq(decoded,callcq)
            endif
            do i=1, ndecoded
               if(decoded.eq.dec(i)%decoded) then
                  dupe=.true.
                  exit
               endif
! hide Hint CQ decode if there was FTRSD decode with the same call
              if(decoded(1:3).eq.'CQ ' .and. hint.eq.'*') then
                 if(.not.dec(i)%freemsg) then
                    call splitmsgdupe(dec(i)%decoded,callncq)
                    if(callncq.eq.callcq) then
                       dupe=.true.
                       exit
                    endif
                 endif
              endif
! hide Hint decode if there was FTRSD decode with the same calls done before on QSOfreq
               if(hint.eq.'*' .and. decoded(1:10).eq.dec(i)%decoded(1:10) .and. &
                  abs(freqa1-dec(i)%freq).le.10.0) then
                  dupe=.true.
                  exit
               endif
            enddo
         endif

!        call timer('subtr65 ',0)
         if(isfalse.ne.'?') then 
            if(hint.ne.'*' .or. (hint.eq.'*' .and. .not.dupe)) then
               call subtract65(freqa1,dtx,a2,nfilt,jt65bc)
            endif
         endif
!          call timer('subtr65 ',1)

         if(decoded(2:5).eq.'....' .or. decoded(9:12).eq.'....') dupe=.true.
! hide decoded messages in 400Hz bandwidth if Filter turned ON, show only 100Hz BW
         if(filter .and. (nfreq.lt.nfqso-50 .or. nfreq.gt.nfqso+50)) dupe=.true.
! hide dupe decodes if frequency difference is greater than 25Hz 
         if(nagainfil .and. (nfreq.lt.nfqso-25 .or. nfreq.gt.nfqso+25)) dupe=.true.
         if(.not.dupe .and. .not.jt65bc(1) .and. .not.jt65bc(2)) then
! split and record data in the dynhint structure if it is not decoding again
              if(.not.freemsg .and. ninterval.ne.nint_prev .and. isfalse.ne.'?') &
                 call splitmsg(decoded,mycall,dtx,hintedrxfreq)
              if(decoded(1:3).eq."CQ ") call splitmsgcq(decoded,dtx)
            ndecoded=ndecoded+1
            dec(ndecoded)%freq=freq+a(1)
            dec(ndecoded)%decoded=decoded
            dec(ndecoded)%hint=hint
            dec(ndecoded)%freemsg=freemsg
            if(associated(this%callback)) then
               if(hint.eq.'*') servis=hint
               if(isfalse.eq.'?') servis=isfalse
               if(.not.hintedrxfreq .and. freemsg) servis='.'
               if(hintedrxfreq .and. freemsg) servis=','
               msg26=decoded//'    '
               call this%callback(nutc,nsnr,dtx-1.0,nfreq,msg26,servis)
            endif
         endif
      endif
   enddo                                 !candidate loop
endif
enddo                                   !four-pass loop

4 continue
! clear old data from dynhint data structure if it is not first run and
! if it is not the same wav file or the decoding again same interval
if(nint_prev.ge.0 .and. ninterval.ne.nint_prev) then
   do i=1,300
      if(dynhint(i)%ninterval.eq.-1) cycle
      if(ninterval-dynhint(i)%ninterval.lt.0) dynhint(i)%ninterval=-1
      if(ninterval-dynhint(i)%ninterval.gt.2) dynhint(i)%ninterval=-1
   enddo
   do i=1,150
      if(dynhintcq(i)%ninterval.eq.-1) cycle
      if(ninterval-dynhintcq(i)%ninterval.lt.0) dynhintcq(i)%ninterval=-1
      if(ninterval-dynhintcq(i)%ninterval.gt.2) dynhintcq(i)%ninterval=-1
   enddo   
   do i=1,20
      if(rxfreq(i)%ninterval.eq.-1) cycle
      if(ninterval-rxfreq(i)%ninterval.lt.0) rxfreq(i)%ninterval=-1
      if(ninterval-rxfreq(i)%ninterval.gt.6) rxfreq(i)%ninterval=-1
   enddo
endif

nint_prev=ninterval ! set as previous interval/wav time number

return
end subroutine decode

end module jt65_decode
