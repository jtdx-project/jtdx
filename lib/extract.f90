subroutine extract(s3b,s3c,ntrials,hint_on,mycall_12,hiscall_12,hisgrid,  &
           msg_decoded,decoded,freemsg,nft,hintedrxfreq,hint,npass1, &
           hintedw,bypass_hintcq,bypass_hintall,bypass_ftrsd,nstophint, &
           hinteddyn,dt,hintedrxfdt,syncpwr,nlasttx,ipass,sync2,isfalse,jt65bc, &
           nharmonicsdepth,showharmonics)

! Input:
!   s3       64-point spectra for each of 63 data symbols

! Output:
!   msg_decoded  true if message is decoded
!   nhist    maximum number of identical symbol values
!   decoded  decoded message (if ncount >=0)
!   freemsg    true if decoded message is free text
!   nft      0=no decode; 1=FT decode; 2=hinted decode

! nlasttx last transmitted message matrix
! nlasttx   last TX message  expected RX message
!   0           Halt TX              -
!   1           GRID             -01, CQ
!   2           -01                R-01, and might be again GRID
!   3           R-01            RRR/RR73/73,  and might be again -01
!   4           RRR/RR73        RRR/RR73/73
!   5           73/freemsg      RRR/RR73/73
!   6           CQ              GRID fm CALL3
!   8           freemsg         RRR/RR73/73

! nlastrx last received message matrix, used for wideband hintdyn decoder, dynhint(300) structure
! nlastrx   last RX message   / dinterval1 expected inversed msg / dinterval2 expected RX message
!   0           initialization/               none              / none
!   1           GRID          /               -01               / R-01, and might be again GRID
!   2           -01           /               R-01              / RRR/RR73/73, and might be again -01
!   3           R-01          /               RRR/RR73/73       / RRR/RR73/73,  and might be again R-01
!   4           RRR/RR73/73   /               RRR/RR73/73       / none

  use, intrinsic :: iso_c_binding
  use packjt
  use jt65_mod2 ! s3(64,63),s3a(64,63),correct(63), correct_hint(63)
  use jt65_mod8 !  dynhint(300),dynhintcq(150),rxfreq(20),rxfchg(1),ninterval,freq,maxsync2,maxsync2freq
  use timer_module, only: timer

  character decoded*22,mycall*6,hiscall*6,hisgrid4*4,hint*1,mycall_12*12,hiscall_12*12,hisgrid*6, &
            call1*6,call2*6,call1_old*6,call2_old*6,direction*2,callsign*6,grid*4,directionrxf*2,grid2*4,&
            isfalse*1,mycall12*12
  integer dat4(12),mrsym(63),mr2sym(63),mrprob(63),mr2prob(63),tmp(63),param(0:5),mrs(63),mrs2(63), &
          mrsymb(63),mr2symb(63),mrprobb(63),mr2probb(63)
  integer nft,nfail,nbirdie,npct,ncand,nlastrx
  logical(1) hintedrxfreq,hintedw,freemsg,hint_on,msg_decoded,hint_call3,hint_wide,hint_dx,&
             bypass_hintcq,bypass_hintall,bypass_ftrsd,npass1,ftrsd_cfmd, &
             falsedec,nstophint,hint_dyn,hinteddyn,hinteddyncq,hintedrxfdt,inverse,lhiscall, &
             jtbcand,jtccand,jt65bc(2),showharmonics
  real s3b(64,63),s3c(64,63),s3temp(64,63),syncpwr

  call1_old=' '; call2_old=' '
  mycall=mycall_12(1:6)
  mycall12='' !not used in chkfalse() for JT65
  hiscall=hiscall_12(1:6)
  hisgrid4=hisgrid(1:4)
  npct=50
  nft=0
  nfail=0
  decoded='                      '
  hinteddyncq=.true.; inverse=.false.; jtbcand=.false.; jtccand=.false.; jt65bc=.false.
  nvect=ntrials
  sdratio=1.0; sdratiob=0.0; sdratioc=0.0

 ! nstophint=.false. ! dedicated for tests only, comment this line for SW release
 ! bypass_ftrsd=.true. ! dedicated for tests only, comment this line for SW release
 ! bypass_hintcq=.true. ! dedicated for tests only, comment this line for SW release
 ! hinteddyn=.false. ! dedicated for tests only, comment this line for SW release
 ! hinteddyncq=.false. ! dedicated for tests only, comment this line for SW release

  call pctile(s3,4032,npct,base)

  s3=s3/base
  nbirdie=6
  hint=' '
  isfalse=' '
  msg_decoded=.true.; hint_wide=.false.; hint_call3=.false.; hint_dx=.false.; ncand=16
  hint_dyn=.false.
  ftrsd_cfmd=.false.
! Get most reliable and second-most-reliable symbol values, and their
! probabilities

1 call demod64a(mrsym,mrprob,mr2sym,mr2prob)

  call chkhist(mrsym,nhist,ipk)       !Test for birdies and QRM
  if(nhist.ge.nbirdie) then
     nfail=nfail+1
     s3(ipk,1:63)=1.0
     if(nfail.gt.30) then
        decoded='                      '; msg_decoded=.false.; mrs=mrsym; mrs2=mr2sym; go to 8
     endif
     go to 1
  endif
  s3a=s3
  mrs=mrsym
  mrs2=mr2sym
!go to 128 ! diag only
  if(ipass.lt.4 .or. ipass.gt.6) then
     if(bypass_ftrsd .or. sync2.lt.0.0021) then ! 0.0021 => -30.0 dB SNR
        nft=0; decoded='                      '; msg_decoded=.false.; go to 8
     endif
  endif

  pdata1=0.
  do j=1,63
     pdata1=pdata1+s3(mrsym(j)+1,j)
  enddo
  sdratio=syncpwr/(pdata1*base)
! applying SYNC/DATA thresholds
  if(sdratio.lt.0.15) then; nft=0; decoded='                      '; msg_decoded=.false.; go to 2; endif

  if(nharmonicsdepth.ge.1 .and. sdratio.gt.1.81) then ! jt65bc decoding applying SYNC/DATA thresholds

     s3=s3b
     nfail=0
64   call demod64a(mrsym,mrprob,mr2sym,mr2prob)
     call chkhist(mrsym,nhist,ipk)       !Test for birdies and QRM
     if(nhist.ge.nbirdie) then
        nfail=nfail+1
        s3(ipk,1:63)=1.0 !=base
        if(nfail.gt.30) then; nft=0; decoded='                      '; msg_decoded=.false.; go to 4096; endif
        go to 64
     endif
     pdata1=0.
     do j=1,63
        pdata1=pdata1+s3(mrsym(j)+1,j)
     enddo
     sdratiob=syncpwr/pdata1
     if(sdratiob.gt.0.15 .and. sdratiob.lt.1.81) then
        jtbcand=.true.
        mrsymb=mrsym; mrprobb=mrprob; mr2symb=mr2sym; mr2probb=mr2prob
        s3temp=s3
     endif

4096 s3=s3c
     nfail=0
256  call demod64a(mrsym,mrprob,mr2sym,mr2prob)
     call chkhist(mrsym,nhist,ipk)       !Test for birdies and QRM
     if(nhist.ge.nbirdie) then
        nfail=nfail+1
        s3(ipk,1:63)=1.0 !=base
        if(nfail.gt.30) then; nft=0; decoded='                      '; msg_decoded=.false.; go to 32; endif
        go to 256
     endif
     pdata1=0.
     do j=1,63
        pdata1=pdata1+s3(mrsym(j)+1,j)
     enddo
     sdratioc=syncpwr/pdata1
     if(sdratioc.gt.0.15 .and. sdratioc.lt.1.81) jtccand=.true.
     if((jtbcand .and. .not.jtccand) .or. (.not.jtbcand .and. jtccand)) go to 8192
     if(.not.jtbcand .and. .not.jtccand) then
        nft=0; decoded='                      '; msg_decoded=.false.; go to 32
     endif
     if(jtbcand .and. jtccand) then
        dsdratiob=abs(sdratiob-1.0); dsdratioc=abs(sdratioc-1.0)
        if(dsdratiob .lt. dsdratioc) then; jtccand=.false.; else; jtbcand=.false.; endif
     endif
8192 if(jtbcand) then
        s3a=s3temp
        mrsym=mrsymb; mrprob=mrprobb; mr2sym=mr2symb; mr2prob=mr2probb
     endif
     if(jtccand) s3a=s3
!print *, 'bc cand'
     if(nharmonicsdepth.lt.4) then; nvect=10; else; nvect=100; endif
  endif

  call graycode65(mrsym,63,-1)        !Remove gray code 
  call interleave63(mrsym,-1)         !Remove interleaving
  call interleave63(mrprob,-1)

  call graycode65(mr2sym,63,-1)      !Remove gray code and interleaving
  call interleave63(mr2sym,-1)       !from second-most-reliable symbols
  call interleave63(mr2prob,-1)

  ! syncpwr -24dB SNR threshold for srand activation, srand for RX freq passes
!  if((npass1.and.ncand_lt9) .or. syncpwr.lt.200.0 .or. (ipass.ge.4 .and. ipass.le.6)) ncand=0
!  call timer('ftrsd   ',0)
  param=0; correct=0
  call ftrsd2(mrsym,mrprob,mr2sym,mr2prob,nvect,ipass,correct,param)
!  call timer('ftrsd   ',1)
  ncandidates=param(0)
  nhard=param(1)
  nsoft=param(2)
  nerased=param(3)
  rtt=0.001*param(4)
  ntotal=param(5)
  if(ntotal.le.81 .and. rtt.le.0.87) nft=1
  if(nhard.gt.49 .or. ntotal.gt.83 .or. rtt.gt.0.90) then
     nft=0; msg_decoded=.false.; decoded='                      '
     if(jtbcand .or. jtccand) then; go to 32; else; go to 8; endif
  endif

  msg_decoded=.false.
  decoded='                      '
  freemsg=.false.
  if(nft.eq.1 .and. nhard.ge.0) then
! Turn the corrected symbol array into channel symbols for subtraction;
! pass it back to jt65a via module "jt65_mod2".
     do i=1,12
        dat4(i)=correct(13-i)
     enddo
     do i=1,63
       tmp(i)=correct(64-i)
     enddo
     correct(1:63)=tmp(1:63)
     call interleave63(correct,1)
     call graycode65(correct,63,1)
     call unpackmsg(dat4,decoded)     !Unpack the user message
     if(decoded.ne.'                      ') then
        msg_decoded=.true.
        if(iand(dat4(10),8).ne.0) freemsg=.true.
        if(sync2.gt.2.0) then
           if(sync2.gt.maxsync2) then
              maxsync2=sync2
              maxsync2freq=freq
           endif
        endif
     else
        nft=0
     endif
  endif

! check some FTRSD decodes with matched filter
  if(nft.eq.1 .and. (nhard.gt.30 .or. nsoft.gt.0)) then
! protection from getting standard message like 'HK0M/MB8STK EQ7OMT'
     if(.not.freemsg) then 
        i1=index(decoded,' ')
        i2=index(decoded(i1+1:),' ')
        i2=i2+i1
        if(i1.gt.7 .or. (i1.gt.4 .and. i2.gt.14)) then
           nft=0; msg_decoded=.false.; decoded='                      '
           if(jtbcand .or. jtccand) then; go to 32; else; go to 8; endif
        endif
     endif

     if(.not.freemsg .and. (decoded(1:3).eq."CQ " .or. decoded(1:4).eq."QRZ " .or. decoded(1:3).eq."DE ")) then 
        falsedec=.false.
        call filterscq(decoded,falsedec)
        if(falsedec) then
           nft=0; msg_decoded=.false.; decoded='                      '
           if(jtbcand .or. jtccand) then; go to 32; else; go to 8; endif
        endif
     endif

     if(.not.freemsg .and. decoded(1:3).eq."DE ") then 
        falsedec=.false.
        call filtersde(decoded,falsedec)
        if(falsedec) then
           nft=0; msg_decoded=.false.; decoded='                      '
           if(jtbcand .or. jtccand) then; go to 32; else; go to 8; endif
        endif
     endif

     if(.not.freemsg .and. decoded(1:3).ne."CQ " .and. decoded(1:3).ne."DE " .and. decoded(1:3).ne."QRZ") then
        falsedec=.false.
        call filtersstd(decoded,mycall,falsedec,syncpwr)
        if(falsedec) then
           nft=0; msg_decoded=.false.; decoded='                      '
           if(jtbcand .or. jtccand) then; go to 32; else; go to 8; endif
        endif
     endif

     if(freemsg .and. decoded(1:3).ne."CQ " .and. decoded(1:3).ne."DE " .and. decoded(1:3).ne."QRZ") then
        falsedec=.false.
        call filtersfree(decoded,falsedec)
        if(falsedec) then
           nft=0; msg_decoded=.false.; decoded='                      '
           if(jtbcand .or. jtccand) then; go to 32; else; go to 8; endif
        endif
     endif

     if(decoded(1:3).eq."CQ ".or. decoded(1:3).eq."DE " .or. decoded(1:3).eq."QRZ" .or. freemsg) then
        if(showharmonics) then
           if(msg_decoded .and. jtbcand) print *,'2nd-h',nint(freq),'# ',decoded
           if(msg_decoded .and. jtccand) print *,'3rd-h',nint(freq),'# ',decoded
        endif
        go to 32
     endif
 
     ftrsd_cfmd=.false.
     falsedec=.false.
     if(.not.jtbcand .or. .not.jtccand) call chkftrsd(mrs,mrs2,decoded,ftrsd_cfmd)
     call chkfalse(decoded,mycall12,falsedec)
     if(ftrsd_cfmd) then
        if(falsedec .and. nhard.gt.44) isfalse='?'
     else
        if(falsedec) then        
           nft=0; msg_decoded=.false.; decoded='                      '
           if(jtbcand .or. jtccand) go to 32
        endif
     endif
     if(showharmonics .and. .not.falsedec .and. msg_decoded .and. (jtbcand .or. jtccand)) then
        if(msg_decoded .and. jtbcand) print *,'2nd-h',nint(freq),'# ',decoded
        if(msg_decoded .and. jtccand) print *,'3rd-h',nint(freq),'# ',decoded
        go to 32
     endif
	 
     if(ftrsd_cfmd) go to 32
  endif

     if(showharmonics .and. msg_decoded .and. (jtbcand .or. jtccand)) then
        if(msg_decoded .and. jtbcand) print *,'2nd-h',nint(freq),'# ',decoded
        if(msg_decoded .and. jtccand) print *,'3rd-h',nint(freq),'# ',decoded
        go to 32
     endif

8  if(bypass_hintall .and. nft.eq.0) then
      msg_decoded=.false.; decoded='                      '; go to 2
   endif

   if(nft.eq.0 .and. hinteddyncq .and. hint_on) then
      do i=1,150
         if(dynhintcq(i)%ninterval.lt.0) cycle
         if(ninterval.gt.0 .and. (ninterval-dynhintcq(i)%ninterval).eq.2 .and. & 
            (ninterval-dynhintcq(i)%ninterval).gt.0 .and. abs(freq-dynhintcq(i)%freq).lt.3.0) then

               direction=dynhintcq(i)%direction
               callsign=dynhintcq(i)%callsign
               grid=dynhintcq(i)%grid
               if(abs(dt-dynhintcq(i)%dt).gt.0.2) cycle     ! applying DT window +-0.2 sec

            call hintdyncq(mrs,mrs2,direction,callsign,grid,decoded,hint_dyn)
            if(hint_dyn) then
               nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
            else
               cycle ! multiple call on single frequency support
            endif
         endif
      enddo
      msg_decoded=.false.; decoded='                      '
   endif 

   if(bypass_hintcq .or. sync2.lt.0.0021 .or. maxsync2.gt.100.0) go to 4 ! 0.0021 => -30.0 dB SNR
   if(maxsync2.gt.2.0 .and. abs(freq-maxsync2freq).lt.2.0) go to 4 ! ban false Hint CQ under strong signals
   if(maxsync2.gt.3.0 .and. hintedrxfreq .and. sync2.lt.0.003)  go to 4 ! ban some false Hint CQ on RX freq -28dB SNR
!128 continue !diag only
   if(nft.eq.0 .and. hintedw .and. hint_on) then

      msg_decoded=.false.
      decoded='                      '

      call hintwidecq(mrs,mrs2,decoded,hint_wide,npass1)
      if(hint_wide) then
         nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
      endif

      call hintwidedx(mrs,mrs2,decoded,hint_wide,npass1)
      if(hint_wide) then
         nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
      endif
   endif
!go to 2 ! diag only 
4  if(nft.eq.0 .and. hint_on) then
      do i=1,300
         if(dynhint(i)%ninterval.lt.0) cycle
         if(ninterval.gt.0 .and. (ninterval-dynhint(i)%ninterval).le.2 .and. & 
            (ninterval-dynhint(i)%ninterval).gt.0 .and. abs(freq-dynhint(i)%freq).lt.3.0) then
            if((ninterval-dynhint(i)%ninterval).eq.1) then
               if(.not.hinteddyn) cycle ! drop one cycle if candidate is out of hinteddyn DT window
               inverse=.true.
               call1=dynhint(i)%call2
               call2=dynhint(i)%call1
            endif
            if((ninterval-dynhint(i)%ninterval).eq.2) then
               inverse=.false.
               call1=dynhint(i)%call1
               call2=dynhint(i)%call2
               if(abs(dt-dynhint(i)%dt).gt.0.2) cycle     ! applying DT window +-0.2 sec
            endif
            if(call1.eq.call1_old .and. call2.eq.call1_old) cycle
            call1_old=call1; call2_old=call2
            nlastrx=dynhint(i)%nlastrx
            grid2=dynhint(i)%grid2
            call hintdyn(mrs,mrs2,call1,call2,grid2,decoded,hint_dyn,inverse,nlastrx)
            if(hint_dyn) then
               nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
            else
               cycle ! multiple call on single frequency support
            endif
         endif
      enddo
      msg_decoded=.false.
      decoded='                      '
   endif

!  hintedrxfdt is the only variable controlled via DT Range of the Advanced tab settings
   if(hintedrxfreq .and. hintedrxfdt .and. nft.eq.0 .and. hint_on) then

      decoded='                      '
      msg_decoded=.false.
      hint_call3=.false.

      if(.not.nstophint .and. nlasttx.ge.4 .and. nlasttx.le.8) then
      call hintrxgrid(mrs,mrs2,mycall,decoded,hint_call3)
         if(hint_call3) then
            nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
         endif
      endif

      if(maxsync2.gt.3.0 .and. sync2.lt.0.003)  go to 16 ! ban some false Hint CQ on RX freq -28dB SNR
      if(maxsync2.gt.10.0)  go to 16 ! ban false Hint CQ if there are strong signals on the band
      call hintrxcq(mrs,mrs2,decoded,hint_call3)
      if(hint_call3) then
         nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
      endif
16    continue
! This memory consuming decoder may be implemented just in case if someone will call
! on RX frequency with signal report rather than grid. Deactivated it as most such calls 
! coming on the non RX frequency
! 
!      if(.not.nstophint) then
!         call hintrxrmin(s3,mrs,mrs2,mycall,decoded,hint_call3)
!         if(hint_call3) then
!            nft=2
!            hint='*'
!            msg_decoded=.true.
!            freemsg=.false.
!            go to 2
!         endif
!      endif

   endif

   if(hintedrxfreq .and. nft.eq.0 .and. hint_on) then

      if(hiscall(1:2).ne.'') then
         if(.not.nstophint) then
            hint_dx=.false.

            lhiscall=.false.
            do i=1,20
               if(rxfreq(i)%call2.eq.hiscall) then
                  lhiscall=.true.
                  exit
               endif
            enddo

               if(nlasttx.lt.2 .or. nlasttx.gt.3) then
                  directionrxf='00'
                  do i=1,20
                     if((ninterval-rxfreq(i)%ninterval).ne.2) cycle
                     if(abs(dt-rxfreq(i)%dt).gt.0.2) cycle ! applying DT window +-0.2 sec for high sensitivity mode
                     if(rxfreq(i)%call2.eq.hiscall .and. rxfreq(i)%direction.ne.'00') then
                        directionrxf=rxfreq(i)%direction
                        exit
                     endif
                  enddo

!128 if(hintedrxfreq) then ; directionrxf='00' !diag only
                  call hintdxcq(mrs,mrs2,hiscall,hisgrid4,decoded,directionrxf,hint_dx)
                  if(hint_dx) then
                     nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
                  endif
!endif; go to 2 ! diag only
               endif


            if(lhiscall) then
               do i=1,20
                  if(rxfreq(i)%ninterval.lt.0) cycle
                  if(ninterval.gt.0 .and. (ninterval-rxfreq(i)%ninterval).le.6 &
                     .and. (ninterval-rxfreq(i)%ninterval).gt.0 .and. &
                     abs(freq-rxfreq(i)%freq).lt.3.0) then

                     if((ninterval-rxfreq(i)%ninterval).eq.1 .or. &
                        (ninterval-rxfreq(i)%ninterval).eq.3 .or. &
                        (ninterval-rxfreq(i)%ninterval).eq.5) cycle
			 
                     if((ninterval-rxfreq(i)%ninterval).eq.2 .or. &
                        (ninterval-rxfreq(i)%ninterval).eq.4 .or. &
                        (ninterval-rxfreq(i)%ninterval).eq.6) then
                        if(rxfreq(i)%call2.eq.hiscall) then

                           if(abs(dt-rxfreq(i)%dt).gt.0.2) cycle     ! applying DT window +-0.2 sec
                           if(nlasttx.eq.2) then ! decoder for mycall+hiscall+grid message retransmission
!128 if(hintedrxfreq) then !diag only
                              call hintdxgrid(mrs,mrs2,mycall,hiscall,hisgrid,decoded,hint_dx)
                              if(hint_dx) then; nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
                              endif
!endif; go to 2 ! diag only
                           endif
                        endif
                     endif
                  endif
               enddo
            endif

            if(nlasttx.gt.0 .and. nlasttx.le.8 .and. nlasttx.ne.6) then
               do i=1,20
                  if(rxfreq(i)%ninterval.lt.0) cycle
                  if(ninterval.gt.0 .and. (ninterval-rxfreq(i)%ninterval).le.6 &
                     .and. (ninterval-rxfreq(i)%ninterval).gt.0 .and. &
                     abs(freq-rxfreq(i)%freq).lt.3.0) then

                     if((ninterval-rxfreq(i)%ninterval).eq.1 .or. &
                        (ninterval-rxfreq(i)%ninterval).eq.3 .or. &
                        (ninterval-rxfreq(i)%ninterval).eq.5) cycle
			 
                    if((ninterval-rxfreq(i)%ninterval).eq.2 .or. &
                       (ninterval-rxfreq(i)%ninterval).eq.4 .or. &
                       (ninterval-rxfreq(i)%ninterval).eq.6) then
                       if(rxfreq(i)%call2.eq.hiscall) then

                          if(abs(dt-rxfreq(i)%dt).gt.0.2) cycle     ! applying DT window +-0.2 sec
                          if(nlasttx.eq.1 .or. nlasttx.eq.3) then
                             call hintdxr(mrs,mrs2,mycall,hiscall,decoded,hint_dx)
                             if(hint_dx) then
                                nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
                             endif
                          endif
                          if(nlasttx.eq.2) then
! 128 if(hintedrxfreq) then ! diag only
                             call hintdxrr(mrs,mrs2,mycall,hiscall,decoded,hint_dx)
                             if(hint_dx) then
                                nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
                             endif
! endif; go to 2 ! diag only
                          endif
                          if(nlasttx.ge.4 .and. nlasttx.le.8 .and. nlasttx.ne.6) then
                             call hintdx73(mrs,mrs2,mycall,hiscall,decoded,hint_dx)
                             if(hint_dx) then
                                nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
                             endif
                          endif
                       endif
                    endif
                 endif
              enddo
            endif

         endif

         if(nstophint .and. rxfchg(1)%rxfchanged) then
            hint_dx=.false.

            directionrxf='00'
            do i=1,20
               if((ninterval-rxfreq(i)%ninterval).ne.2) cycle
               if(abs(dt-rxfreq(i)%dt).gt.0.2) cycle ! applying DT window +-0.2 sec for high sensitivity mode
               if(rxfreq(i)%call2.eq.hiscall .and. rxfreq(i)%direction.ne.'00') then
                  directionrxf=rxfreq(i)%direction
                  exit
               endif
            enddo

            call hintdxcq(mrs,mrs2,hiscall,hisgrid4,decoded,directionrxf,hint_dx)
            if(hint_dx) then
               nft=2; hint='*'; msg_decoded=.true.; freemsg=.false.; go to 2
            endif

         endif
      endif    
   endif

2 if(hint_wide .or. hint_dyn .or. hint_call3 .or. hint_dx) correct(1:63)=correct_hint(1:63)

!  if(nft.eq.1 .and. msg_decoded .and. (sdratio.lt.0.15 .or. sdratio.gt.1.81))  print *,sdratio
  if(nharmonicsdepth.eq.2) sdthreshold=1.0
  if(nharmonicsdepth.ge.3) sdthreshold=0.0
  if(.not.msg_decoded .and. nharmonicsdepth.ge.2 .and. sdratio.gt.sdthreshold) then !try more jt65bc candidates

     s3=s3b
     nfail=0
512  call demod64a(mrsym,mrprob,mr2sym,mr2prob)
     call chkhist(mrsym,nhist,ipk)       !Test for birdies and QRM
     if(nhist.ge.nbirdie) then
        nfail=nfail+1
        s3(ipk,1:63)=1.0 !=base
        if(nfail.gt.30) then
           nft=0; decoded='                      '; msg_decoded=.false.; go to 2048
        endif
        go to 512
     endif
     pdata1=0.
     do j=1,63
        pdata1=pdata1+s3(mrsym(j)+1,j)
     enddo
     sdratiob=syncpwr/pdata1
     if(sdratiob.gt.0.6 .and. sdratiob.lt.1.81) then
        jtbcand=.true.
        mrsymb=mrsym; mrprobb=mrprob; mr2symb=mr2sym; mr2probb=mr2prob
        s3temp=s3
     endif

2048 s3=s3c
     nfail=0
1024 call demod64a(mrsym,mrprob,mr2sym,mr2prob)
     call chkhist(mrsym,nhist,ipk)       !Test for birdies and QRM
     if(nhist.ge.nbirdie) then
        nfail=nfail+1
        s3(ipk,1:63)=1.0 !=base
        if(nfail.gt.30) then
           nft=0; decoded='                      '; msg_decoded=.false.; go to 32
        endif
        go to 1024
     endif
     pdata1=0.
     do j=1,63
        pdata1=pdata1+s3(mrsym(j)+1,j)
     enddo
     sdratioc=syncpwr/pdata1
     if(sdratioc.gt.0.6 .and. sdratioc.lt.1.81) jtccand=.true.
     if((jtbcand .and. .not.jtccand) .or. (.not.jtbcand .and. jtccand)) go to 8194
     if(.not.jtbcand .and. .not.jtccand) then
        nft=0; decoded='                      '; msg_decoded=.false.; go to 32
     endif
     if(jtbcand .and. jtccand) then
        dsdratiob=abs(sdratiob-1.0); dsdratioc=abs(sdratioc-1.0)
        if(dsdratiob .lt. dsdratioc) then
           jtccand=.false.
        else
           jtbcand=.false.
        endif
     endif
8194 if(jtbcand) then
        s3a=s3temp
        mrsym=mrsymb; mrprob=mrprobb; mr2sym=mr2symb; mr2prob=mr2probb
     endif
     if(jtccand) s3a=s3

     call graycode65(mrsym,63,-1)        !Remove gray code 
     call interleave63(mrsym,-1)         !Remove interleaving
     call interleave63(mrprob,-1)

     call graycode65(mr2sym,63,-1)      !Remove gray code and interleaving
     call interleave63(mr2sym,-1)       !from second-most-reliable symbols
     call interleave63(mr2prob,-1)

!  call timer('ftrsd   ',0)
     param=0; correct=0
     if(nharmonicsdepth.lt.4) then; nvect=10; else; nvect=100; endif
     call ftrsd2(mrsym,mrprob,mr2sym,mr2prob,nvect,ipass,correct,param)
!  call timer('ftrsd   ',1)
     ncandidates=param(0)
     nhard=param(1)
     nsoft=param(2)
     nerased=param(3)
     rtt=0.001*param(4)
     ntotal=param(5)
     if(ntotal.le.81 .and. rtt.le.0.87) nft=1
     if(nhard.gt.49 .or. ntotal.gt.83 .or. rtt.gt.0.90) then
        nft=0; msg_decoded=.false.; decoded='                      '; go to 32
     endif

     msg_decoded=.false.
     decoded='                      '
     freemsg=.false.
     if(nft.eq.1 .and. nhard.ge.0) then
! Turn the corrected symbol array into channel symbols for subtraction;
! pass it back to jt65a via module "jt65_mod2".
        do i=1,12
           dat4(i)=correct(13-i)
        enddo
        do i=1,63
          tmp(i)=correct(64-i)
        enddo
        correct(1:63)=tmp(1:63)
        call interleave63(correct,1)
        call graycode65(correct,63,1)
        call unpackmsg(dat4,decoded)     !Unpack the user message
        if(decoded.ne.'                      ') then
           msg_decoded=.true.
           if(iand(dat4(10),8).ne.0) freemsg=.true.
           if(sync2.gt.2.0) then
              if(sync2.gt.maxsync2) then
                 maxsync2=sync2
                 maxsync2freq=freq
              endif
           endif
        else
           nft=0; go to 32
        endif
     endif

! check some FTRSD decodes with matched filter
     if(nft.eq.1 .and. (nhard.gt.30 .or. nsoft.gt.0)) then
! protection from getting standard message like 'HK0M/MB8STK EQ7OMT'
        if(.not.freemsg) then 
           i1=index(decoded,' ')
           i2=index(decoded(i1+1:),' ')
           i2=i2+i1
           if(i1.gt.7 .or. (i1.gt.4 .and. i2.gt.14)) then
              nft=0; msg_decoded=.false.; decoded='                      '; go to 32
           endif
        endif

        if(.not.freemsg .and. (decoded(1:3).eq."CQ " .or. decoded(1:4).eq."QRZ " .or. decoded(1:3).eq."DE ")) then
           falsedec=.false.
           call filterscq(decoded,falsedec)
           if(falsedec) then; nft=0; msg_decoded=.false.; decoded='                      '; go to 32; endif
        endif

        if(.not.freemsg .and. decoded(1:3).eq."DE ") then 
           falsedec=.false.
           call filtersde(decoded,falsedec)
           if(falsedec) then; nft=0; msg_decoded=.false.; decoded='                      '; go to 32; endif
        endif

        if(.not.freemsg .and. decoded(1:3).ne."CQ " &
           .and. decoded(1:3).ne."DE " .and. decoded(1:3).ne."QRZ") then
           falsedec=.false.
           call filtersstd(decoded,mycall,falsedec,syncpwr)
           if(falsedec) then; nft=0; msg_decoded=.false.; decoded='                      '; go to 32; endif
        endif

        if(freemsg .and. decoded(1:3).ne."CQ " .and. decoded(1:3).ne."DE " .and. decoded(1:3).ne."QRZ") then
           falsedec=.false.
           call filtersfree(decoded,falsedec)
           if(falsedec) then; nft=0; msg_decoded=.false.; decoded='                      '; go to 32; endif
        endif

        if(decoded(1:3).eq."CQ ".or. decoded(1:3).eq."DE " .or. decoded(1:3).eq."QRZ" .or. freemsg) then
           if(showharmonics) then
              if(msg_decoded .and. jtbcand) print *,'2nd-h',nint(freq),'# ',decoded
              if(msg_decoded .and. jtccand) print *,'3rd-h',nint(freq),'# ',decoded
           endif
           go to 32
        endif
 
        falsedec=.false.
        call chkfalse(decoded,mycall12,falsedec)
        if(falsedec) then; nft=0; msg_decoded=.false.; decoded='                      '; go to 32; endif
        if(showharmonics .and. msg_decoded) then
           if(msg_decoded .and. jtbcand) print *,'2nd-h',nint(freq),'# ',decoded
           if(msg_decoded .and. jtccand) print *,'3rd-h',nint(freq),'# ',decoded
           go to 32
        endif
     endif
     if(showharmonics .and. msg_decoded) then
        if(msg_decoded .and. jtbcand) print *,'2nd-h',nint(freq),'# ',decoded
        if(msg_decoded .and. jtccand) print *,'3rd-h',nint(freq),'# ',decoded
        go to 32
     endif
  endif

32 if(msg_decoded) then; if(jtbcand) jt65bc(1)=.true.; if(jtccand) jt65bc(2)=.true.; endif
  
  return
end subroutine extract

subroutine getpp(workdat,pp)

  use, intrinsic :: iso_c_binding
  use jt65_mod2, only : s3a
  integer workdat(63),b(63)
  real pp

  b(1:63)=workdat(63:1:-1)
  call interleave63(b,1)
  call graycode(b,63,1,b)
  
  psum=0.
  do j=1,63
     i=b(j)+1
     x=s3a(i,j)
     s3a(i,j)=0.
     psum=psum + x
     s3a(i,j)=x
  enddo
  pp=psum/63.0

  return
end subroutine getpp
