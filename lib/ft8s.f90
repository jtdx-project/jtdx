! last time modified by Igor UA3DJY on 20191119

subroutine ft8s(s8,srr,itone,msg37,lft8s,nft8rxfsens,stophint)

  use ft8_mod1, only : itone56,idtone56,msg,nlasttx,lastrxmsg,mycall,hiscall
  real, intent(in) :: s8(0:7,79),srr
  real s8_1(0:7,79),s8d(0:7,58),xsync(21),xmsync(21),xdata(58),xmdata(58),xnoise(79),xmnoise(79)
  character, intent(out) :: msg37*37
  character*37 lastrcvdmsg,msgsent37
  integer, intent(out) :: itone(79)
  integer, intent(in) :: nft8rxfsens
  integer itonedem(58),idtone(58),mrs(58),mrs2(58),mrs3(58),mrs4(58),ip1(1)
  logical(1), intent(in) :: stophint
  logical(1), intent(out) :: lft8s
  logical(1) lmatched(58),lmycall,lhiscall,lrrr,lr73,lcallingrprt,lastrrprt,lastreport,lgrid

! taken from ft8_mod1
!    integer itone56(56,79),idtone56(56,58)
!    character*37 msg(56)

!nlasttx  last TX message
!  0       Tx was halted
!  1      AA1AA BB1BB PL35
!  2      AA1AA BB1BB -15
!  3      AA1AA BB1BB R-15
!  4      AA1AA BB1BB RRR/RR73
!  5      AA1AA BB1BB 73
!  6      CQ BB1BB PL35

  lft8s=.false.
  if(stophint) return; if(len_trim(hiscall).lt.3) return; if(nlasttx.eq.6 .or. nlasttx.eq.0) return ! CQ or TX halted
  lgrid=.false.; nft8rxfslow=nft8rxfsens; lmycall=.false.; lastrcvdmsg=''
  lmatched=.false.; s8_1=s8; lhiscall=.false.; lrrr=.false.; lr73=.false.; lcallingrprt=.false.; lastrrprt=.false.
  lastreport=.false.; itonedem=11; ntresh1=26; ntresh2=38; if(srr.gt.7.0) then; ntresh1=29; ntresh2=41; endif

  if(lastrxmsg(1)%lstate) then; lastrcvdmsg=lastrxmsg(1)%lastmsg; else; lastrcvdmsg=''; endif

  if(lastrcvdmsg.ne.'                                     ') then
    if(index(lastrcvdmsg,trim(hiscall)).gt.0) lhiscall=.true.
    if(index(lastrcvdmsg,trim(mycall)).eq.1) lmycall=.true.
    if(lmycall .and. lhiscall) then
      if(index(lastrcvdmsg,' RRR').gt.0) lrrr=.true.
      if(index(lastrcvdmsg,' RR73').gt.0 .or. index(lastrcvdmsg,' 73').gt.0) lr73=.true.
    endif
  else
    if(nlasttx.eq.2) lcallingrprt=.true. ! calling with REPORT scenario
  endif
  if(lmycall .and. lhiscall) then
    if(index(lastrcvdmsg,' +').gt.0 .or. index(lastrcvdmsg,' -').gt.0) lastreport=.true.
    if(index(lastrcvdmsg,' R+').gt.0 .or. index(lastrcvdmsg,' R-').gt.0) lastrrprt=.true.
  endif
  if(lastrxmsg(1)%lstate .and. lmycall .and. lhiscall .and. .not.lastreport .and. .not. lastrrprt .and. .not.lrrr .and. &
    .not.lr73) lgrid=.true.
  if(lastrxmsg(1)%lstate .and. .not.lmycall .and. lhiscall .and. nlasttx.eq.2) lcallingrprt=.true. ! calling with REPORT

  if(lgrid .and. index(msg(53),trim(mycall)//' '//trim(hiscall)//' AA00').eq.1) then ! standard callsigns
    i3=-1; n3=-1
    msg(53)=lastrxmsg(1)%lastmsg
    call genft8sd(msg(53),i3,n3,msgsent37,msgbits,itone)
    idtone56(53,1:29)=itone(8:36)
    idtone56(53,30:58)=itone(44:72)
    itone56(53,1:79)=itone(1:79)
  endif

  do i=1,58
    if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
    else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
  enddo

  nmycall1=0; nbase1=0
  do k=1,19
    if(idtone56(1,k).eq.itonedem(k)) then; if(k.lt.10) nmycall1=nmycall1+1; nbase1=nbase1+1; endif
  enddo

! /'-01 ','-02 ','-03 ','-04 ','-05 ','-06 ','-07 ','-08 ','-09 ','-10 ',
!  '-11 ','-12 ','-13 ','-14 ','-15 ','-16 ','-17 ','-18 ','-19 ','-20 ',
!  '-21 ','-22 ','-23 ','-24 ','-25 ','-26 ','R-01','R-02','R-03','R-04',
!  'R-05','R-06','R-07','R-08','R-09','R-10','R-11','R-12','R-13','R-14',
!  'R-15','R-16','R-17','R-18','R-19','R-20','R-21','R-22','R-23','R-24',
!  'R-25','R-26','AA00','RRR ','RR73','73  '/

  ilow=1; ihigh=56
  if(nlasttx.eq.1) then; ilow=1; ihigh=26 ! GRID R+REPORT RRR RR73 73 are not valid msgs
  elseif(lgrid) then; ilow=27; ihigh=53 ! REPORT RRR RR73 73 are not valid msgs
  elseif(lcallingrprt) then; ilow=27; ihigh=52 ! GRID REPORT RRR RR73 73 are not valid msgs
  elseif(lastrrprt) then; ilow=27; ihigh=56 ! REPORT is not a valid msg, GRID shall be excluded later 
  elseif(lr73 .or. lrrr) then; ilow=54; ihigh=56 ! GRID, REPORT or R+REPORT is not a valid msg, 
! MF GRID shall be excluded later
  endif

  nmatchditer1=0; ncrcpatyiter1=0; imax=0
  do i=ilow,ihigh
    if(lastreport .and. i.gt.26 .and. i.lt.54) cycle ! RREPORT is not a valid msg, direction change is not allowed
    nmatch1=0; ncrcpaty1=0
    do k=1,58
      if(idtone56(i,k).eq.itonedem(k)) then; nmatch1=nmatch1+1; if(k.gt.25) ncrcpaty1=ncrcpaty1+1; endif
    enddo
    if(nmatch1.gt.nmatchditer1) then
      imax=i; nmatchditer1=nmatch1; ncrcpatyiter1=ncrcpaty1
    endif
  enddo
!write (*,"(i2,1x,i2,1x,i2,1x,i2)") nmatchlast1,nmatchditer1,ncrcpatylast1,ncrcpatyiter1
  if(imax.eq.0) return
  if(srr.gt.3.0 .and. (nmycall1.lt.5 .or. nbase1.lt.10)) return ! prevent false decodes from the strong signals
  if((nlasttx.eq.1 .or. lcallingrprt) .and. nmycall1.eq.0) return ! answer to other operator
  if(lr73 .and. (imax.eq.53 .or. imax.eq.54)) return ! RRR,GRID is not a valid msg

  nmatch1=nmatchditer1; ncrcpaty1=ncrcpatyiter1
  idtone(1:58)=idtone56(imax,1:58)
  do k=1,58
    if(idtone(k).eq.itonedem(k)) lmatched(k)=.true. 
  enddo

  if(ncrcpaty1.lt.8) return
  
  thresh=0.0; i1=0; i2=0; i33=0; i4=0
  do i=1,58; if(i.le.29) then; s8d(0:7,i)=s8(0:7,i+7); else; s8d(0:7,i)=s8(0:7,i+14); endif; enddo
  do j=1,58
    s1=-1.0E6; s2=-1.0E6; s3=-1.0E6; s4=-1.0E6
    do i=0,7; if(s8d(i,j).gt.s1) then; s1=s8d(i,j); i1=i; endif; enddo
    do i=0,7; if(i.ne.i1 .and. s8d(i,j).gt.s2) then; s2=s8d(i,j); i2=i; endif; enddo
    do i=0,7; if(i.ne.i1 .and. i.ne.i2 .and. s8d(i,j).gt.s3) then; s3=s8d(i,j); i33=i; endif; enddo
    do i=0,7; if(i.ne.i1 .and. i.ne.i2 .and. i.ne.i33 .and. s8d(i,j).gt.s4) then; s4=s8d(i,j); i4=i; endif; enddo
    mrs(j)=i1; mrs2(j)=i2; mrs3(j)=i33; mrs4(j)=i4
  enddo
  ref0=0.; ref0paty=0.; ref0mycl=0.; ref0oth=0.
  do i=1,58
    ref0=ref0 + s8d(mrs(i),i)
    if(i.gt.26) then; ref0paty=ref0paty + s8d(mrs(i),i)
    else if(i.lt.10) then; ref0mycl=ref0mycl + s8d(mrs(i),i)
    endif
  enddo
  ref0oth=ref0mycl+ref0paty
  ipk=0; u1=0.; u2=0.; u1paty=0.; u2paty=0.; u1oth=0.; u2oth=0.
  do k=ilow,ihigh
    if(lastreport .and. k.gt.26 .and. k.lt.54) cycle ! GRID, RREPORT is not a valid msg, direction change is not allowed
    psum=0.0; ref=ref0; psumpaty=0.; refpaty=ref0paty; psumoth=0.; refoth=ref0oth
    do j=1,58; i=idtone56(k,j); psum=psum + s8d(i,j)
      if(j.gt.26) then; psumpaty=psumpaty + s8d(i,j); psumoth=psumoth + s8d(i,j)
      else if(j.lt.10) then; psumoth=psumoth + s8d(i,j)
      endif
      if(i.eq.mrs(j)) then
        stmp=s8d(mrs2(j),j)-s8d(i,j); ref=ref + stmp
        if(j.gt.26) then; refpaty=refpaty + stmp; refoth=refoth + stmp
        else if(j.lt.10) then; refoth=refoth + stmp
        endif
      endif
      if(i.eq.mrs2(j)) then
        stmp=s8d(mrs3(j),j) - s8d(mrs2(j),j); ref=ref + stmp
        if(j.gt.26) then; refpaty=refpaty + stmp; refoth=refoth + stmp
        else if(j.lt.10) then; refoth=refoth + stmp
        endif
      endif
      if(i.eq.mrs3(j)) then
        stmp=s8d(mrs4(j),j) - s8d(mrs3(j),j); ref=ref + stmp
        if(j.gt.26) then; refpaty=refpaty + stmp; refoth=refoth + stmp
        else if(j.lt.10) then; refoth=refoth + stmp
        endif
      endif
!!      if(i.eq.mrs2(j)) then; ref=ref - s8d(mrs(j),j) + s8d(mrs3(j),j); endif
!!      if(i.eq.mrs3(j)) then; ref=ref - s8d(mrs(j),j) + s8d(mrs4(j),j); endif
    enddo
    p=psum/ref; ppaty=psumpaty/refpaty; poth=psumoth/refoth
    if(p.gt.u1) then; u2=u1; u1=p; u2paty=u1paty; u1paty=ppaty; u2oth=u1oth; u1oth=poth; ipk=k
    else if(p.gt.u2) then; u2=p; u2paty=ppaty; u2oth=poth
    endif
  enddo
  if(lastrrprt .and. ipk.eq.53) return ! GRID is not valid
  if(lr73 .and. ipk.lt.55) return

  if(lcallingrprt .or. nlasttx.eq.1) nft8rxfslow=1

  if(ipk.ne.0) then
    qual=100.0*(u1-u2); qualp=100.0*(u1paty-u2paty); qualo=100.0*(u1oth-u2oth)
    thresh=(qual+10.0)*(u1-0.6); if(thresh.lt.1.5) return
    threshp=(qualp+10.0)*(u1paty-0.6); thresho=(qualo+10.0)*(u1oth-0.6)
    if((lcallingrprt .or. nlasttx.eq.1) .and. thresho.lt.3.43) return
    if(thresho.lt.2.63 .or. threshp.lt.2.45) return
    if(((nft8rxfslow.eq.1 .and. thresh.gt.4.0) .or. (nft8rxfslow.eq.2 .and. thresh.gt.3.55) &
      .or. (nft8rxfslow.eq.3 .and. thresh.gt.3.0)) .and. qual.gt.2.6 .and. u1.gt.0.77) then
!if(ipk.ne.7) print *,'match1',thresh
      lft8s=.true.; msg37=msg(ipk); itone(1:79)=itone56(ipk,1:79); go to 2
    endif
  endif

  if(imax.eq.ipk .and. (nft8rxfslow.gt.1 .or. (nft8rxfslow.eq.1 .and. thresh.gt.2.7)) .and. srr.lt.7.0 &
    .and. (ncrcpaty1.gt.14 .or. (nmatch1.gt.22 .and. ncrcpaty1.gt.13))) then
!if(imax.ne.7) print *,'11',thresh
    lft8s=.true.; msg37=msg(imax); itone(1:79)=itone56(imax,1:79); go to 2
  endif

  if(imax.eq.ipk .and. nmatch1.gt.ntresh1 .and. ncrcpaty1.gt.10) then
!if(imax.ne.7) print *,'12',thresh
    lft8s=.true.; msg37=msg(imax); itone(1:79)=itone56(imax,1:79); go to 2
  endif

  if(nmatchditer1.ge.16) then

    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
    enddo
    nmatch2=nmatch1; ncrcpaty2=ncrcpaty1
    do k=1,58
      if(lmatched(k)) cycle
      if(idtone(k).eq.itonedem(k)) then; nmatch2=nmatch2+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty2=ncrcpaty2+1; endif
    enddo
    if(nmatch2.gt.ntresh2 .and. ncrcpaty2.gt.19) then
!if(imax.ne.7) print *,'2',thresh
       lft8s=.true.; msg37=msg(imax); itone(1:79)=itone56(imax,1:79); go to 2
    endif

    if(srr.gt.7.0) return ! do not process strong signals anymore

    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
    enddo
    nmatch3=nmatch2; ncrcpaty3=ncrcpaty2
    do k=1,58
      if(lmatched(k)) cycle
      if(idtone(k).eq.itonedem(k)) then; nmatch3=nmatch3+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty3=ncrcpaty3+1; endif
    enddo
    if(nmatch3.gt.44 .and. ncrcpaty3.gt.21) then
!if(imax.ne.7) print *,'3',thresh
      lft8s=.true.; msg37=msg(imax); itone(1:79)=itone56(imax,1:79); go to 2
    endif

    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
    enddo
    nmatch4=nmatch3; ncrcpaty4=ncrcpaty3
    do k=1,58
      if(lmatched(k)) cycle
      if(idtone(k).eq.itonedem(k)) then; nmatch4=nmatch4+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty4=ncrcpaty4+1; endif
    enddo
    if((nft8rxfslow.eq.3 .or. (nft8rxfslow.eq.2 .and. thresh.gt.2.2)) .and. nmatch4.gt.47 .and. ncrcpaty4.gt.23) then
!if(imax.ne.7) print *,'4',thresh
      lft8s=.true.; msg37=msg(imax); itone(1:79)=itone56(imax,1:79); go to 2
    endif

    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
    enddo
    nmatch5=nmatch4; ncrcpaty5=ncrcpaty4
    do k=1,58
      if(lmatched(k)) cycle
      if(idtone(k).eq.itonedem(k)) then; nmatch5=nmatch5+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty5=ncrcpaty5+1; endif
    enddo
    if((nft8rxfslow.eq.3 .or. (nft8rxfslow.eq.1 .and. thresh.gt.3.4) .or. (nft8rxfslow.eq.2 .and. thresh.gt.3.25)) &
      .and. nmatch5.gt.50 .and. (nmatch1.gt.21 .or. nmatch2.gt.31 .or. nmatch3.gt.38 .or. nmatch4.gt.46) &
      .and. ncrcpaty5.gt.25) then
!if(imax.ne.7) print *,'5',thresh
      lft8s=.true.; msg37=msg(imax); itone(1:79)=itone56(imax,1:79); go to 2
    endif

    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
    enddo
    nmatch6=nmatch5; ncrcpaty6=ncrcpaty5
    do k=1,58
      if(lmatched(k)) cycle
      if(idtone(k).eq.itonedem(k)) then; nmatch6=nmatch6+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty6=ncrcpaty6+1; endif
    enddo
    if(((nft8rxfslow.eq.2 .and. thresh.gt.2.6) .or. (nft8rxfslow.eq.3 .and. thresh.gt.2.22)) &
      .and. imax.eq.ipk .and. ncrcpaty6.gt.26) then
!if(imax.ne.7) print *,'61',thresh
!print *,'61',thresh
!write (*,"(i2,1x,i2,1x,f5.2)") 6,ncrcpaty6,thresh
      lft8s=.true.; msg37=msg(imax); itone(1:79)=itone56(imax,1:79); go to 2
    endif

    if(nft8rxfslow.eq.1) then
      if(nmatch6.gt.54 .and. (nmatch1.gt.22 .or. nmatch2.gt.27 .or. nmatch3.gt.35 .or. &
        (nmatch2-nmatch1).gt.9 .or. (nmatch3-nmatch2).gt.10) .and. ncrcpaty6.gt.29 .and. thresh.gt.3.15) then
!if(imax.ne.7) print *,'62',thresh
!print *,'62',thresh
        lft8s=.true.; msg37=msg(imax); itone(1:79)=itone56(imax,1:79); go to 2
      endif
    endif

    if(ncrcpaty6.gt.29) then
      if((nft8rxfslow.eq.3 .or. (nft8rxfslow.eq.3 .and. thresh.gt.1.94)) .and. imax.eq.ipk .and. ncrcpaty2-ncrcpaty1.gt.3 &
        .and. ncrcpaty3-ncrcpaty2.gt.3 .and. ncrcpaty4-ncrcpaty3.gt.3 .and. ncrcpaty6-ncrcpaty5.lt.6) then
!if(imax.ne.7) print *,'63',thresh
!print *,'63',thresh
!if(imax.ne.7) write (*,"(i2,1x,f5.2,1x,f5.2,1x,f5.2)") 62,u1,qual,thresh
        lft8s=.true.; msg37=msg(imax); itone(1:79)=itone56(imax,1:79); go to 2
      endif
    endif
  endif

2 if(lft8s) then
    snr=0.0; snrbase=0.0; snrpaty=0.0; snrdata=0.0; snrsync=0.0; snrmycall=0.0; snrother=0.0
    do i=1,79
      xsig=s8(itone(i),i); xnoi=(sum(s8(0:7,i)) - xsig)/7.0; snr1=xsig/(xnoi+1E-6); snr=snr+snr1
      if(i.gt.7 .and. i.lt.34) then; snrbase=snrbase + snr1; if(i.lt.17) snrmycall=snrmycall + snr1; endif
      if((i.gt.43 .and. i.lt.73) .or. (i.gt.33 .and. i.lt.37)) then; snrpaty=snrpaty+snr1; endif
    enddo
    snrdata=snrbase+snrpaty; snrsync=snr-snrdata; snrother=snrmycall+snrpaty
    snrsync=snrsync/21.0; snrother=snrother/48.0; snrpaty=snrpaty/32.0
    if(lcallingrprt .or. nlasttx.eq.1) then
      soratio=snrsync/snrother
      if(soratio.gt.1.29) then;lft8s=.false.; msg37=''; return; endif
    endif
    spratio=snrsync/snrpaty
    if(spratio.lt.0.6 .or. spratio.gt.1.25) then;lft8s=.false.; msg37=''; return; endif

    do i=1,7; xsync(i)=s8(itone(i),i); enddo; do i=8,14; k=i+29; xsync(i)=s8(itone(k),k); enddo
    do i=15,21; k=i+58; xsync(i)=s8(itone(k),k); enddo
    do i=1,29; k=i+7; xdata(i)=s8(itone(k),k); enddo; do i=30,58; k=i+14; xdata(i)=s8(itone(k),k); enddo
    do i=1,79; k=modulo(itone(i)+4,8); xnoise(i)=s8(k,i); enddo

    do i=1,19
      if((xsync(i).gt.xsync(i+1) .and. xsync(i).lt.xsync(i+2)) &
        .or. (xsync(i).lt.xsync(i+1) .and. xsync(i).gt.xsync(i+2))) then; xmsync(i)=xsync(i)
      else if((xsync(i+1).gt.xsync(i) .and. xsync(i+1).lt.xsync(i+2)) &
        .or. (xsync(i+1).lt.xsync(i) .and. xsync(i+1).gt.xsync(i+2))) then; xmsync(i)=xsync(i+1)
      else if((xsync(i+2).gt.xsync(i) .and. xsync(i+2).lt.xsync(i+1)) &
        .or. (xsync(i+2).lt.xsync(i) .and. xsync(i+2).gt.xsync(i+1))) then; xmsync(i)=xsync(i+2)
      else; xmsync(i)=xsync(i)
      endif
    enddo
    xmsync(20)=xmsync(18); xmsync(21)=xmsync(19)

    do i=1,56
      if((xdata(i).gt.xdata(i+1) .and. xdata(i).lt.xdata(i+2)) &
        .or. (xdata(i).lt.xdata(i+1) .and. xdata(i).gt.xdata(i+2))) then; xmdata(i)=xdata(i)
      else if((xdata(i+1).gt.xdata(i) .and. xdata(i+1).lt.xdata(i+2)) &
        .or. (xdata(i+1).lt.xdata(i) .and. xdata(i+1).gt.xdata(i+2))) then; xmdata(i)=xdata(i+1)
      else if((xdata(i+2).gt.xdata(i) .and. xdata(i+2).lt.xdata(i+1)) &
        .or. (xdata(i+2).lt.xdata(i) .and. xdata(i+2).gt.xdata(i+1))) then; xmdata(i)=xdata(i+2)
      else; xmdata(i)=xdata(i)
      endif
    enddo
    xmdata(57)=xmdata(55); xmdata(58)=xmdata(56)

    do i=1,77
      if((xnoise(i).gt.xnoise(i+1) .and. xnoise(i).lt.xnoise(i+2)) &
        .or. (xnoise(i).lt.xnoise(i+1) .and. xnoise(i).gt.xnoise(i+2))) then; xmnoise(i)=xnoise(i)
      else if((xnoise(i+1).gt.xnoise(i) .and. xnoise(i+1).lt.xnoise(i+2)) &
        .or. (xnoise(i+1).lt.xnoise(i) .and. xnoise(i+1).gt.xnoise(i+2))) then; xmnoise(i)=xnoise(i+1)
      else if((xnoise(i+2).gt.xnoise(i) .and. xnoise(i+2).lt.xnoise(i+1)) &
        .or. (xnoise(i+2).lt.xnoise(i) .and. xnoise(i+2).gt.xnoise(i+1))) then; xmnoise(i)=xnoise(i+2)
      else; xmnoise(i)=xnoise(i)
      endif
    enddo
    xmnoise(78)=xmnoise(76); xmnoise(79)=xmnoise(77)

    ssync=sum(xmsync)/21.0; spaty=sum(xmdata(27:58))/32.0; spnoise=(sum(xmnoise(34:36)) + sum(xmnoise(44:72)))/32.0
    spother=(sum(xmdata(1:9)) + sum(xmdata(27:58)))/41.0
    spratiom=ssync/(spaty+1E-6); spnratiom=spaty/(spnoise+1E-6); sporatiom=ssync/(spother+1E-6)
    if(spnratiom.gt.2.3) then
      if(lcallingrprt .or. nlasttx.eq.1) then; if(sporatiom.gt.1.35) then; lft8s=.false.; msg37=''; return; endif
      else; if(spratiom.gt.1.35) then; lft8s=.false.; msg37=''; return; endif
      endif
    endif
  endif

  return
end subroutine ft8s
