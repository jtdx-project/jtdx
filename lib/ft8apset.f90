subroutine ft8apset(lmycallstd,lhiscallstd,numthreads)

  use packjt77
  use ft8_mod1, only : apsym,mycall,hiscall,apsymsp,lhound,apsymdxns1,apsymdxnsrrr,mybcall,hisbcall,apcqsym,hisgrid4,     &
                       apsymdxnsrr73,apsymdxns73,apsymmyns1,apsymmyns2,apsymmynsrr73,apsymmyns73,apsymdxstd,apsymdxnsr73, &
                       apsymdxns732,apsymmynsrrr
  character*77 c77
  character*37 msg,msgchk
  character*12 hiscallt,mycallprev,hiscallprev
  logical lnohiscall,unpk77_success,first
  logical(1) lhoundprev
  logical(1), intent(in) :: lmycallstd,lhiscallstd
  data mycallprev/'QQ2QQ'/
  data hiscallprev/'QQ1QQ'/
  data lhoundprev/.false./
  data first/.true./
  save hiscallprev,mycallprev,lhoundprev,first

  if(hiscall.ne.hiscallprev .or. mycall.ne.mycallprev .or. (lhound.neqv.lhoundprev) .or. first) then ! first for lhound triggered

    first=.false.; mycallprev=mycall; lhoundprev=lhound

! shall hash both callsigns for making AP masks with nonstandard callsign message
    if(.not.lhound) call fillhash(numthreads,.true.)

    lnohiscall=.false.
    hiscallt=hiscall
    if(len_trim(hiscallt).lt.3) then
      hiscallt=mycall  ! use mycall for dummy hiscall - mycall won't be hashed
      lnohiscall=.true.
    endif

    if(hiscall.ne.hiscallprev) then
      hiscallprev=hiscall
! Encode a dummy standard message: i3=1, 28 1 28 1 1 15
      if(len_trim(hiscall).gt.2) then
        if(lhiscallstd .and. len_trim(hisgrid4).eq.4) then; msg='CQ '//trim(hiscall)//' '//trim(hisgrid4)
        else; msg='CQ '//trim(hiscall)
        endif

        call pack77(msg,i3,n3,c77,0)
        call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
        if(lhiscallstd .and. i3.ne.1 .or. .not.lhiscallstd .and. i3.ne.4 .or. (msg.ne.msgchk) .or. .not.unpk77_success) go to 1
        read(c77,'(77i1)',err=1) apcqsym(1:77)
        apcqsym=2*apcqsym-1
      endif

      if(lhiscallstd .and. .not.lmycallstd) then
        msg=trim(hiscall)//' '//trim(hiscall)//' RRR'
        i3=0; n3=0
        call pack77(msg,i3,n3,c77,0)
        call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
        if(i3.ne.1 .or. (msg.ne.msgchk) .or. .not.unpk77_success) go to 2
        read(c77,'(58i1)',err=2) apsymdxstd(1:58)
        apsymdxstd=2*apsymdxstd-1
      endif

      if(.not.lhound .and. .not.lhiscallstd .and. len_trim(hiscall).gt.2) then
! nonstandard DXCall searching
        msg='<W9XYZ> '//trim(hiscall)//' RR73'
        call pack77(msg,i3,n3,c77,0)
        call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
        if(i3.ne.4 .or. .not.unpk77_success) go to 3
        read(c77,'(77i1)',err=3) apsymdxnsrr73(1:77)
        apsymdxnsrr73=2*apsymdxnsrr73-1

        msg='<W9XYZ> '//trim(hiscall)//' 73'
        call pack77(msg,i3,n3,c77,0)
        call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
        if(i3.ne.4 .or. .not.unpk77_success) go to 4
        read(c77,'(77i1)',err=4) apsymdxns73(1:77)
        apsymdxns73=2*apsymdxns73-1
      endif
    endif

    if(.not.lhound .and. .not.lmycallstd .and. len_trim(mycall).gt.2) then
      if(lhiscallstd) then
        msg='<'//trim(mycall)//'> '//trim(hiscall)//' -15'
        call pack77(msg,i3,n3,c77,0)
        call unpack77(c77,1,msgchk,unpk77_success,25)
        if(i3.ne.1 .or. msg.ne.msgchk .or. .not.unpk77_success) go to 5
        read(c77,'(58i1)',err=5) apsymmyns2(1:58)
        apsymmyns2=2*apsymmyns2-1

        nlenmyc=len_trim(mycall)
        msg=trim(mycall)//' <'//trim(hiscall)//'> RR73'
        call pack77(msg,i3,n3,c77,0)
        call unpack77(c77,1,msgchk,unpk77_success,25)
        if(i3.ne.4 .or. msg.ne.msgchk .or. .not.unpk77_success) go to 6
        read(c77,'(77i1)',err=6) apsymmynsrr73(1:77)
        apsymmynsrr73=2*apsymmynsrr73-1
        msg=trim(mycall)//' <'//trim(hiscall)//'> 73'
        call pack77(msg,i3,n3,c77,0)
        call unpack77(c77,1,msgchk,unpk77_success,25)
        if(i3.ne.4 .or. msg.ne.msgchk .or. .not.unpk77_success) go to 7
        read(c77,'(77i1)',err=7) apsymmyns73(1:77)
        apsymmyns73=2*apsymmyns73-1
        msg=trim(mycall)//' <'//trim(hiscall)//'> RRR'
        call pack77(msg,i3,n3,c77,0)
        call unpack77(c77,1,msgchk,unpk77_success,25)
        if(i3.ne.4 .or. msg.ne.msgchk .or. .not.unpk77_success) go to 15
        read(c77,'(77i1)',err=15) apsymmynsrrr(1:77)
        apsymmynsrrr=2*apsymmynsrrr-1

      else if(lnohiscall) then

        msg='<'//trim(mycall)//'> ZZ1ZZZ -15'
        call pack77(msg,i3,n3,c77,0)
        call unpack77(c77,1,msgchk,unpk77_success,25)
        if(i3.ne.1 .or. msgchk.ne.msg .or. .not.unpk77_success) go to 8
        read(c77,'(29i1)',err=8) apsymmyns1(1:29)
        apsymmyns1=2*apsymmyns1-1
      endif
    endif

    if(len_trim(mycall).lt.3) return

    if(.not.lhound .and. lmycallstd .and. (lhiscallstd .or. lnohiscall)) then
      msg=trim(mycall)//' '//trim(hiscallt)//' RRR'
      i3=0; n3=0
      call pack77(msg,i3,n3,c77,0)
      call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
      if(i3.ne.1 .or. (msg.ne.msgchk) .or. .not.unpk77_success) go to 9
      read(c77,'(58i1)',err=9) apsym(1:58)
      apsym=2*apsym-1; if(lnohiscall) apsym(30)=99
    endif

    if(lhound) then
! standard messages from Fox, always base callsigns
      if(len_trim(hisbcall).gt.2 .and. len_trim(mybcall).gt.2) then
        msg=trim(mybcall)//' '//trim(hisbcall)//' -15'
        call pack77(msg,i3,n3,c77,0)
        call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
        if(i3.ne.1 .or. (msg.ne.msgchk) .or. .not.unpk77_success) go to 9
        read(c77,'(58i1)',err=9) apsym(1:58)
        apsym=2*apsym-1
      else
        apsym(30)=99
      endif

! special messages
      msg=trim(mycall)//' RR73; '//trim(mycall)//' <'//trim(hiscallt)//'> -16'
      i3=0; n3=1
      call pack77(msg,i3,n3,c77,0)
      call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
      i1=0; i1=index(msgchk,'<'); if(i1.lt.15) return
      if(i3.ne.0 .or. msg(1:i1).ne.msgchk(1:i1) .or. .not.unpk77_success) go to 10
      read(c77,'(66i1)',err=10) apsymsp(1:66)
      apsymsp=2*apsymsp-1!; if(lnohiscall) apsymsp(30)=99
    endif

    if(.not.lhound .and. lmycallstd .and. .not.lhiscallstd .and. len_trim(hiscall).gt.2) then
      msg=trim(mycall)//' <'//trim(hiscall)//'> -16' ! report, rreport
      call pack77(msg,i3,n3,c77,0)
      call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
      if(i3.ne.1 .or. msg.ne.msgchk .or. .not.unpk77_success) go to 11
      read(c77,'(58i1)',err=11) apsymdxns1(1:58)
      apsymdxns1=2*apsymdxns1-1

      msg='<'//trim(mycall)//'> '//trim(hiscall)//' RRR'
      call pack77(msg,i3,n3,c77,0)
      call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
      if(i3.ne.4 .or. msg.ne.msgchk .or. .not.unpk77_success) go to 12
      read(c77,'(77i1)',err=12) apsymdxnsrrr(1:77)
      apsymdxnsrrr=2*apsymdxnsrrr-1

      msg='<'//trim(mycall)//'> '//trim(hiscall)//' RR73'
      call pack77(msg,i3,n3,c77,0)
      call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
      if(i3.ne.4 .or. msg.ne.msgchk .or. .not.unpk77_success) go to 13
      read(c77,'(77i1)',err=13) apsymdxnsr73(1:77)
      apsymdxnsr73=2*apsymdxnsr73-1

      msg='<'//trim(mycall)//'> '//trim(hiscall)//' 73'
      call pack77(msg,i3,n3,c77,0)
      call unpack77(c77,1,msgchk,unpk77_success,25)
!read(c77(75:77),'(b3)') k3; print *,'i3 =',k3; print *,msgchk
      if(i3.ne.4 .or. msg.ne.msgchk .or. .not.unpk77_success) go to 14
      read(c77,'(77i1)',err=14) apsymdxns732(1:77)
      apsymdxns732=2*apsymdxns732-1
    endif

  endif

  go to 32

1 apcqsym=0; apcqsym(1)=99; apcqsym(30)=99; go to 32
2 apsymdxstd=0; apsymdxstd(1)=99; apsymdxstd(30)=99; go to 32
3 apsymdxnsrr73=0; apsymdxnsrr73(1)=99; apsymdxnsrr73(30)=99; go to 32
4 apsymdxns73=0; apsymdxns73(1)=99; apsymdxns73(30)=99; go to 32
5 apsymmyns2=0; apsymmyns2(1)=99; apsymmyns2(30)=99; go to 32
6 apsymmynsrr73=0; apsymmynsrr73(1)=99; apsymmynsrr73(30)=99; go to 32
7 apsymmyns73=0; apsymmyns73(1)=99; apsymmyns73(30)=99; go to 32
8 apsymmyns1=0; apsymmyns1(1)=99; go to 32
9 apsym=0; apsym(1)=99; apsym(30)=99; go to 32
10 apsymsp=0; apsymsp(1)=99; apsymsp(30)=99; go to 32
11 apsymdxns1=0; apsymdxns1(1)=99; apsymdxns1(30)=99; go to 32
12 apsymdxnsrrr=0; apsymdxnsrrr(1)=99; apsymdxnsrrr(30)=99; go to 32
13 apsymdxnsr73=0; apsymdxnsr73(1)=99; apsymdxnsr73(30)=99; go to 32
14 apsymdxns732=0; apsymdxns732(1)=99; apsymdxns732(30)=99; go to 32
15 apsymmynsrrr=0; apsymmynsrrr(1)=99; apsymmynsrrr(30)=99; go to 32

32 continue

  return
end subroutine ft8apset
