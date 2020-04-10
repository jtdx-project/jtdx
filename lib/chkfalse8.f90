subroutine chkfalse8(msg37,i3,n3,nbadcrc,iaptype)

  use ft8_mod1, only : mycall,hiscall
  character msg37*37,decoded*22,callsign*12,calltmp*12,call_a*12,call_b*12,grid*12
  integer, intent(in) :: i3,n3
  logical(1) falsedec,lchkcall,lgvalid,lwrongcall

  call_a='            '; call_b='            '
! iaptype=0 non-AP decoder
! iaptype=1 CQ message AP decoder

! i3=0...4 n3=0...5
! i3=0 n3=0  free text message
! i3=0 n3=1  special message DXpedition
! i3=1 std message with std callsigns, or both callsigns /P || /R
! i3=2 one of two callsigns in the message is compound (/p)
! i3=3 (n3=1?) contest message with RST and serial number 
! i3=4 nonstandard callsign in the message

!print *,iaptype
!print *,i3,n3,msg37

! 'CQ 5NGX46MFKUE' 'CQ 53HDFKJEASD' 'CQ HDF53KJEASD'  i3=4 n3=1 
! -23  0.3 1482 ~ CQ EQ3YCI/KVNA  i3=4 n3=1 
! -23 -1.3 CQ P28Z58W/77M  i3=4 n3=1
  if(msg37(1:3).eq.'CQ ') then
    callsign='            '; ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    if(i3.eq.4 .and. n3.eq.1) then
      islash=index(msg37,'/')
      if(ispc2.gt.7 .and. islash.le.0) then ! 'CQ nnn XX1XX' message shall not be checked
        if(islash.le.0) then
          callsign=msg37(ispc1+1:ispc2-1)
        else
          if(islash.gt.ispc1 .and. islash.lt.ispc2) then
            if(islash-ispc1.le.ispc2-islash) then; callsign=msg37(islash+1:ispc2-1)
            else; callsign=msg37(ispc1+1:islash-1)
            endif
          endif
        endif
        include 'callsign_q.f90'
        if(islash.gt.0) then
          falsedec=.false.; call chkflscall('CQ          ',callsign,falsedec)
          if(falsedec) then; nbadcrc=1; msg37=''; return; endif
        else
          if(len_trim(callsign).ge.10) then
            falsedec=.false.
            call chklong8(callsign,falsedec)
            if(falsedec) then; nbadcrc=1; msg37='                                     '; return; endif
          endif
        endif
      endif
! 'CQ 3Q2VFI RH49          *' filter is based on the Grid
! i=1 n=7 'CQ 2A1GKO/R GC63        *'
! -24 -1.8 2389 ~ CQ N9OAT/R RP25         *
! -24  0.3  550 ~ CQ DZ0BIL/R AD94          *
    else if(iaptype.eq.1) then
      ispc3=index(msg37((ispc2+1):),' ')+ispc2
      if(msg37(ispc2-3:ispc2-1).eq.'/R' .or. msg37(ispc2-3:ispc2-1).eq.'/P') then; callsign=msg37(ispc1+1:ispc2-3)
      else; callsign=msg37(ispc1+1:ispc2-1)
      endif
      grid=msg37(ispc2+1:ispc3-1)
      include 'callsign_q.f90'
      call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
      if(lwrongcall) then; nbadcrc=1; msg37='                                     '; return; endif
      if(lchkcall .or. .not.lgvalid) then
        falsedec=.false.
        call chkflscall('CQ          ',callsign,falsedec)
        if(falsedec) then; nbadcrc=1; msg37='                                     '; return; endif
      endif
    endif

    return ! prevent further checking if message starts from 'CQ '
! TO DO via patterns: '175715 -18 -0.1 2519 ~ CQ P2PSR6UWBHS'
  endif

  if((i3.ge.1 .and. i3.le.3) .and. (index(msg37,' R ').gt.0 .or. index(msg37,'/R ').gt.0 .or. index(msg37,'/P ').gt.0)) then
! i3=1: 'VF5RMP/P OW3ATG/P R MF' 'ES1JA JA1QZQ/R R GD47' '378YYA/R 9H5PQU R OL53'
! i3=2: 'YT4MEY/P WL8QEW R IR03'
! i3=1: 'ER2XXD 8L7UWC R 529 26'			   
! i3=3 n3=1: '1M8JSH TI0CZQ R 539 42'
! i3=2 n3=5: 'DQ2VF/P J2OON KG37'
! 'N40RTH AD2WPI/R MG62'
!print *,msg37
    islash1=index(msg37,'/')
    call_a='            '; call_b='            '
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    if(islash1.le.0 .and. ispc1.gt.3 .and. ispc2.gt.7) then
      call_a=msg37(1:ispc1-1); call_b=msg37(ispc1+1:ispc2-1)
      include 'call_q.f90'
      falsedec=.false.
      call chkflscall(call_a,call_b,falsedec)
      if(falsedec) then; nbadcrc=1; &
      msg37='                                     '; return; endif
    endif

    if(islash1.gt.0 .and. ispc1.gt.3 .and. ispc2.gt.7) then
      islash2=index(msg37((islash1+1):),'/')+islash1
      if(islash1.gt.ispc1) then
        call_a=msg37(1:ispc1-1); call_b=msg37(ispc1+1:islash1-1)
      else
        call_a=msg37(1:islash1-1)
        if(islash2.gt.islash1) then
          call_b=msg37(ispc1+1:islash2-1)
        else
          call_b=msg37(ispc1+1:ispc2-1)
        endif
      endif
      include 'call_q.f90'
      if(call_a.ne.mycall .and. call_b.ne.hiscall) then
        falsedec=.false.
        call chkflscall(call_a,call_b,falsedec)
        if(falsedec) then; nbadcrc=1; &
          msg37='                                     '; return; endif
      endif
    endif
  endif

  if((i3.eq.1 .or. i3.eq.3) .and. index(msg37,' R ').le.0 .and. index(msg37,'/').le.0) then
! i3=1 n3=1 '4P6NPC 5M5UJG BK49'
! i3=3 n3=1 '8Z7TLB JQ0OTZ 589 02' 
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    if(ispc1.gt.3 .and. ispc2.gt.7) then
      call_a=msg37(1:ispc1-1); call_b=msg37(ispc1+1:ispc2-1)
      include 'call_q.f90'
      falsedec=.false.
      call chkflscall(call_a,call_b,falsedec)
      if(falsedec) then; nbadcrc=1; &
        msg37='                                     '; return; endif
    endif
  endif

  if(i3.eq.4) then
    ibrace1=index(msg37,'<.')
    if(index(msg37,'<.').gt.4) then
! i3=4 n3=2 '6397X6JN637 <...> RRR' 
      callsign='            '
      callsign=msg37(1:ibrace1-2)
! 'R 5QCDOIY9 <...> RR73'
      if(index(trim(callsign),' ').gt.0) then; nbadcrc=1; &
        msg37='                                     '; return; endif
      include 'callsign_q.f90'
      falsedec=.false.
      call chklong8(callsign,falsedec)
      if(falsedec) then; nbadcrc=1; &
        msg37='                                     '; return; endif
    endif
    if(index(msg37,'<.').eq.1) then
!'<...> 104J71MFOT9 RR73'
! i3=4 n3=0 '<...> /7ZZZZ/ /8'
! i3=4 n3=2 '<...> 6HGLDZ 5IN2 RRR'
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
      callsign='            '
      callsign=msg37(ispc1+1:ispc2-1)
      include 'callsign_q.f90'
      falsedec=.false.
      call chklong8(callsign,falsedec)
      if(falsedec) then; nbadcrc=1; &
        msg37='                                     '; return; endif
    endif
! '<...> T99LMF GC41' need to understand how to check such message
  endif

  if(i3.eq.3 .and. msg37(1:3).eq.'TU;') then 
!i3=3 n3=3 'TU; QV4UPP 632TGU 529'
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1 
    ispc3=index(msg37((ispc2+1):),' ')+ispc2
    if(ispc2.gt.7 .and. ispc3.gt.11) then
      call_a=msg37(ispc1+1:ispc2-1); call_b=msg37(ispc2+1:ispc3-1)
      include 'call_q.f90'
      falsedec=.false.
      call chkflscall(call_a,call_b,falsedec)
      if(falsedec) then; nbadcrc=1; &
        msg37='                                     '; return; endif
    endif
  endif

  if(i3.eq.0 .and. n3.eq.0) then
! 'J/.ZM1R-D1Q89'
    if(index(msg37,'/.').gt.0) then; nbadcrc=1; &
      msg37='                                     '; return; endif
    decoded=msg37(1:22)
    falsedec=.false.
    call filtersfree(decoded,falsedec)
    if(falsedec) then; nbadcrc=1; &
      msg37='                                     '; return; endif
  endif

! CQ_RLXA 551626 ^J65XI 
  if(i3.eq.0 .and. (msg37(1:3).eq.'CQ_' .or. index(msg37,'^').gt.0)) then
    nbadcrc=1; msg37='                                     '; return
  endif

  if(iaptype.eq.2) then ! message starts with user's callsign
! 'UA3ALE A70DDN/R OF23'
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    callsign='            '
    callsign=msg37(ispc1+1:ispc2-1)
    islash=index(callsign,'/')
    if(islash.gt.0) then
      calltmp='            '; calltmp=callsign(1:islash-1); callsign=calltmp
    endif
    include 'callsign_q.f90'
    if(callsign.ne.hiscall) then
      falsedec=.false.
      call chkflscall('MYCALL      ',callsign,falsedec)
      if(falsedec) then; nbadcrc=1; &
        msg37='                                     '; return; endif
    endif
  endif

! old FT8v1 filters
!------------------

!! 075815 -23 2.0 2693 ~ SDGCGLYHHQ/ES
!! 041430 -23 -1.3 1950 ~ K8.CET4PNB0I
!! filter false -23 ./ freemsgs while trying to avoid blocking content messages using multiple criteria
!               if(iFreeText.eq.1 .and. nint(xsnr).eq.-23 .and. (rxdt.lt.-0.5 .or. rxdt.gt.1.9)) then
!                 islash=index(message,'/')
!                 ispace=index(message,' ')
!                 if((ispace.eq.0 .or. ispace.gt.12) .and. (index(message,'.').gt.0 .or. islash.gt.7 .or. &
!                    (islash.gt.5 .and. islash.lt.5))) then
!!print *,"0 -23_dot_slash freemsg"
!!print *,message
!                    nbadcrc=1
!                    message='                      '
!                    return 
!                 endif
!               endif
!               if(iaptype.ge.1 .and. iaptype.le.3) then
!                 if(i3bit.eq.2 .and. iFreeText.eq.0) then
!!print *,"1 i3bit",i3bit
!!print *,message
!                    nbadcrc=1
!                    message='                      '
!                    return 
!                 endif
!               endif
!               if(lapon .and. (iaptype.eq.1 .or. iaptype.eq.2)) then
!                 falsedec=.false.
!                 call chkfalse(message,mycall,falsedec)
!                 if(falsedec) then
!!print *,"2 chkfalse Hint",iaptype
!!print *,message
!                    nbadcrc=1
!                    message='                      '
!                    return 
!                 endif
!               endif
!               if(iaptype.eq.0 .and. message(1:2).ne.'CQ' &
!                 .and. message(1:3).ne.'DE ' .and. message(1:3).ne.'QRZ') then
!                 if(iFreeText.eq.1) then
!                    falsedec=.false.
!                    call filtersfree(message,falsedec)
!                    if(falsedec) then
!!print *,"3 filtersfree not Hint"
!!print *,message
!                       nbadcrc=1
!                       message='                      '
!                       return 
!                    endif
!                 endif
!                 if(iFreeText.eq.0) then
!                    falsedec=.false.
!!not Hint decodes starting with mycall shall not be checked against ALLCALL.TXT
!                    call1=message(1:index(message,' ')-1)
!                    if(len_trim(call1).le.3 .or. &
!                       (len_trim(call1).gt.3 .and. index(mycall,trim(call1)).eq.0)) &
!                       call chkfalse(message,mycall,falsedec)
!                    if(falsedec) then
!!print *,"4 chkfalse not Hint"
!!print *,message
!                       nbadcrc=1
!                       message='                      '
!                       return 
!                    endif
!                 endif
!               endif
!               if(iaptype.eq.0 .and. (message(1:3).eq.'CQ ' &
!                 .or. message(1:3).eq.'DE ' .or. message(1:4).eq.'QRZ ')) then
!                 falsedec=.false.
!                 call filterscq2(message,falsedec,iFreeText)
!                 if(falsedec) then
!!print *,"5 filterscq2 freemsg:",iFreeText
!!print *,message
!                    nbadcrc=1
!                    message='                      '
!                    return 
!                 endif
!               endif
!             endif
!             if(message(1:9).eq."CQ ......" .or. message(7:13).eq." ......") then
!               nbadcrc=1
!               message='                      '
!               return 
!             endif
!! CQ 083TRB PJ46
!             if(message(1:3).eq."CQ " .and. message(4:4).ge.'0' .and. message(4:4).le.'9' &
!              .and. message(5:5).ge.'0' .and. message(5:5).le.'9' &
!              .and. message(6:6).ge.'0' .and. message(6:6).le.'9' &
!              .and. message(7:7).ge.'A' .and. message(7:7).le.'Z' &
!              .and. message(10:10).eq." ") then
!               nbadcrc=1
!               message='                      '
!               return 
!             endif
!! ZP/K2PAL NU1T   *  false decode
!             if(iaptype.ge.2 .and. iaptype.le.6 .and. index(mycall,'/').eq.0) then
!               islpos=index(message,'/')
!               imycallpos=index(message,trim(mycall))
!               if(islpos.ne.0 .and. imycallpos.ne.0 .and. islpos.lt.imycallpos) then
!                 nbadcrc=1
!                 message='                      '
!                 return 
!               endif
!             endif

  return
end subroutine chkfalse8
