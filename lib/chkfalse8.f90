subroutine chkfalse8(msg37,i3,n3,nbadcrc,iaptype,lcall1hash)

  use ft8_mod1, only : mycall,hiscall,hisgrid4
  character msg37*37,decoded*22,callsign*12,calltmp*12,call_a*12,call_b*12,grid*12,callmask6*6
  character*6 mask6(3)
  integer, intent(in) :: i3,n3
  logical(1) falsedec,lchkcall,lgvalid,lwrongcall
  logical(1), intent(in) :: lcall1hash
  data mask6/'001000','101000','011000'/

  call_a=''; call_b=''
! iaptype=0 non-AP decoder
! iaptype=1 CQ message AP decoder

! i3 n3                                      Bits               Total  Message type
! ---------------------------------------------------------------------------------------
! 0   0 FREE TEXT MSG                        71                   71
! 0   1 K1ABC RR73; W9XYZ <KH1/KH7Z> -12     28 28 10 5           71   DXpedition Mode
! 0   2 PA3XYZ/P R 590003 IO91NP             28 1 1 3 12 25       70   EU VHF contest (2)
! 0   2 PA3XYZ 520093 IO91NP                 28 1 1 3 12 25       70   EU VHF contest (2)
! 0   3 WA9XYZ KA1ABC R 16A EMA              28 28 1 4 3 7        71   ARRL Field Day
! 0   3 WA9XYZ KA1ABC 7D EMA                 28 28 1 4 3 7        71   ARRL Field Day
! 0   3 WA9XYZ G8ABC 1D DX                   28 28 1 4 3 7        71   ARRL Field Day
! 0   4 WA9XYZ KA1ABC R 32A EMA              28 28 1 4 3 7        71   ARRL Field Day
! 0   5 123456789ABCDEF012                   71                   71   Telemetry (18 hex)
! 0   5 7123456789ABCDEF01                   71                   71   Telemetry (18 hex)
! 0   5 71234567                             71                   71   Telemetry (18 hex)
! 0   5 81234567                             71                   71   Telemetry (18 hex)
! 0   5 8123456789ABCDEF01                   71                   71   Telemetry (18 hex)
! 1     WA9XYZ/R KA1ABC/R R FN42             28 1 28 1 1 15       74   Standard msg, with std or non std callsign
! 1     WA9XYZ KA1ABC R-11                   28 1 28 1 1 15       74   Standard msg, with std or non std callsign
! 1     CQ 039 P30KJE/R DR46
! 2     PA1XYZ/P GM4ABC/P R FN42             28 1 28 1 1 15       74   EU VHF Contest
! 2     CQ 039 P30KJE/P DR46
! 3     TU; W9XYZ K1ABC R 579 MA             1 28 28 1 3 13       74   ARRL RTTY contest
! 3     TU; W9XYZ G8ABC R 559 0013           1 28 28 1 3 13       74   ARRL RTTY (DX)
! 4     <WA9XYZ> PJ4/KA1ABC RR73             13 58 1 2            74   Nonstandard call
! 4     <WA9XYZ> PJ4/KA1ABC                  13 58 1 2            74   Nonstandard call
! 4     PJ4/KA1ABC <WA9XYZ> RRR              13 58 1 2            74   Nonstandard call
! 4     CQ ZW5STAYHOME                       13 58 1 2            74   Nonstandard call

!print *,iaptype
!print *,i3,n3,msg37

! 'CQ 5NGX46MFKUE' 'CQ 53HDFKJEASD' 'CQ HDF53KJEASD'  i3=4 n3=1 
! -23  0.3 1482 ~ CQ EQ3YCI/KVNA  i3=4 n3=1 
! -23 -1.3 CQ P28Z58W/77M  i3=4 n3=1
  if(msg37(1:3).eq.'CQ ') then
    callsign=''; ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    if(i3.eq.4) then
      islash=index(msg37,'/')
      if(islash.le.0) then
        callsign=trim(msg37(ispc1+1:)); nlencall=len_trim(callsign); if(nlencall.lt.1) return; ispccall=index(callsign,' ')
! CQ 9D6T0GA4 Z space inside callsign of any length: protocol violation, or last char is digit
        if(ispccall.lt.nlencall .or. callsign(nlencall:nlencall).lt.':') then; nbadcrc=1; msg37=''; return; endif
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
        nlen=len_trim(callsign)
        if(nlen.ge.10) then
          falsedec=.false.
          call chklong8(callsign,falsedec)
          if(falsedec) then; nbadcrc=1; msg37=''; return; endif
        else if(nlen.eq.6 .and. index(callsign,'/').lt.1) then ! 'CQ VK6ZRW'  protocol violation, i3 for stdcall shall be equal to 1
          do i=1,6
            if(callsign(i:i).gt.'/' .and. callsign(i:i).lt.':') then; callmask6(i:i)='1'; else; callmask6(i:i)='0'; endif
          enddo
          falsedec=.false.
          do i=1,3
            if(callmask6.eq.mask6(i)) then; falsedec=.true.; exit; endif ! standard callsign, false decode
          enddo
          if(falsedec) then; nbadcrc=1; msg37=''; return; endif
        endif
      endif

! i=1 AP mask iaptype 1 false decodes
! -23  0.9 2630 ~ CQ P5RXL MK92 *
! -24  0.3  550 ~ CQ DZ0BIL/R AD94 *
!  -7  0.2  400 ~ CQ LI9NAI/R CF97 * in some noisy setups such decodes possible with a high SNR
    else if(iaptype.eq.1) then
      ispc3=index(msg37((ispc2+1):),' ')+ispc2
      if(msg37(ispc2-2:ispc2-1).eq.'/R' .or. msg37(ispc2-2:ispc2-1).eq.'/P') then; callsign=msg37(ispc1+1:ispc2-3)
      else; callsign=msg37(ispc1+1:ispc2-1)
      endif
      include 'callsign_q.f90'
      grid=''
      if(ispc3-ispc2.eq.5 .and. msg37(ispc2+1:ispc2+1).gt.'@' .and. msg37(ispc2+1:ispc2+1).lt.'S' .and. &
         msg37(ispc2+2:ispc2+2).gt.'@' .and. msg37(ispc2+2:ispc2+2).lt.'S' .and. &
         msg37(ispc2+3:ispc2+3).lt.':' .and. msg37(ispc2+4:ispc2+4).lt.':') grid=msg37(ispc2+1:ispc3-1)
      if(len_trim(grid).eq.4) then
        call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
        if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
      endif
    endif

    if(i3.eq.4) then
! CQ Y09Q7/SS60M  i3=4 n3=1
      callsign=''; callsign=trim(msg37(4:)); islash=index(callsign,'/'); islash2=index(callsign(islash+1:),'/')
      nlencall=len_trim(callsign)
      if(nlencall.eq.11 .and. islash.gt.0 .and. islash2.lt.1) then
        if(islash.lt.7) then
          if((callsign(islash+1:islash+1).lt.':' .and. callsign(islash+2:islash+2).lt.':') .or. &
             callsign(islash+1:islash+1).eq.'Q' .or. callsign(nlencall:nlencall).lt.':') then
            nbadcrc=1; msg37=''; return
          endif
        endif
        if(islash.gt.5) then
          if(callsign(islash-1:islash-1).lt.':' .or. (callsign(1:1).lt.':' .and. callsign(2:2).lt.':') .or. &
             callsign(1:1).eq.'Q') then
            nbadcrc=1; msg37=''; return
          endif
        endif
        if(islash.gt.4 .and. islash.lt.8) then
          call_a=''; call_b=''; call_a=callsign(1:islash-1); call_b=callsign(islash+1:)
          falsedec=.false.
          call chkflscall(call_a,call_b,falsedec)
          if(falsedec) then; nbadcrc=1; msg37=''; return; endif
        endif
      endif
    endif

! CQ 039 P30KJE/R DR46 i3=1 n3=5
! CQ 039 P30KJE/P DR46 i3=2 n3=5
    if(msg37(4:4).lt.':' .and. msg37(4:4).gt.'/' .and. msg37(7:7).eq.' ' .and. (i3.eq.1 .or. i3.eq.2)) then
      islash=index(msg37,'/')
      if(islash.gt.10 .and. islash.lt.15) then
        callsign=''; grid=''; callsign=msg37(8:islash-1); grid=msg37(islash+3:islash+6); nlengrid=len_trim(grid)
        if(nlengrid.eq.4 .and. grid(1:1).gt.'@' .and. grid(4:4).lt.':') then
          call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
          if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
        endif
        falsedec=.false.
        call chkflscall('MYCALL      ',callsign,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif
    endif

! CQ CJ3OPE/R FP61 i3=1
! CQ KU3XIK NN41
    if(i3.eq.1 .and. iaptype.eq.0) then
      callsign=''; grid=''
      islash=index(msg37,'/R ')
      if(islash.gt.6) then
        callsign=msg37(4:islash-1); grid=msg37(islash+3:islash+6)
      else if(islash.lt.1) then
        ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1; ispc3=index(msg37((ispc2+1):),' ')+ispc2
        if(len_trim(msg37).eq.ispc3-1) then
          callsign=msg37(ispc1+1:ispc2-1); grid=msg37(ispc2+1:ispc3-1)
        else
          return ! 4-word message, will be processed in ft8b.f90
        endif
      endif
      nlengrid=len_trim(grid)
      if(nlengrid.eq.4 .and. grid(1:1).gt.'@' .and. grid(4:4).lt.':') then
        call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
        if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
      endif
      if(islash.gt.6) then
        falsedec=.false.
        call chkflscall('MYCALL      ',callsign,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif
    endif

    return ! prevent further checking if message starts from 'CQ '
! TO DO via patterns: '175715 -18 -0.1 2519 ~ CQ P2PSR6UWBHS'
  endif

  if(iaptype.eq.2) then ! message starts with user's callsign, 2nd callsign is random
! ES1JA JA1QZQ/R R GD47
! UA3ALE A70DDN/R OF23
! ES6DO NH4CXV MA87
! EA1AHY PW1BSL R GR47 i3=1
! ES6DO ST0VQA R174 invalid message, can not be transmitted
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    callsign=''; grid=''
    callsign=msg37(ispc1+1:ispc2-1)
    islash=index(callsign,'/R')
    if(islash.gt.3) then
      calltmp='            '; calltmp=callsign(1:islash-1); callsign=calltmp
    endif
    include 'callsign_q.f90'
    ispc3=index(msg37((ispc2+1):),' ')+ispc2
    indxr=index(msg37,' R ')
    if(indxr.gt.7) then
      ispc4=index(msg37((ispc3+1):),' ')+ispc3
      if(ispc4-ispc3.eq.5) then
        if(msg37(ispc3+1:ispc3+1).gt.'@' .and. msg37(ispc3+1:ispc3+1).lt.'S' .and. &
           msg37(ispc3+2:ispc3+2).gt.'@' .and. msg37(ispc3+2:ispc3+2).lt.'S' .and. &
           msg37(ispc3+3:ispc3+3).lt.':' .and. msg37(ispc3+4:ispc3+4).lt.':') then
          grid=msg37(ispc3+1:ispc4-1)
        else ! invalid message
          nbadcrc=1; msg37=''; return
        endif
      endif
    else if(indxr.lt.1) then
      if(ispc3-ispc2.eq.5) then
        if(msg37(ispc2+1:ispc2+1).gt.'@' .and. msg37(ispc2+1:ispc2+1).lt.'S' .and. &
           msg37(ispc2+2:ispc2+2).gt.'@' .and. msg37(ispc2+2:ispc2+2).lt.'S' .and. &
           msg37(ispc2+3:ispc2+3).lt.':' .and. msg37(ispc2+4:ispc2+4).lt.':') then
          grid=msg37(ispc2+1:ispc3-1)
        else ! invalid message
          nbadcrc=1; msg37=''; return
        endif
      endif
    endif
    if(len_trim(grid).eq.4) then
      call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
      if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
    endif
!      call chkflscall('MYCALL      ',callsign,falsedec) ! too high risk to eliminate correct decode if callsign missed in ALLCALL7
!      if(falsedec) then; nbadcrc=1; msg37=''; return; endif
    return ! no need in other checks
  endif

  if(iaptype.eq.40) then ! AP mask '<mycall> ??? ???'
! JH4PUL/3 BG4NSA RQ43 * reported false decodes
! JH4PUL/3 M06FLK R AE10 * at least it is possible to transmit the message
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    callsign=''; grid=''
    callsign=msg37(ispc1+1:ispc2-1)
    include 'callsign_q.f90'
    ispc3=index(msg37((ispc2+1):),' ')+ispc2
    indxr=index(msg37,' R ')
    if(indxr.gt.7) then
      ispc4=index(msg37((ispc3+1):),' ')+ispc3
      if(ispc4-ispc3.eq.5 .and. msg37(ispc3+1:ispc3+1).gt.'@' .and. msg37(ispc3+1:ispc3+1).lt.'S' .and. &
         msg37(ispc3+2:ispc3+2).gt.'@' .and. msg37(ispc3+2:ispc3+2).lt.'S' .and. &
         msg37(ispc3+3:ispc3+3).lt.':' .and. msg37(ispc3+4:ispc3+4).lt.':') grid=msg37(ispc3+1:ispc4-1)
    else if(indxr.lt.1) then
      if(ispc3-ispc2.eq.5 .and. msg37(ispc2+1:ispc2+1).gt.'@' .and. msg37(ispc2+1:ispc2+1).lt.'S' .and. &
         msg37(ispc2+2:ispc2+2).gt.'@' .and. msg37(ispc2+2:ispc2+2).lt.'S' .and. &
         msg37(ispc2+3:ispc2+3).lt.':' .and. msg37(ispc2+4:ispc2+4).lt.':') grid=msg37(ispc2+1:ispc3-1)
    endif
    if(len_trim(grid).eq.4) then
      call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
      if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
    endif
    return ! no need in other checks for iaptype 40
  endif

! RV6ARZ CX7CO R NI25 ! mycall hiscall ??? AP mask false decode, both callsigns are correct, checking for valid grid
! ES6DO TK60CNES R MJ79 ! non standard DXCall, iaptype 11
! UA0ZZZ AA1AAA/1 R GC45 ! non standard DXCall, iaptype 11
! JH4PUL/3 HB9CEX R PN76 * ! non standard mycall, iaptype 41
! JH4PUL/3 JT1DN HB30 * ! non standard mycall, iaptype 41
  if(iaptype.eq.3 .or. iaptype.eq.11 .or. iaptype.eq.21 .or. iaptype.eq.41) then
    indxr=index(msg37,' R ')
    if(iaptype.eq.21 .and. indxr.gt.0) then; nbadcrc=1; msg37=''; return; endif ! not valid message in Hound mode
    islash1=index(msg37,'/'); ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    if(ispc1.gt.3 .and. ispc2.gt.7) then
      if(islash1.lt.ispc1 .or. (islash1.eq.ispc2-2 .and. msg37(ispc2-1:ispc2-1).lt.':' )) then ! AA1AAA/1 can be first or 2nd callsign
        if(msg37(1:ispc1-1).eq.mycall .and. msg37(ispc1+1:ispc2-1).eq.hiscall) then
          ispc3=index(msg37((ispc2+1):),' ')+ispc2; grid=''
          if(indxr.gt.7) then
            ispc4=index(msg37((ispc3+1):),' ')+ispc3
            if(ispc4-ispc3.eq.5 .and. msg37(ispc3+1:ispc3+1).gt.'@' .and. msg37(ispc3+1:ispc3+1).lt.'S' .and. &
               msg37(ispc3+2:ispc3+2).gt.'@' .and. msg37(ispc3+2:ispc3+2).lt.'S' .and. &
               msg37(ispc3+3:ispc3+3).lt.':' .and. msg37(ispc3+4:ispc3+4).lt.':') grid=msg37(ispc3+1:ispc4-1)
          else if(indxr.lt.1) then
            if(ispc3-ispc2.eq.5 .and. msg37(ispc2+1:ispc2+1).gt.'@' .and. msg37(ispc2+1:ispc2+1).lt.'S' .and. &
               msg37(ispc2+2:ispc2+2).gt.'@' .and. msg37(ispc2+2:ispc2+2).lt.'S' .and. &
               msg37(ispc2+3:ispc2+3).lt.':' .and. msg37(ispc2+4:ispc2+4).lt.':') grid=msg37(ispc2+1:ispc3-1)
          endif
          if(len_trim(grid).eq.4 .and. grid.ne.hisgrid4) then ! correctly decoded contest message
            if(iaptype.eq.21) then
              if(grid.eq.'RR73') then
                return ! AP mask 'mybcall hisbcall ???' can trigger RR73 message
              else
                nbadcrc=1; msg37=''; return ! not valid message in Hound mode
              endif
            endif
            call chkgrid(hiscall,grid,lchkcall,lgvalid,lwrongcall)
            if(.not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
          endif
        endif
      endif
    endif
    return ! no need in other checks
  endif

  if((i3.ge.1 .and. i3.le.3) .and. (index(msg37,' R ').gt.0 .or. index(msg37,'/R ').gt.0 .or. index(msg37,'/P ').gt.0)) then
! i3=1: 'VF5RMP/P OW3ATG/P R MF' '378YYA/R 9H5PQU R OL53'
! i3=2: 'YT4MEY/P WL8QEW R IR03'
! i3=1: 'ER2XXD 8L7UWC R 529 26'			   
! i3=3 n3=1: '1M8JSH TI0CZQ R 539 42'
! i3=2 n3=5: 'DQ2VF/P J2OON KG37'
! 'N40RTH AD2WPI/R MG62'
!print *,msg37
    islash1=index(msg37,'/')
    call_a=''; call_b=''
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    if(islash1.lt.1 .and. ispc1.gt.3 .and. ispc2.gt.7) then
      call_a=msg37(1:ispc1-1); call_b=msg37(ispc1+1:ispc2-1)
      if(call_b.ne.hiscall) then
        include 'call_q.f90'
        falsedec=.false.
        call chkflscall(call_a,call_b,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif
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
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif
    endif
  endif

! i3=1 n3=4
! 000189 -23  0.4  300 ~ <...> QG6GFE CQ14
! 110015 -21 -0.0 2050 ~ <...> NF7OEX CP03
! check if grid is correct for call_2
  if(lcall1hash .and. i3.eq.1) then ! hash can be associated with a valid callsign in the table
    ispc1=index(msg37,' ')
    if(msg37(1:ispc1-1).ne.mycall) then
      ispc2=index(msg37((ispc1+1):),' ')+ispc1
      callsign=''; grid=''
      callsign=msg37(ispc1+1:ispc2-1)
      islash=index(callsign,'/')
      if(islash.lt.1) then
        include 'callsign_q.f90'
        if(callsign.ne.hiscall) then
          ispc3=index(msg37((ispc2+1):),' ')+ispc2
          if(ispc3-ispc2.eq.5 .and. msg37(ispc2+1:ispc2+1).gt.'@' .and. msg37(ispc2+1:ispc2+1).lt.'S' .and. &
             msg37(ispc2+2:ispc2+2).gt.'@' .and. msg37(ispc2+2:ispc2+2).lt.'S' .and. &
             msg37(ispc2+3:ispc2+3).lt.':' .and. msg37(ispc2+4:ispc2+4).lt.':') then
            grid=msg37(ispc2+1:ispc3-1)
            call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
            if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
          endif
        endif
      endif
    endif
  endif

! i3=1 n3=1 '4P6NPC 5M5UJG BK49'
! i3=3 n3=1 '8Z7TLB JQ0OTZ 589 02'
! i3=0 n3=3 'CE3NTP 4C2DWN 5E ONE'
  if(iaptype.ne.2) then
    if(((i3.eq.1 .or. i3.eq.3) .and. index(msg37,' R ').le.0 .and. index(msg37,'/').le.0) .or. (i3.eq.0 .and. n3.eq.3)) then
      if(msg37(1:2).eq.'<.') go to 2
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
      if(ispc1.gt.3 .and. ispc2.gt.7) then
        call_a=msg37(1:ispc1-1); if(call_a.eq.mycall) go to 2 ! exception to incoming calls
        call_b=msg37(ispc1+1:ispc2-1)
        include 'call_q.f90'
        falsedec=.false.
        call chkflscall(call_a,call_b,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif
    endif
  endif

2 continue

  if(i3.eq.4) then
    ibrace1=index(msg37,'<.')
    if(index(msg37,'<.').gt.4) then
      callsign=''; callsign=msg37(1:ibrace1-2); islash=index(callsign,'/'); nlencall=len_trim(callsign)
      if(nlencall.lt.1) go to 4
! i3=4 n3=2 '6397X6JN637 <...> RRR' 
! 'R 5QCDOIY9 <...> RR73'
      if(index(trim(callsign),' ').gt.0 .or. (islash.lt.1 .and. callsign(nlencall:nlencall).lt.':')) then
        nbadcrc=1; msg37=''; return
      endif
      include 'callsign_q.f90'
      falsedec=.false.
      call chklong8(callsign,falsedec)
      if(falsedec) then; nbadcrc=1; msg37=''; return; endif
    endif
    if(msg37(1:2).eq.'<.' .or. index(msg37,'.>').gt.4) then
!'<...> 104J71MFOT9 RR73'
! i3=4 n3=0 '<...> /7ZZZZ/ /8'
! i3=4 n3=2 '<...> 6HGLDZ 5IN2 RRR'
      callsign=''; nlenmsg=len_trim(msg37)
      if(msg37(1:2).eq.'<.') then
        if(msg37(nlenmsg-4:nlenmsg).eq.' RR73') then; callsign=msg37(7:nlenmsg-5)
        else if(msg37(nlenmsg-2:nlenmsg).eq.' 73') then; callsign=msg37(7:nlenmsg-3)
        else if(msg37(nlenmsg-3:nlenmsg).eq.' RRR') then; callsign=msg37(7:nlenmsg-4)
        else; callsign=msg37(7:nlenmsg)
        endif
      else
        ibracket=index(msg37,'<.'); callsign=msg37(1:ibracket-2)
      endif
      nlencall=len_trim(callsign)
      if(nlencall.gt.0) then
        ispace=index(callsign(1:nlencall),' '); islash=index(callsign,'/')
        if(ispace.gt.0 .or. (islash.lt.1 .and. callsign(nlencall:nlencall).lt.':') .or. islash.eq.nlencall) then
          nbadcrc=1; msg37=''; return
        endif
! KY2HTW/0O5M <...>  i3=4 n3=0
        if(islash.gt.4) then
          islash2=index(callsign(islash+1:),'/')
          if(nlencall.eq.11 .and. islash2.lt.1) then
            if(islash.lt.7) then
              if((callsign(islash+1:islash+1).lt.':' .and. callsign(islash+2:islash+2).lt.':') .or. &
                 callsign(islash+1:islash+1).eq.'Q' .or. callsign(nlencall:nlencall).lt.':') then
                nbadcrc=1; msg37=''; return
              endif
            endif
            if(islash.gt.5) then
              if(callsign(islash-1:islash-1).lt.':' .or. (callsign(1:1).lt.':' .and.callsign(2:2).lt.':') .or. &
                 callsign(1:1).eq.'Q') then
                nbadcrc=1; msg37=''; return
              endif
            endif
            if(islash.gt.4 .and. islash.lt.8) then
              call_a=''; call_b=''; call_a=callsign(1:islash-1); call_b=callsign(islash+1:)
              falsedec=.false.
              call chkflscall(call_a,call_b,falsedec)
              if(falsedec) then; nbadcrc=1; msg37=''; return; endif
            endif
          endif
        endif
        include 'callsign_q.f90'
        falsedec=.false.
        call chklong8(callsign,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif
    endif
! '<...> T99LMF GC41' need to understand how to check such message
  endif

4 continue

  if(i3.eq.3 .and. msg37(1:3).eq.'TU;') then 
!i3=3 n3=3 'TU; QV4UPP 632TGU 529'
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1 
    ispc3=index(msg37((ispc2+1):),' ')+ispc2
    if(ispc2.gt.7 .and. ispc3.gt.11) then
      call_a=msg37(ispc1+1:ispc2-1); call_b=msg37(ispc2+1:ispc3-1)
      include 'call_q.f90'
      falsedec=.false.
      call chkflscall(call_a,call_b,falsedec)
      if(falsedec) then; nbadcrc=1; msg37=''; return; endif
    endif
  endif

  if(i3.eq.0 .and. n3.eq.0) then
! 'J/.ZM1R-D1Q89'
    if(index(msg37,'/.').gt.0) then; nbadcrc=1; msg37=''; return; endif
    decoded=msg37(1:22)
    falsedec=.false.
    call filtersfree(decoded,falsedec)
    if(falsedec) then; nbadcrc=1; msg37=''; return; endif
  endif

! CQ_RLXA 551626 ^J65XI 
  if(i3.eq.0 .and. (msg37(1:3).eq.'CQ_' .or. index(msg37,'^').gt.0)) then
    nbadcrc=1; msg37=''; return
  endif

  return
end subroutine chkfalse8
