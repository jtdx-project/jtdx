subroutine chkfalse8(msg37,i3,n3,nbadcrc,iaptype)

  use ft8_mod1, only : mycall,hiscall
  character msg37*37,decoded*22,callsign*12,calltmp*12,call_a*12,call_b*12,grid*12,callmask6*6
  character*6 mask6(3)
  integer, intent(in) :: i3,n3
  logical(1) falsedec,lchkcall,lgvalid,lwrongcall
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
! 1     WA9XYZ/R KA1ABC/R R FN42             28 1 28 1 1 15       74   Standard msg
! 1     WA9XYZ KA1ABC R-11                   28 1 28 1 1 15       74   Standard msg
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
! 'CQ 3Q2VFI RH49          *' filter is based on the Grid
! i=1 n=7 'CQ 2A1GKO/R GC63        *'
! -24 -1.8 2389 ~ CQ N9OAT/R RP25         *
! -24  0.3  550 ~ CQ DZ0BIL/R AD94          *
    else if(iaptype.eq.1) then
      ispc3=index(msg37((ispc2+1):),' ')+ispc2
      if(msg37(ispc2-2:ispc2-1).eq.'/R' .or. msg37(ispc2-2:ispc2-1).eq.'/P') then; callsign=msg37(ispc1+1:ispc2-3)
      else; callsign=msg37(ispc1+1:ispc2-1)
      endif
      grid=msg37(ispc2+1:ispc3-1)
      include 'callsign_q.f90'
      call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
      if(lwrongcall) then; nbadcrc=1; msg37=''; return; endif
      if(lchkcall .or. .not.lgvalid) then
        falsedec=.false.
        call chkflscall('CQ          ',callsign,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
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

! CQ CJ3OPE/R FP61 i3=1 n3=1
    if(i3.eq.1) then
      islash=index(msg37,'/R ')
      if(islash.eq.9 .or. islash.eq.10) then
        callsign=''; grid=''; callsign=msg37(4:islash-1); grid=msg37(islash+3:islash+6); nlengrid=len_trim(grid)
        if(nlengrid.eq.4 .and. grid(1:1).gt.'@' .and. grid(4:4).lt.':') then
          call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
          if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
        endif
        falsedec=.false.
        call chkflscall('MYCALL      ',callsign,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
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
    call_a=''; call_b=''
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    if(islash1.le.0 .and. ispc1.gt.3 .and. ispc2.gt.7) then
      call_a=msg37(1:ispc1-1); call_b=msg37(ispc1+1:ispc2-1)
      include 'call_q.f90'
      falsedec=.false.
      call chkflscall(call_a,call_b,falsedec)
      if(falsedec) then; nbadcrc=1; msg37=''; return; endif
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

! i3=1 n3=1 '4P6NPC 5M5UJG BK49'
! i3=3 n3=1 '8Z7TLB JQ0OTZ 589 02'
! i3=0 n3=3 'CE3NTP 4C2DWN 5E ONE'
  if(((i3.eq.1 .or. i3.eq.3) .and. index(msg37,' R ').le.0 .and. index(msg37,'/').le.0) .or. (i3.eq.0 .and. n3.eq.3)) then
! some exceptions, may be checked for valid grid
! <...> DJ9KM JO40
! <...> ON6DSL R-05
    if(msg37(1:2).eq.'<.') return
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    if(ispc1.gt.3 .and. ispc2.gt.7) then
      call_a=msg37(1:ispc1-1); call_b=msg37(ispc1+1:ispc2-1)
      include 'call_q.f90'
      falsedec=.false.
      call chkflscall(call_a,call_b,falsedec)
      if(falsedec) then; nbadcrc=1; msg37=''; return; endif
    endif
  endif

  if(i3.eq.4) then
    ibrace1=index(msg37,'<.')
    if(index(msg37,'<.').gt.4) then
      callsign=''; callsign=msg37(1:ibrace1-2); islash=index(callsign,'/'); nlencall=len_trim(callsign)
      if(nlencall.lt.1) return
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

  if(iaptype.eq.2) then ! message starts with user's callsign
! 'UA3ALE A70DDN/R OF23'
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
    callsign=''; grid=''
    callsign=msg37(ispc1+1:ispc2-1)
    islash=index(callsign,'/')
    if(islash.gt.0) then
      calltmp='            '; calltmp=callsign(1:islash-1); callsign=calltmp
    endif
    include 'callsign_q.f90'
    if(callsign.ne.hiscall) then
      falsedec=.false.
      islash=index(msg37,'/R ')
      if(islash.gt.7) then
        ispc3=index(msg37((ispc2+1):),' ')+ispc2
        if(ispc3-ispc2.eq.5 .and. msg37(ispc2+1:ispc2+1).gt.'@' .and. msg37(ispc2+1:ispc2+1).lt.'S' .and. &
           msg37(ispc2+2:ispc2+2).gt.'@' .and. msg37(ispc2+2:ispc2+2).lt.'S' .and. &
           msg37(ispc2+3:ispc2+3).lt.':' .and. msg37(ispc2+4:ispc2+4).lt.':') then
          grid=msg37(ispc2+1:ispc3-1)
          call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
          if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
        endif
      endif
      call chkflscall('MYCALL      ',callsign,falsedec)
      if(falsedec) then; nbadcrc=1; msg37=''; return; endif
    endif
  endif

  return
end subroutine chkfalse8
