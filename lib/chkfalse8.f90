subroutine chkfalse8(msg37,i3,n3,nbadcrc,iaptype)

  use ft8_mod1, only : mycall,hiscall
  character msg37*37,decoded*22,callsign*12,calltmp*12,call_a*12,call_b*12,grid*12
  integer, intent(in) :: i3,n3
  logical(1) falsedec,lchkcall,lgvalid

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

! 'CQ 5NGX46MFKUE' 'CQ 53HDFKJEASD' 'CQ HDF53KJEASD'
  if(msg37(1:3).eq.'CQ ') then

    callsign='            '
    ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1; islashr=index(msg37,'/R ')
    if(ispc2.gt.7 .and. islashr.le.0) then ! 'CQ nnn XX1XX' message shall not be checked
      callsign=msg37(ispc1+1:ispc2-1)
      include 'callsign_q.f90'
      if(len_trim(callsign).ge.10) then
        falsedec=.false.
        call chklong8(callsign,falsedec)
        if(falsedec) then; nbadcrc=1; &
          msg37='                                     '; return; endif
      endif
    endif

! i=1 n=7 'CQ 2A1GKO/R GC63        *'
    if(iaptype.eq.1 .and. ispc2.gt.7 .and. islashr.gt.0) then
      callsign=msg37(ispc1+1:islashr-1)
      include 'callsign_q.f90'
      falsedec=.false.
      call chkflscall('CQ          ',callsign,falsedec)
      if(falsedec) then; nbadcrc=1; &
        msg37='                                     '; return; endif
    endif
! 'CQ 3Q2VFI RH49          *' filter is based on the Grid
    islash=index(msg37,'/')
    if(iaptype.eq.1 .and. ispc2.gt.7 .and. islash.le.0) then
      ispc3=index(msg37((ispc2+1):),' ')+ispc2
      if(ispc3.eq.ispc2+5) then
        callsign=msg37(ispc1+1:ispc2-1)
        grid=msg37(ispc2+1:ispc3-1)
        include 'callsign_q.f90'
lchkcall=.false.
if(grid(1:1).gt.'@' .and. grid(1:1).lt.'J') then
  if((grid(1:1).eq.'A' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'O') .or. grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'B' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'K') .or. grid(2:2).eq.'N' .or. grid(2:2).eq.'M' .or. &
       grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'C' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'M') .or. grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'D' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'K') .or. grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'E' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'I') .or. grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'F' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'D') .or. grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'G' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'F') .or. (grid(2:2).gt.'J' .and. grid(2:2).lt.'N') .or. &
       grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'H' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'H') .or. (grid(2:2).gt.'I' .and. grid(2:2).lt.'P') .or. & 
       grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'I' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'J') .or. grid(2:2).eq.'Q' .or. grid(2:2).eq.'R'))) &
    lchkcall=.true.
else  if(grid(1:1).gt.'I' .and. grid(1:1).lt.'S') then
  if((grid(1:1).eq.'J' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'F'))) .or. &
     (grid(1:1).eq.'K' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'F'))) .or. &
     (grid(1:1).eq.'L' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'G') .or. grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'M' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'J') .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'N' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'I') .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'O' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'F') .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'P' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'F') .or. grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'Q' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'E') .or. grid(2:2).eq.'Q' .or. grid(2:2).eq.'R')) .or. &
     (grid(1:1).eq.'R' .and. ((grid(2:2).gt.'@' .and. grid(2:2).lt.'E') .or. (grid(2:2).gt.'H' .and. grid(2:2).lt.'O') .or. &
       grid(2:2).eq.'Q' .or. grid(2:2).eq.'R'))) lchkcall=.true.
else if (grid(1:1).gt.'/' .and. grid(1:1).lt.':') then ! 0..9
  lchkcall=.true.
endif

lgvalid=.false.
if(.not.lchkcall) then
! USA
  if((callsign(1:1).eq.'A' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'M') .or. callsign(1:1).eq.'K' .or. &
     callsign(1:1).eq.'N' .or. callsign(1:1).eq.'W') then
    if((grid(1:1).gt.'B' .and. grid(1:1).lt.'G' .and. (grid(1:1).eq.'M' .or. grid(1:1).eq.'N')) .or. grid(1:2).eq.'DL' .or. &
       grid(1:2).eq.'EL' .or. grid(1:2).eq.'BP' .or. grid(1:2).eq.'AP' .or. grid(1:2).eq.'BL' .or. grid(1:2).eq.'BK') &
      lgvalid=.true.

! Japan
  else if(callsign(1:2).eq.'JA' .or. (callsign(1:1).eq.'J' .and. callsign(2:2).gt.'D' .and. callsign(2:2).lt.'T') .or. &
         (callsign(1:1).eq.'7' .and. callsign(2:2).gt.'I' .and. callsign(2:2).lt.'O')) then
    if((grid(1:2).eq.'PM' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.':') .or. &
       (grid(1:3).eq.'QM0' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'QN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'6') .or. &
       grid(1:4).eq.'PN90' .or. grid(1:4).eq.'PN91' .or. grid(1:4).eq.'PN92' .or. grid(1:4).eq.'QM19' .or. &
       (grid(1:2).eq.'PL' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.'6')) lgvalid=.true.

! Fed. Rep. of Germany
  else if((callsign(1:1).eq.'D' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'S') .or. &
          (callsign(1:1).eq.'Y' .and. callsign(2:2).gt.'1' .and. callsign(2:2).lt.':')) then
    if((grid(1:2).eq.'JO' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'5') .or. &
       (grid(1:2).eq.'JN' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'6' .and. grid(4:4).lt.':')) &
      lgvalid=.true.

! UK
  else if(callsign(1:1).eq.'G' .or. callsign(1:1).eq.'M' .or. callsign(1:1).eq.'2') then
    if((grid(1:2).eq.'IO' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':') .or. &
       (grid(1:3).eq.'JO0' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4') .or. &
       grid(1:4).eq.'IP90' .or. grid(1:4).eq.'IN69' .or. grid(1:4).eq.'IN89') lgvalid=.true.

! Russia
  else if(callsign(1:1).eq.'R' .or. (callsign(1:1).eq.'U' .and. ((callsign(2:2).gt.'@' .and. callsign(2:2).lt.'J') .or. &
          (callsign(2:2).gt.'/' .and. callsign(2:2).lt.':')))) then
    if(grid(1:2).eq.'KO' .or. grid(1:2).eq.'LO' .or. &
       (grid(1:2).eq.'LN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'5') .or. grid(1:2).eq.'MO' .or. grid(1:2).eq.'NO' .or. &
       (grid(1:2).eq.'KN' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'KP' .and. ((grid(3:3).gt.'3' .and. grid(3:3).lt.':') .or. grid(1:4).eq.'KP30')) .or. grid(1:2).eq.'NP' &
       .or. grid(1:2).eq.'OO' .or. grid(1:2).eq.'PO' .or. grid(1:2).eq.'LP' .or. grid(1:2).eq.'MP' .or. &
       grid(1:3).eq.'QO0' .or. grid(1:3).eq.'QO8' .or. grid(1:3).eq.'QO9' .or. grid(1:3).eq.'QN0' .or. grid(1:3).eq.'QN1' &
       .or. grid(1:4).eq.'PP41' .or. grid(1:4).eq.'PP42' .or. grid(1:4).eq.'QO49' .or. grid(1:4).eq.'QO59' .or. &
       grid(1:4).eq.'RP59' .or. grid(1:4).eq.'RP84') lgvalid=.true.

! France
  else if(callsign(1:1).eq.'F' .and. callsign(2:2).gt.'/' .and. callsign(2:2).lt.':') then
    if((grid(1:2).eq.'IN' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'JN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'5') .or. &
       grid(1:4).eq.'JO00' .or. grid(1:4).eq.'JO10' .or. grid(1:4).eq.'JO20' .or. grid(1:4).eq.'JO11') lgvalid=.true.

! Netherlands
  else if(callsign(1:1).eq.'P' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'J') then
    if(((grid(1:3).eq.'JO2' .or. grid(1:3).eq.'JO3') .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4') .or. &
       grid(1:4).eq.'JO11') lgvalid=.true.

! Italy
  else if(callsign(1:1).eq.'I') then
    if((grid(1:2).eq.'JN' .and. callsign(3:3).gt.'2' .and. callsign(3:3).lt.':' .and. callsign(4:4).gt.'/' .and. &
       callsign(4:4).lt.'8') .or. (grid(1:2).eq.'JM' .and. callsign(3:3).gt.'3' .and. callsign(3:3).lt.':' .and. &
       callsign(4:4).gt.'4' .and. callsign(4:4).lt.':')) lgvalid=.true.

! Poland
  else if((callsign(1:1).eq.'S' .and. callsign(2:2).gt.'M' .and. callsign(2:2).lt.'S') .or. callsign(1:2).eq.'3Z' .or. &
    callsign(1:2).eq.'HF') then
    if((grid(1:2).eq.'JO' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'5') .or. &
       (grid(1:2).eq.'KO' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'5') .or. &
       grid(1:4).eq.'KN09' .or. grid(1:4).eq.'KN19' .or. grid(1:4).eq.'KO20' .or. grid(1:4).eq.'JN99' .or. &
       grid(1:4).eq.'JN89') lgvalid=.true.

! Australia
  else if(callsign(1:2).eq.'AX' .or. (callsign(1:1).eq.'V' .and. callsign(2:2).gt.'G' .and. callsign(2:2).lt.'O') .or. &
    callsign(1:2).eq.'VZ') then
    if((grid(1:2).eq.'QG' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'7') .or. &
       (grid(1:2).eq.'QF' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'7') .or. &
       (grid(1:2).eq.'PF' .and. grid(4:4).gt.'1' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'OF' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'QE' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.'5' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'QH' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'5') .or. & 
       (grid(1:2).eq.'OG' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':') .or. & 
       grid(1:2).eq.'PG' .or. grid(1:2).eq.'PH') lgvalid=.true.

! Spain
  else if((callsign(1:1).eq.'A' .and. callsign(2:2).gt.'L' .and. callsign(2:2).lt.'P') .or. (callsign(1:1).eq.'E' .and. &
          callsign(2:2).gt.'@' .and. callsign(2:2).lt.'I')) then
    if((grid(1:2).eq.'IN' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4') .or. &
       (grid(1:2).eq.'IM' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':').or. &
       (grid(1:2).eq.'JN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2') .or. &
       (grid(1:2).eq.'JM' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'7' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'IL' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'6' .and. grid(4:4).lt.':') .or. &
       grid(1:4).eq.'IM12' .or. grid(1:4).eq.'IM13') lgvalid=.true.

! China, Taiwan
  else if(callsign(1:1).eq.'B' .or. (callsign(1:1).eq.'3' .and. callsign(2:2).gt.'G' .and. callsign(2:2).lt.'V') .or. &
          callsign(1:2).eq.'XS') then
    if(grid(1:2).eq.'OL' .or. &
       (grid(1:2).eq.'PL' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2') .or. grid(1:2).eq.'OM' .or. &
       (grid(1:2).eq.'PM' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3') .or. &
       (grid(1:2).eq.'PN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'8') .or. &
       (grid(1:2).eq.'PO' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4') .or. &
       grid(1:4).eq.'OK48' .or. grid(1:4).eq.'OK49' .or. grid(1:4).eq.'OK58' .or. grid(1:4).eq.'OK59' .or. &
       grid(1:2).eq.'ON' .or. grid(1:4).eq.'OO90' .or. grid(1:4).eq.'OO91' .or. grid(1:2).eq.'NL' .or. grid(1:2).eq.'NN' .or. &
       grid(1:2).eq.'NM' .or. &
       (grid(1:2).eq.'MN' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2') .or. &
       (grid(1:2).eq.'MM' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':')) lgvalid=.true.

! Sweden
  else if((callsign(1:1).eq.'S' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'N') .or. callsign(1:2).eq.'7S' .or. &
          callsign(1:2).eq.'8S') then
    if((grid(1:2).eq.'JO' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'JP' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'9') .or. &
       (grid(1:2).eq.'KP' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.':')) &
      lgvalid=.true.

! Canada
  else if((callsign(1:1).eq.'C' .and. callsign(2:2).gt.'E' .and. callsign(2:2).lt.'L') .or. &
          (callsign(1:1).eq.'V' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'H') .or. &
          (callsign(1:1).eq.'X' .and. callsign(2:2).gt.'I' .and. callsign(2:2).lt.'P') .or. &
          callsign(1:2).eq.'VX' .or. callsign(1:2).eq.'VY' .or. callsign(1:2).eq.'CY' .or. callsign(1:2).eq.'CZ') then
    if((grid(1:2).eq.'CN' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'7' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'FN' .and. grid(4:4).gt.'1' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'EN' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.':') .or. grid(1:2).eq.'CO' .or. &
       (grid(1:2).eq.'GN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.':') .or. &
       grid(1:2).eq.'FO' .or. grid(1:2).eq.'EO' .or. grid(1:2).eq.'DO' .or. &
       (grid(1:2).eq.'GO' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'5') .or. &
       (grid(1:2).eq.'DN'.and. grid(4:4).gt.'8' .and. grid(4:4).lt.':') .or. &
       grid(1:2).eq.'CP' .or. grid(1:2).eq.'DP' .or. grid(1:2).eq.'EP')  lgvalid=.true.

! Finland
  else if(callsign(1:1).eq.'O' .and. callsign(2:2).gt.'E' .and. callsign(2:2).lt.'K') then
    if((grid(1:2).eq.'KP' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'6') .or. &
       grid(1:4).eq.'KO19' .or. grid(1:4).eq.'KQ30') lgvalid=.true.
  
! Greece
  else if((callsign(1:1).eq.'S' .and. callsign(2:2).gt.'U' .and. callsign(2:2).lt.'[') .or. callsign(1:2).eq.'J4') then
    if((grid(1:2).eq.'KM' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.'5' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.':') .or. &
       grid(1:4).eq.'JM99' .or. &
       (grid(1:2).eq.'KN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).eq.'1') .or. &
       (grid(1:2).eq.'KN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).eq.'0')) lgvalid=.true.

! Ukraine
  else if((callsign(1:1).eq.'U' .and. callsign(2:2).gt.'Q' .and. callsign(2:2).lt.'[') .or. &
          (callsign(1:1).eq.'E' .and. callsign(2:2).gt.'L' .and. callsign(2:2).lt.'P')) then
    if((grid(1:2).eq.'KO' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'3') .or. &
       (grid(1:2).eq.'KN' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':')) &
      lgvalid=.true.

! Norway
  else if(callsign(1:1).eq.'L' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'O') then
    if((grid(1:2).eq.'JO' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'7' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'JP' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'KP' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'7' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'KQ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2') .or. &
       grid(1:4).eq.'JQ90') lgvalid=.true.

! Belgium
  else if(callsign(1:1).eq.'O' .and. callsign(2:2).gt.'M' .and. callsign(2:2).lt.'U') then
    if((grid(1:2).eq.'JO' .and. (grid(3:4).eq.'10' .or. grid(3:4).eq.'11' .or. grid(3:4).eq.'20' .or. grid(3:4).eq.'21' .or. &
       grid(3:4).eq.'30')) .or. grid(1:4).eq.'JN29') lgvalid=.true.

! Switzerland, Liechtenstein
  else if(callsign(1:2).eq.'HB' .or. callsign(1:2).eq.'HE') then
    if((grid(1:3).eq.'JN3' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.'8') .or. &
       (grid(1:3).eq.'JN4' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.'8') .or. &
       grid(1:4).eq.'JN56') lgvalid=.true.

! Romania
  else if(callsign(1:1).eq.'Y' .and. callsign(2:2).gt.'N' .and. callsign(2:2).lt.'S') then
    if(grid(1:2).eq.'KN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'5' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.'9') &
      lgvalid=.true.

! Austria
  else if(callsign(1:2).eq.'OE') then
    if(grid(1:2).eq.'JN' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'9' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.'9') &
      lgvalid=.true.

! Indonesia
  else if((callsign(1:1).eq.'Y' .and. callsign(2:2).gt.'A' .and. callsign(2:2).lt.'I') .or. &
          (callsign(1:1).eq.'7' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'J') .or. &
          (callsign(1:1).eq.'8' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'J') .or. &
          (callsign(1:1).eq.'P' .and. callsign(2:2).gt.'J' .and. callsign(2:2).lt.'P')) then
    if((grid(1:2).eq.'NJ' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'NI' .and. grid(3:3).gt.'8' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'6' .and. grid(4:4).lt.':') .or. &
        grid(1:2).eq.'OI' .or. grid(1:2).eq.'PI' .or. &
       (grid(1:2).eq.'OJ' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'5') .or. &
       (grid(1:2).eq.'PJ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'5' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'5') .or. &
       (grid(1:2).eq.'QI' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'1' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'8') .or. &
       (grid(1:2).eq.'PH' .and. (grid(3:4).eq.'09' .or. grid(3:4).eq.'19' .or. grid(3:4).eq.'29'))) lgvalid=.true.

! Brazil
  else if((callsign(1:1).eq.'P' .and. callsign(2:2).gt.'O' .and. callsign(2:2).lt.'Z') .or. &
          (callsign(1:1).eq.'Z' .and. callsign(2:2).gt.'U' .and. callsign(2:2).lt.'[')) then
    if(grid(1:2).eq.'GG' .or. &
       (grid(1:2).eq.'HI' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'8') .or. &
       (grid(1:2).eq.'HH' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2') .or. &
       grid(1:2).eq.'GH' .or. grid(1:2).eq.'GI' .or. &
       (grid(1:2).eq.'GF' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.'5' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'GJ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'5') .or. &
       (grid(1:2).eq.'FJ' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'6') .or. &
       (grid(1:2).eq.'FI' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'FH' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.':')) &
      lgvalid=.true.

! Pakistan
  else if((callsign(1:1).eq.'A' .and. callsign(2:2).gt.'O' .and. callsign(2:2).lt.'T') .or. (callsign(1:1).eq.'6' .and. &
          callsign(2:2).gt.'O' .and. callsign(2:2).lt.'T')) then
    if((grid(1:2).eq.'MM' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'7') .or. &
       (grid(1:2).eq.'ML' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.':')) &
      lgvalid=.true.

! India
  else if((callsign(1:1).eq.'A' .and. callsign(2:2).gt.'S' .and. callsign(2:2).lt.'X') .or. (callsign(1:1).eq.'V' .and. &
          callsign(2:2).gt.'S' .and. callsign(2:2).lt.'X') .or. (callsign(1:1).eq.'8' .and. callsign(2:2).gt.'S' .and. &
          callsign(2:2).lt.'Z')) then
    if((grid(1:2).eq.'MJ' .and. grid(3:3).gt.'7' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'7' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'MK' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'NK' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'1' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'ML' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'NL' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'5') .or. &
       (grid(1:2).eq.'MM' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'3') .or. &
       grid(1:4).eq.'NM00') lgvalid=.true.

! Argentina
  else if(callsign(1:2).eq.'AY' .or. callsign(1:2).eq.'AZ' .or. (callsign(1:1).eq.'L' .and. callsign(2:2).gt.'0' .and. &
          callsign(2:2).lt.':') .or. (callsign(1:1).eq.'L' .and. callsign(2:2).gt.'N' .and. callsign(2:2).lt.'X')) then
    if((grid(1:2).eq.'GF' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'FF' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'FG' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'GG' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'6') .or. &
       (grid(1:2).eq.'FE' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.'9') .or. &
       (grid(1:2).eq.'FD' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.':')) &
      lgvalid=.true.

! not valid callsigns
  else if((callsign(1:1).eq.'C' .and. (callsign(2:2).eq.'0' .or. callsign(2:2).eq.'1' .or. callsign(2:2).eq.'7')) .or. &
          (callsign(1:1).eq.'2' .and. ((callsign(2:2).gt.'M' .and. callsign(2:2).lt.'U') .or. callsign(2:2).eq.'V' .or. &
          (callsign(2:2).gt.'W' .and. callsign(2:2).lt.'['))) .or. &
          callsign(1:2).eq.'E1' .or. callsign(1:2).eq.'H1') then
    nbadcrc=1; msg37='                                     '; return

! Nauru, Andorra
  else if((callsign(1:2).eq.'C2' .and. grid(1:4).eq.'RI39') .or. (callsign(1:2).eq.'C3' .and. grid(1:4).eq.'JN02')) then
    lgvalid=.true.
! Cyprus and UK base areas
  else if(callsign(1:2).eq.'C4' .or. callsign(1:2).eq.'5B' .or. callsign(1:2).eq.'H2' .or. callsign(1:2).eq.'P3' .or. &
          callsign(1:2).eq.'ZC') then
    if(grid(1:2).eq.'KM' .and. (grid(3:4).eq.'64' .or. grid(3:4).eq.'65' .or. grid(3:4).eq.'74' .or. grid(3:4).eq.'75')) &
      lgvalid=.true.
 ! The Gambia
  else if(callsign(1:2).eq.'C5' .and. grid(1:2).eq.'IK' .and. (grid(3:4).eq.'13' .or. grid(3:4).eq.'23' .or. &
          grid(3:4).eq.'33')) then
    lgvalid=.true.
 ! Bahamas
  else if(callsign(1:2).eq.'C6' .and. grid(1:2).eq.'FL' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. &
          grid(4:4).gt.'1' .and. grid(4:4).lt.'7') then
    lgvalid=.true.
 ! Mozambique
  else if(callsign(1:2).eq.'C8' .or. callsign(1:2).eq.'C9') then
  if((grid(1:2).eq.'KG' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.':') .or. &
     (grid(1:2).eq.'KH' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':') .or. &
     (grid(1:2).eq.'LH' .and. grid(3:3).eq.'1' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.':')) lgvalid=.true.
 ! Chile
  else if((callsign(1:1).eq.'C' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'F') .or. callsign(1:2).eq.'3G' .or. &
  callsign(1:2).eq.'XQ' .or. callsign(1:2).eq.'XR') then
    if((grid(1:2).eq.'FD' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'FE' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.'5') .or. &
       (grid(1:2).eq.'FF' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.'5') .or. &
       (grid(1:2).eq.'FG' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'7') .or. &
       (grid(1:2).eq.'FH' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'3')) &
      lgvalid=.true.
! Cuba
  else if(callsign(1:2).eq.'CL' .or. callsign(1:2).eq.'CM' .or. callsign(1:2).eq.'CO' .or. callsign(1:2).eq.'T4') then
    if((grid(1:2).eq.'EL' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'FL' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3') .or. grid(1:4).eq.'FK19') lgvalid=.true.
! Morocco
  else if(callsign(1:2).eq.'CN' .or. (callsign(1:1).eq.'5' .and. callsign(2:2).gt.'B' .and. callsign(2:2).lt.'H')) then
    if((grid(1:2).eq.'IM' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'IL' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.'8')) lgvalid=.true.
! Bolivia
  else if(callsign(1:2).eq.'CP') then
    if((grid(1:2).eq.'FH' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'FG' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':') .or. &
       grid(1:3).eq.'GH0' .or. grid(1:3).eq.'GH1' .or. grid(1:4).eq.'GG09') lgvalid=.true.
! Portugal and Madeira
  else if(callsign(1:1).eq.'C' .and. callsign(2:2).gt.'P' .and. callsign(2:2).lt.'U') then
    if(grid(1:3).eq.'IN5' .or. grid(1:3).eq.'IN6' .or. grid(1:3).eq.'IM5' .or. grid(1:3).eq.'IM6' .or. &
       grid(1:4).eq.'IM12' .or. grid(1:4).eq.'IM13') lgvalid=.true.
! Azores
  else if(callsign(1:2).eq.'CU') then
    if(grid(1:2).eq.'HM' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'8') lgvalid=.true.
! Uruguay
  else if(callsign(1:1).eq.'C' .and. callsign(2:2).gt.'U' .and. callsign(2:2).lt.'Y') then
    if(grid(1:2).eq.'GF' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4') lgvalid=.true.
! Angola
  else if(callsign(1:2).eq.'D2' .or. callsign(1:2).eq.'D3') then
    if((grid(1:2).eq.'JI' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'JH' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':') .or. &
        grid(1:3).eq.'KH0' .or. grid(1:3).eq.'KH1') lgvalid=.true.
! Cape Verde
  else if(callsign(1:2).eq.'D4' .and. ((grid(1:3).eq.'HK7' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.'8') .or. &
    (grid(1:3).eq.'HK8' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.'7'))) then
    lgvalid=.true.
! Liberia
  else if(callsign(1:2).eq.'D5' .or. callsign(1:2).eq.'5L' .or. callsign(1:2).eq.'5M' .or. callsign(1:2).eq.'6Z' .or. &
     callsign(1:2).eq.'A8' .or. callsign(1:2).eq.'EL') then
    if(grid(1:2).eq.'IJ' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.'9') &
      lgvalid=.true.
! Comoros
  else if(callsign(1:2).eq.'D6' .and. grid(1:2).eq.'LH' .and. (grid(3:4).eq.'17' .or. grid(3:4).eq.'18' .or. &
    grid(3:4).eq.'27')) then
    lgvalid=.true.

! Republic of Korea
  else if((callsign(1:1).eq.'D' .and. callsign(2:2).gt.'6' .and. callsign(2:2).lt.':') .or. &
          (callsign(1:1).eq.'6' .and. callsign(2:2).gt.'J' .and. callsign(2:2).lt.'O') .or. &
          callsign(1:2).eq.'DS' .or. callsign(1:2).eq.'DT' .or. callsign(1:2).eq.'HL') then
    if((grid(1:3).eq.'PM3' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.'9') .or. &
       (grid(1:3).eq.'PM4' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.'9') .or. &
       grid(1:4).eq.'PM24' .or. grid(1:4).eq.'PM57') lgvalid=.true.

! Philippines
  else if((callsign(1:1).eq.'D' .and. callsign(2:2).gt.'T' .and. callsign(2:2).lt.'[') .or. &
          (callsign(1:1).eq.'4' .and. callsign(2:2).gt.'C' .and. callsign(2:2).lt.'J')) then
    if((grid(1:2).eq.'PK' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3') .or. &
       (grid(1:2).eq.'PJ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':')) &
      lgvalid=.true.

! Thailand
  else if(callsign(1:2).eq.'E2' .or. callsign(1:2).eq.'HS') then
    if((grid(1:2).eq.'OK' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'NK' .and. grid(3:3).gt.'7' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'NJ' .and. grid(3:3).gt.'8' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'OJ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.'9') .or. &
       grid(1:4).eq.'NL90' .or. grid(1:4).eq.'OL00') lgvalid=.true.

! Eritrea
  else if(callsign(1:2).eq.'E3') then
    if((grid(1:2).eq.'KK' .and. grid(3:3).gt.'7' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.'8') .or. &
       (grid(1:2).eq.'LK' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'1' .and. grid(4:4).lt.'7')) &
      lgvalid=.true.

! Palestine
  else if(callsign(1:2).eq.'E4' .and. (grid(1:4).eq.'KM71' .or. grid(1:4).eq.'KM72')) then
    lgvalid=.true.

! Ireland
  else if(callsign(1:2).eq.'EI' .or. callsign(1:2).eq.'EJ') then
    if(grid(1:2).eq.'IO' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.'6') &
      lgvalid=.true.

! Armenia
  else if(callsign(1:2).eq.'EK') then
    if((grid(1:2).eq.'LN' .and. (grid(3:4).eq.'10' .or. grid(3:4).eq.'11' .or. grid(3:4).eq.'20' .or. &
        grid(3:4).eq.'21')) .or. &
       (grid(1:2).eq.'LM' .and. (grid(3:4).eq.'29' .or. grid(3:4).eq.'39' .or. grid(3:4).eq.'38'))) lgvalid=.true.

! Iran
  else if(callsign(1:2).eq.'EP' .or. callsign(1:2).eq.'EQ' .or. (callsign(1:1).eq.'9' .and. callsign(2:2).gt.'A' .and. &
          callsign(2:2).lt.'E')) then
    if((grid(1:2).eq.'LM' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'LL' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'ML' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'MM' .and. grid(3:3).eq.'0' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'7')) &
      lgvalid=.true.

! Moldova
  else if(callsign(1:2).eq.'ER') then
    if((grid(1:2).eq.'KN' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.'5' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.'9') .or. &
       grid(1:4).eq.'KN56') lgvalid=.true.

! Estonia
  else if(callsign(1:2).eq.'ES') then
    if((grid(1:2).eq.'KO' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'6' .and. grid(4:4).lt.':') .or. &
       grid(1:4).eq.'KO49') lgvalid=.true.

! Ethiopia
  else if(callsign(1:2).eq.'ET' .or. callsign(1:2).eq.'9E' .or. callsign(1:2).eq.'9F') then
    if((grid(1:2).eq.'LJ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'LK' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'5') .or. &
       (grid(1:2).eq.'KJ' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'KK' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'5')) &
      lgvalid=.true.

! Belarus
  else if(callsign(1:1).eq.'E' .and. (callsign(2:2).eq.'U' .or. callsign(2:2).eq.'V' .or. callsign(2:2).eq.'W')) then
    if(grid(1:2).eq.'KO' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.'7') &
      lgvalid=.true.

! Kyrgyzstan
  else if(callsign(1:2).eq.'EX') then
    if((grid(1:2).eq.'MN' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4') .or. &
       (grid(1:2).eq.'MM' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'7' .and. grid(4:4).eq.'9')) lgvalid=.true.

! Tajikistan
  else if(callsign(1:2).eq.'EY') then
    if((grid(1:2).eq.'MM' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'MN' .and. (grid(3:4).eq.'40' .or. grid(3:4).eq.'50'))) lgvalid=.true.

! Turkmenistan
  else if(callsign(1:2).eq.'EZ') then
    if((grid(1:2).eq.'LM' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'6' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'LN' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'3') .or. &
       (grid(1:2).eq.'MM' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'MN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2')) &
      lgvalid=.true.

! Hungary
  else if(callsign(1:1).eq.'H' .and. (callsign(2:2).eq.'A' .or. callsign(2:2).eq.'G')) then
    if((grid(1:2).eq.'JN' .and. grid(3:3).gt.'7' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.'9') .or. &
       (grid(1:2).eq.'KN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.'9')) &
      lgvalid=.true.

! Ecuador
  else if(callsign(1:1).eq.'H' .and. (callsign(2:2).eq.'C' .or. callsign(2:2).eq.'D')) then
    if((grid(1:2).eq.'FI' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'FJ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2') .or. &
       (grid(1:2).eq.'EI' .and. grid(3:3).eq.'9' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'EJ' .and. (grid(3:4).eq.'90' .or. grid(3:4).eq.'40')) .or. &
       (grid(1:2).eq.'EI' .and. (grid(3:4).eq.'49' .or. grid(3:4).eq.'59' .or. grid(3:4).eq.'48' .or. grid(3:4).eq.'58'))) &
      lgvalid=.true.

! Haiti
  else if(callsign(1:2).eq.'HH' .or. callsign(1:2).eq.'4V') then
       if((grid(1:2).eq.'FK' .and. (grid(3:4).eq.'38' .or. grid(3:4).eq.'39' .or. grid(3:4).eq.'48' .or. &
           grid(3:4).eq.'49')) .or. grid(1:4).eq.'FL30') lgvalid=.true.

! Dominican Republic
  else if(callsign(1:2).eq.'HI') then
       if(grid(1:2).eq.'FK' .and. (grid(3:4).eq.'48' .or. grid(3:4).eq.'49' .or. grid(3:4).eq.'58' .or. &
          grid(3:4).eq.'59' .or. grid(3:4).eq.'47')) lgvalid=.true.

! Colombia
  else if((callsign(1:1).eq.'H' .and. (callsign(2:2).eq.'K' .or. callsign(2:2).eq.'J')) .or. &
          (callsign(1:1).eq.'5' .and. (callsign(2:2).eq.'K' .or. callsign(2:2).eq.'J'))) then
    if((grid(1:2).eq.'FJ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'7') .or. &
       (grid(1:2).eq.'FK' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.'5' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4') .or. &
       (grid(1:2).eq.'FI' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'EJ' .and. (grid(3:4).eq.'93' .or. grid(3:4).eq.'94')) .or. grid(1:4).eq.'EK92') lgvalid=.true.

! Panama
  else if((callsign(1:1).eq.'H' .and. (callsign(2:2).eq.'P' .or. callsign(2:2).eq.'3' .or. callsign(2:2).eq.'8' .or. &
           callsign(2:2).eq.'9' .or. callsign(2:2).eq.'0')) .or. &
          (callsign(1:1).eq.'3' .and. (callsign(2:2).eq.'E' .or. callsign(2:2).eq.'F'))) then
    if((grid(1:2).eq.'FJ' .and. (grid(3:4).eq.'08' .or. grid(3:4).eq.'09' .or. grid(3:4).eq.'17' .or. &
        grid(3:4).eq.'18')) .or. &
       (grid(1:2).eq.'EJ' .and. (grid(3:4).eq.'88' .or. grid(3:4).eq.'89' .or. grid(3:4).eq.'97' .or. &
        grid(3:4).eq.'98' .or. grid(3:4).eq.'99'))) lgvalid=.true.

! Honduras
  else if(callsign(1:1).eq.'H' .and. (callsign(2:2).eq.'R' .or. callsign(2:2).eq.'Q')) then
    if(grid(1:2).eq.'EK' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.'9' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.'7') &
      lgvalid=.true.

  else if(callsign(1:1).eq.'J') then
! Djibouti
    if(callsign(1:2).eq.'J2') then
      if(grid(1:2).eq.'LK' .and. (grid(3:4).eq.'01' .or. grid(3:4).eq.'11' .or. grid(3:4).eq.'12')) lgvalid=.true.

! Grenada
    else if(callsign(1:2).eq.'J3') then
      if(grid(1:2).eq.'FK' .and. (grid(3:4).eq.'92' .or. grid(3:4).eq.'91')) lgvalid=.true.

! Guinea-Bissau
    else if(callsign(1:2).eq.'J5') then
      if(grid(1:2).eq.'IK' .and. (grid(3:4).eq.'21' .or. grid(3:4).eq.'22' .or. grid(3:4).eq.'11' .or. &
         grid(3:4).eq.'12' .or. grid(3:4).eq.'31' .or. grid(3:4).eq.'32' .or. grid(3:4).eq.'20')) lgvalid=.true.

! St. Lucia
    else if(callsign(1:2).eq.'J6') then
      if(grid(1:2).eq.'FK' .and. (grid(3:4).eq.'93' .or. grid(3:4).eq.'94')) lgvalid=.true.

! Dominica
    else if(callsign(1:2).eq.'J7' .and. grid(1:4).eq.'FK95') then
      lgvalid=.true.

! St. Vincent
    else if(callsign(1:2).eq.'J8') then
      if(grid(1:2).eq.'FK' .and. (grid(3:4).eq.'93' .or. grid(3:4).eq.'92')) lgvalid=.true.

! Ogasawara and Minami Torishima
    else if(callsign(1:3).eq.'JD1') then
      if(grid(1:4).eq.'QL04' .or. grid(1:4).eq.'QL05' .or. grid(1:4).eq.'QL16' .or. grid(1:4).eq.'QL17') lgvalid=.true.

! Mongolia
    else if(callsign(1:1).eq.'J' .and. (callsign(2:2).eq.'T' .or. callsign(2:2).eq.'U' .or. callsign(2:2).eq.'V')) then
      if((grid(1:2).eq.'ON' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.':') .or. &
        (grid(1:2).eq.'NN' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'1' .and. grid(4:4).lt.':').or. &
        (grid(1:2).eq.'NO' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2').or. &
        (grid(1:2).eq.'OO' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2').or. &
        (grid(1:2).eq.'OO' .and. (grid(3:4).eq.'60' .or. grid(3:4).eq.'70'))) lgvalid=.true.

! Svalbard, Bear Island
    else if(callsign(1:2).eq.'JW') then
      if((grid(1:2).eq.'JQ' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'KQ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'JR' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':' .and. grid(4:4).eq.'0') .or. &
         (grid(1:2).eq.'KR' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'7' .and. grid(4:4).eq.'0') .or. &
         grid(1:4).eq.'JQ94') lgvalid=.true.

! Jan Mayen
    else if(callsign(1:2).eq.'JX') then
      if(grid(1:2).eq.'IQ' .and. (grid(3:4).eq.'50' .or. grid(3:4).eq.'51' .or. grid(3:4).eq.'61')) lgvalid=.true.

! Jordan
    else if(callsign(1:2).eq.'JY') then
      if((grid(1:2).eq.'KM' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4').or. &
         (grid(1:2).eq.'KL' .and. (grid(3:4).eq.'79' .or. grid(3:4).eq.'89'))) lgvalid=.true.

! not valid callsigns
    else if((callsign(1:1).eq.'J' .and. (callsign(2:2).eq.'B' .or. callsign(2:2).eq.'C' .or. callsign(2:2).eq.'1' .or. &
            callsign(2:2).eq.'9' .or. callsign(2:2).eq.'Z')) .or. &
            (callsign(1:2).eq.'JD' .and. callsign(3:3).gt.'1' .and. callsign(3:3).lt.':')) then
      nbadcrc=1; msg37='                                     '; return

    endif ! 'J'

  else if(callsign(1:1).eq.'L') then
! Luxemburg
    if(callsign(1:2).eq.'LX') then
      if((grid(1:2).eq.'JN' .and. (grid(3:4).eq.'29' .or. grid(3:4).eq.'39')) .or. &
         (grid(1:2).eq.'JO' .and. (grid(3:4).eq.'20' .or. grid(3:4).eq.'30'))) lgvalid=.true.

! Lithuania
    else if(callsign(1:2).eq.'LY') then
      if(grid(1:2).eq.'KO' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'1' .and. grid(4:4).lt.'7') &
        lgvalid=.true.

! Bulgaria
    else if(callsign(1:2).eq.'LZ') then
      if(grid(1:2).eq.'KN' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.'5' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.'5') &
        lgvalid=.true.

    endif

! Peru
  else if((callsign(1:1).eq.'O' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'D') .or. callsign(1:2).eq.'4T') then
    if((grid(1:2).eq.'FH' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'FI' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'5') .or. &
       (grid(1:3).eq.'EI9' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.'7')) lgvalid=.true.

! Czech Republic
  else if(callsign(1:1).eq.'O' .and. (callsign(2:2).eq.'K' .or. callsign(2:2).eq.'L')) then
    if((grid(1:2).eq.'JO' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'7' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'JN' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2')) &
      lgvalid=.true.

! Slovak Republic
  else if(callsign(1:2).eq.'OM') then
    if((grid(1:2).eq.'JN' .and. grid(3:3).gt.'7' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'6' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'KN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'7' .and. grid(4:4).lt.':')) &
      lgvalid=.true.

! Greenland
  else if(callsign(1:2).eq.'OX' .or. callsign(1:2).eq.'XP') then
    if((grid(1:2).eq.'GP' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'HP' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'9' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. &
       grid(1:4).eq.'HQ90' .or. &
       (grid(1:2).eq.'GQ' .and. (grid(3:4).eq.'30' .or. grid(3:4).eq.'31' .or. grid(3:4).eq.'41' .or. grid(3:4).eq.'22' .or. &
        grid(3:4).eq.'14' .or. grid(3:4).eq.'12')) .or. &
       (grid(1:2).eq.'FQ' .and. (grid(3:4).eq.'76' .or. grid(3:4).eq.'67' .or. grid(3:4).eq.'57' .or. grid(3:4).eq.'56' .or. &
        grid(3:4).eq.'38'))) lgvalid=.true.

! Denmark
  else if((callsign(1:1).eq.'O' .and. (callsign(2:2).eq.'Z' .or. callsign(2:2).eq.'U' .or. callsign(2:2).eq.'V')) .or. &
          (callsign(1:1).eq.'5' .and. (callsign(2:2).eq.'P' .or. callsign(2:2).eq.'Q'))) then
    if(grid(1:2).eq.'JO' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.'8') &
      lgvalid=.true.

  else if(callsign(1:1).eq.'P') then
! Papua New Guinea
    if(callsign(1:2).eq.'P2') then
      if((grid(1:2).eq.'QI' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'9').or.&
         (grid(1:2).eq.'QH' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'7' .and. grid(4:4).lt.':')) &
        lgvalid=.true.

! Aruba
    else if(callsign(1:2).eq.'P4' .and. (grid(1:4).eq.'FK42' .or. grid(1:4).eq.'FK52')) then
      lgvalid=.true.

! DPR of Korea:
    else if(callsign(2:2).gt.'4' .and. callsign(2:2).lt.':') then
      if((grid(1:2).eq.'PM' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.'5' .and. grid(4:4).gt.'6' .and. grid(4:4).lt.':').or.&
         (grid(1:2).eq.'PN' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'3')) &
        lgvalid=.true.

! Curacao, Bonaire, Saba & St. Eustatius, Sint Maarten
    else if(callsign(1:2).eq.'PJ' .and. &
            ((callsign(3:3).eq.'2' .or. callsign(3:3).eq.'4') .and. grid(1:4).eq.'FK52') .or. &
            ((callsign(3:3).eq.'5' .or. callsign(3:3).eq.'6') .and. grid(1:4).eq.'FK87') .or. &
            ((callsign(3:3).eq.'7' .or. callsign(3:3).eq.'8' .or. callsign(3:3).eq.'0') .and. grid(1:4).eq.'FK88')) then
      lgvalid=.true.

! Suriname
    else if(callsign(1:2).eq.'PZ') then
      if(grid(1:2).eq.'GJ' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'1' .and. grid(4:4).lt.'6') &
        lgvalid=.true.

! not valid callsigns
    else if(callsign(1:2).eq.'P1') then
      nbadcrc=1; msg37='                                     '; return

    endif ! 'P'

  else if(callsign(1:1).eq.'S') then

! Slovenia
    else if(callsign(1:2).eq.'S5') then
      if(grid(1:2).eq.'JN' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.'9' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.'7') &
        lgvalid=.true.

! Western Sahara
    if(callsign(1:2).eq.'S0') then
      if(grid(1:2).eq.'IL' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'8') &
        lgvalid=.true.

! Bangladesh
    else if(callsign(1:2).eq.'S2' .or. callsign(1:2).eq.'S3') then
      if(grid(1:2).eq.'NL' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.'7') &
        lgvalid=.true.

! Seychelles
    else if(callsign(1:2).eq.'S7') then
      if((grid(1:2).eq.'LI' .and. (grid(3:4).eq.'75' .or. grid(3:4).eq.'76' .or. grid(3:4).eq.'74' .or. grid(3:4).eq.'64' &
        .or. grid(3:4).eq.'63' .or. grid(3:4).eq.'82' .or. grid(3:4).eq.'62' .or. grid(3:4).eq.'50' .or. grid(3:4).eq.'30'))&
         .or. (grid(1:2).eq.'LH' .and. (grid(3:4).eq.'59' .or. grid(3:4).eq.'39'))) lgvalid=.true.

! Sao Tome & Principe
    else if(callsign(1:2).eq.'S9' .and. (grid(1:4).eq.'JJ30' .or. grid(1:4).eq.'JJ31')) then
      lgvalid=.true.

! not valid callsigns
    else if(callsign(1:2).eq.'S1') then
      nbadcrc=1; msg37='                                     '; return

    endif ! 'S'

! Sudan
  else if(callsign(1:2).eq.'ST' .or. (callsign(1:1).eq.'6' .and. (callsign(2:2).eq.'T' .or. callsign(2:2).eq.'U'))) then
    if((grid(1:2).eq.'KK' .and. grid(3:3).gt.'0' .and. grid(3:3).lt.':') .or. &
       (grid(1:2).eq.'KL' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.'9' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2') .or. &
       grid(1:4).eq.'KJ79') lgvalid=.true.


! Turkey
  else if((callsign(1:1).eq.'T' .and. callsign(2:2).gt.'@' .and. callsign(2:2).lt.'D') .or. callsign(1:2).eq.'YM') then
    if((grid(1:2).eq.'KM' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. &
       (grid(1:2).eq.'KN' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'3') .or. &
       (grid(1:2).eq.'LN' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2') .or. &
       (grid(1:2).eq.'LM' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.':')) &
      lgvalid=.true.  

  else if(callsign(1:1).eq.'T') then
! Tuvalu
    if(callsign(1:2).eq.'T2') then
      if((grid(1:2).eq.'RI' .and. (grid(3:4).eq.'91' .or. grid(3:4).eq.'92' .or. grid(3:4).eq.'82' .or.grid(3:4).eq.'83'.or. &
         grid(3:4).eq.'84'))  .or. grid(1:4).eq.'RH99') lgvalid=.true.

! Kiribati, Banana Island
    else if(callsign(1:2).eq.'T3') then
      if(grid(1:1).eq.'R') then
        if((grid(1:3).eq.'RJ6' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4') .or. &
           (grid(1:3).eq.'RI7' .and. grid(4:4).gt.'6' .and. grid(4:4).lt.':') .or. &
           grid(1:4).eq.'RI49' .or. grid(1:4).eq.'RI87' .or. grid(1:4).eq.'RI88') lgvalid=.true.
      else if(grid(1:1).eq.'A') then
        if((grid(1:2).eq.'AI' .and. grid(3:3).gt.'1' .and. grid(3:3).lt.'5' .and. grid(4:4).gt.'4' .and. &
            grid(4:4).lt.'8') .or. grid(1:4).eq.'AJ94') lgvalid=.true.
      else if(grid(1:1).eq.'B') then
        if(grid(1:4).eq.'BJ03' .or. grid(1:4).eq.'BJ11' .or. grid(1:4).eq.'BJ12' .or. &
           (grid(1:3).eq.'BI2' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.'7') .or. &
           grid(1:4).eq.'BH48' .or. grid(1:4).eq.'BH49' .or. grid(1:4).eq.'BI40') lgvalid=.true.
      endif

! San Marino
    else if(callsign(1:2).eq.'T7' .and. grid(1:4).eq.'JN63') then
      lgvalid=.true.

! Palau
    else if(callsign(1:2).eq.'T8') then
      if(grid(1:2).eq.'PJ' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'1' .and. grid(4:4).lt.'9') &
        lgvalid=.true.

! Iceland
    else if(callsign(1:2).eq.'TF') then
      if((grid(1:2).eq.'HP' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.'7').or. &
         (grid(1:2).eq.'IP' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.'7')) &
        lgvalid=.true.

! Guatemala
    else if(callsign(1:2).eq.'TG' .or. callsign(1:2).eq.'TD') then
      if(grid(1:2).eq.'EK' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'2' .and. grid(4:4).lt.'8') &
        lgvalid=.true.

! Costa Rica, Cocos Island
    else if(callsign(1:2).eq.'TI' .or. callsign(1:2).eq.'TE') then
      if((grid(1:2).eq.'EK' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.'9' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2').or. &
         (grid(1:2).eq.'EJ' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.'9' .and. grid(4:4).gt.'7' .and. grid(4:4).lt.':').or. &
         grid(1:4).eq.'EJ65') lgvalid=.true.

! Cameroon
    else if(callsign(1:2).eq.'TJ') then
      if((grid(1:2).eq.'JJ' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'JK' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4')) &
        lgvalid=.true.

! Corsica
    else if(callsign(1:2).eq.'TK') then
      if(grid(1:3).eq.'JN4' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.'4') lgvalid=.true.

! Central African Republic
    else if(callsign(1:2).eq.'TL') then
      if((grid(1:2).eq.'JJ' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'1' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'KJ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'4' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'KK' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).eq.'0')) lgvalid=.true.

! Republic of the Congo
    else if(callsign(1:2).eq.'TN') then
      if((grid(1:2).eq.'JI' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.'9' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'JJ' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4')) &
        lgvalid=.true.

! Gabon
    else if(callsign(1:2).eq.'TR') then
      if(callsign(1:5).eq.'TR8CA' .or. &
         (grid(1:2).eq.'JJ' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'3').or. &
         (grid(1:2).eq.'JI' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.':')) &
        lgvalid=.true.

! Chad
    else if(callsign(1:2).eq.'TT') then
      if((grid(1:2).eq.'JK' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':') .or. &
         (grid(1:2).eq.'JJ' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'6' .and. grid(4:4).lt.':').or. &
         grid(1:4).eq.'KJ09' .or. &
         (grid(1:2).eq.'KK' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2') .or. &
         (grid(1:2).eq.'JL' .and. grid(3:3).gt.'6' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'4').or. &
         (grid(1:2).eq.'KL' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2')) &
        lgvalid=.true.

! Cote d'Ivoire:
    else if(callsign(1:2).eq.'TU') then
      if((grid(1:2).eq.'IJ' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.'9' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'IK' .and. (grid(3:4).eq.'60' .or. grid(3:4).eq.'70' .or. grid(3:4).eq.'50'))) lgvalid=.true.

! Benin:
    else if(callsign(1:2).eq.'TY') then
      if((grid(1:2).eq.'JJ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'1' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'JK' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'1' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'3')) &
        lgvalid=.true.

! Mali:
    else if(callsign(1:2).eq.'TZ') then
      if((grid(1:2).eq.'IK' .and. grid(3:3).gt.'2' .and. grid(3:3).lt.':') .or. &
         (grid(1:2).eq.'JK' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'IL' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'5').or. &
         (grid(1:2).eq.'JL' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'2' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'2')) &
        lgvalid=.true.

! not valid callsigns
    else if(callsign(1:2).eq.'T1') then
      nbadcrc=1; msg37='                                     '; return

    endif
  endif ! 'T'

  else if(callsign(1:1).eq.'V') then

! Antigua & Barbuda
    if(callsign(1:2).eq.'V2') then
      if(grid(1:2).eq.'FK' .and. (grid(3:4).eq.'86' .or. grid(3:4).eq.'97')) lgvalid=.true.

! Belize
    else if(callsign(1:2).eq.'V3') then
      if((grid(1:2).eq.'EK' .and. grid(3:3).eq.'5' .and. grid(4:4).gt.'4' .and. grid(4:4).lt.'9') .or. & 
         (grid(1:2).eq.'EK' .and. grid(3:3).eq.'6' .and. grid(4:4).gt.'5' .and. grid(4:4).lt.'9')) lgvalid=.true.

! St. Kitts & Nevis
    if(callsign(1:2).eq.'V4' .and. grid(1:4).eq.'FK87') then
      lgvalid=.true.

! Namibia
    else if(callsign(1:2).eq.'V5') then
      if((grid(1:2).eq.'JG' .and. grid(3:3).gt.'5' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'0' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'JH' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'3').or. &
         (grid(1:2).eq.'KG' .and. (grid(3:4).eq.'08' .or. grid(3:4).eq.'09')).or. &
         (grid(1:2).eq.'KH' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'3' .and. grid(4:4).gt.'/' .and. grid(4:4).lt.'3')) &
        lgvalid=.true.

! Micronesia
    else if(callsign(1:2).eq.'V6') then
      if((grid(1:2).eq.'PJ' .and. grid(3:3).gt.'7' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'7' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'QJ' .and. ((grid(4:4).gt.'4' .and. grid(4:4).lt.':') .or. grid(3:4).eq.'71' .or. &
          grid(3:4).eq.'73')) .or. &
         (grid(1:2).eq.'RJ' .and. (grid(3:4).eq.'06' .or. grid(3:4).eq.'15')) .or. grid(1:4).eq.'PK90') lgvalid=.true.

! Marshall Islands
    else if(callsign(1:2).eq.'V7') then
      if((grid(1:2).eq.'RJ' .and. grid(3:3).gt.'/' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'3' .and. grid(4:4).lt.':').or. &
         (grid(1:2).eq.'RK' .and. ((grid(3:3).gt.'/' .and. grid(3:3).lt.'6' .and. grid(4:4).gt.'/' .and. &
          grid(4:4).lt.'3') .or. grid(3:4).eq.'44' .or. grid(3:4).eq.'39'))) lgvalid=.true.

! Brunei Darussalam
    else if(callsign(1:2).eq.'V8') then
      if(grid(1:2).eq.'OJ' .and. (grid(3:4).eq.'74' .or. grid(3:4).eq.'75')) lgvalid=.true.

! Canada
    else if(callsign(1:6).eq.'VY0ERC') then
      lgvalid=.true.

! Heard Island, Macquarie Island
    else if(callsign(1:3).eq.'VK0' .and. &
            ((grid(1:2).eq.'MD' .and. (grid(3:4).eq.'66' .or. grid(3:4).eq.'67')) .or. &
             (grid(1:2).eq.'QD' .and. (grid(3:4).eq.'95' .or. grid(3:4).eq.'94')))) then
      lgvalid=.true.

! Cocos (Keeling) Islands, Lord Howe Island, Mellish Reef, Norfolk Island, Willis Island
    else if(callsign(1:3).eq.'VK9' .and. &
            ((grid(1:2).eq.'NH' .and. (grid(3:4).eq.'87' .or. grid(3:4).eq.'88')) .or. &
             grid(1:4).eq.'QF98' .or. grid(1:4).eq.'OH29' .or. &
             (grid(1:2).eq.'RG' .and. (grid(3:4).eq.'30' .or. grid(3:4).eq.'31')) .or. &
             (grid(1:2).eq.'QH' .and. grid(3:3).gt.'3' .and. grid(3:3).lt.'8' .and. grid(4:4).gt.'1' .and. &
              grid(4:4).lt.'5'))) then
      lgvalid=.true.

! Anguilla, Montserrat, British Virgin Islands
    else if(callsign(1:3).eq.'VP2' .and. &
            (grid(1:2).eq.'FK' .and. (grid(3:4).eq.'88' .or. grid(3:4).eq.'86' .or. grid(3:4).eq.'78'))) then
      lgvalid=.true.

! Turks & Caicos Islands
    else if((callsign(1:3).eq.'VP5' .or. callsign(1:3).eq.'VQ5') .and. &
            (grid(1:2).eq.'FL' .and. (grid(3:4).eq.'31' .or. grid(3:4).eq.'41'))) then
      lgvalid=.true.

! Pitcairn Island, Ducie Island
    else if(callsign(1:3).eq.'VP6' .and. (grid(1:2).eq.'CG' .and. (grid(3:4).eq.'55' .or. grid(3:4).eq.'44' .or. &
            grid(3:4).eq.'46' .or. grid(3:4).eq.'75'))) then
      lgvalid=.true.

! Falkland Islands, South Georgia Island, South Shetland Islands, South Orkney Islands, South Sandwich Islands
    else if(callsign(1:3).eq.'VP8') then
      if((grid(1:2).eq.'FD' .and. grid(3:3).gt.'8' .and. grid(3:3).lt.':' .and. grid(4:4).gt.'6' .and. grid(4:4).lt.'9').or. &
         grid(1:4).eq.'GD86' .or. &
         (grid(1:2).eq.'HD' .and. ((grid(3:4).eq.'15' .or. grid(3:4).eq.'25' .or.grid(3:4).eq.'05' .or.grid(3:4).eq.'16'.or. &
          grid(3:4).eq.'06') .or. (grid(1:3).eq.'HD6' .and.(grid(4:4).gt.'/' .and. grid(4:4).lt.'4')) .or. &
          grid(1:4).eq.'HD53')) .or. &
         grid(1:4).eq.'GC69' .or. grid(1:4).eq.'GC79') lgvalid=.true.

! Bermuda
    else if(callsign(1:3).eq.'VP9' .and. grid(1:4).eq.'FM72') then
      lgvalid=.true.

! Chagos Islands
    else if(callsign(1:3).eq.'VQ9') then
      if(grid(1:2).eq.'MI' .and. grid(3:3).gt.'4' .and. grid(3:3).lt.'7' .and. grid(4:4).gt.'1' .and. grid(4:4).lt.'5') &
        lgvalid=.true.

! Hong Kong
    else if(callsign(1:2).eq.'VR'.and. (grid(1:2).eq.'OL' .and. (grid(3:4).eq.'72' .or. grid(3:4).eq.'62'))) then
      lgvalid=.true.

! not valid callsigns
    else if(callsign(1:2).eq.'V1') then
      nbadcrc=1; msg37='                                     '; return

    endif
  endif ! 'V'

! add other

endif

        if(lchkcall .or. .not.lgvalid) then
          falsedec=.false.
          call chkflscall('CQ          ',callsign,falsedec)
          if(falsedec) then; nbadcrc=1; &
            msg37='                                     '; return; endif
        endif
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
