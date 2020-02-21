! This source code file was last time modified by Igor UA3DJY on February 21st, 2018
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine filtersstd(decoded,mycall,falsedec,syncpwr)

  integer, parameter :: EGRIDMAX=111,PFXDMAX=69,PFXLMAX=54
  real syncpwr,datapwr,datacorr
  character grid*4,mycall*6,call1*6,call2*6,decoded*22,mycall12*12
  logical(1) falsedec
  character*2 egrid(EGRIDMAX)&
  /'AA','AB','AC','AD','AF','AM','AN','AR','BA','BB','BC','BD','BE','BF','BM','BN','CA','CB','CC','CD', &
   'CE','CF','CL','CK','CJ','CR','DA','DB','DC','DD','DE','DF','DH','DI','DJ','DR','EA','EB','EC','ED', &
   'EE','EF','EG','EH','ER','FA','FB','GA','GB','GE','GL','GM','GR','HA','HB','HC','HE','HF','HG','HJ', &
   'HL','HN','HO','HR','IA','IB','IC','ID','IE','IG','JA','JB','JC','JE','JR','KA','KB','KC','KD','KR', &
   'LA','LB','LC','LD','LF','LQ','MA','MB','MG','MR','NA','NB','NC','ND','NE','NF','NG','PA','PC','PD', &
   'PE','QA','QB','QC','RA','RC','RL','RM','RN','RQ','RR'/
  character*2 pfxd(PFXDMAX)&
  /'2E','2I','2K','2M','2W','3A','3B','3C','3D','3V','3W','3X','3Y','3Z','4A','4F','4J','4K','4L','4N', &
   '4O','4S','4U','4X','4Z','5A','5C','5H','5P','5Q','5R','5T','5U','5V','5W','5X','6K','7J','7K','7L', &
   '7M','7N','7O','7P','7Q','7S','7X','7Z','8J','8N','8P','8Q','8R','8S','9A','9G','9H','9J','9K','9L', &
   '9M','9N','9Q','9U','9V','9W','9X','9Y','9Z'/
  character*2 pfxl(PFXLMAX)&
  /'A2','A3','A4','A5','A6','A7','A9','C2','C3','C4','C5','C6','C9','D2','D4','D6','E1','E2','E3','E4', &
   'E5','E9','H2','H4','J2','J3','J4','J5','J6','J7','J8','P2','P3','P4','S0','S2','S5','S6','S7','S9', &
   'T2','T3','T4','T7','T8','T9','V2','V3','V4','V5','V6','V7','Z2','Z3'/


  mycall12='' !not used in chkfalse() for JT65
 
! check if any callsign begins with two digits

  i1=index(decoded,' ')
  if(i1.lt.4) go to 2
  i2=index(decoded(i1+1:),' ')
  if(i2.lt.4) go to 2
  if(i1.eq.4 .and. i2.eq.4) go to 2
  i2=i2+i1

  call1=decoded(1:i1-1)
  if(call1.eq.mycall) go to 4
  call2=decoded(i1+1:i2-1)
  if(i2.gt.14) go to 2 ! avoid getting out of decoded*22
  grid=decoded(i2+1:i2+4)

  if(call1(1:1).ge.'0' .and. call1(1:1).le.'9' .and. &
     call1(2:2).ge.'0' .and. call1(2:2).le.'9') go to 2
  
  if(call2(1:1).ge.'0' .and. call2(1:1).le.'9' .and. &
     call2(2:2).ge.'0' .and. call2(2:2).le.'9') go to 2

  if(call1(1:1).eq.'Q' .or. call2(1:1).eq.'Q') go to 2
  if(call1(1:1).eq.'0' .or. call2(1:1).eq.'0') go to 2
  if(call1(1:1).eq.'1' .and. call1(2:2).ne.'A' .and. call1(2:2).ne.'B') go to 2
  if(call2(1:1).eq.'1' .and. call2(2:2).ne.'A' .and. call2(2:2).ne.'B') go to 2

  if((grid(1:2).eq."GK" .and. grid(1:4).ne."GK03") .or. &
     (grid(1:2).eq."IF" .and. grid(1:4).ne."IF32") .or. &
     (grid(1:2).eq."IH" .and. grid(1:4).ne."IH74") .or. &
     (grid(1:2).eq."II" .and. grid(1:4).ne."II22") .or. &
     (grid(1:2).eq."JD" .and. grid(1:4).ne."JD15") .or. &
     (grid(1:2).eq."KE" .and. grid(1:4).ne."KE83") .or. &
      grid(1:3).eq."KF7" .or. &
     (grid(1:2).eq."LE" .and. grid(1:3).ne."LE5" .and. grid(1:3).ne."LE6") .or. &
      grid(1:4).eq."LJ08" .or. &
     (grid(1:2).eq."PB" .and. grid(1:4).ne."PB14") .or. &
      grid(1:4).eq."PI83" .or. &
     (grid(1:3).eq."PO8" .and. grid(1:4).ne."PO80") .or. &
      grid(1:3).eq."PP8" .or. grid(1:3).eq."PQ8" .or. &
     (grid(1:2).eq."QL" .and. grid(1:3).ne."QL0" .and. grid(1:3).ne."QL1") .or. &
     (grid(1:2).eq."RB" .and. grid(1:4).ne."RB25" .and. grid(1:4).ne."RB32") .or. &
     (grid(1:2).eq."RK" .and. grid(1:4).ne."RK39")) then

     if(call1(1:1).ge.'0' .and. call1(1:1).le.'9' .and. &
        call2(1:1).ge.'0' .and. call2(1:1).le.'9') go to 2

     if(call1(1:1).ge.'0' .and. call1(1:1).le.'9' .and. &
        call2(2:2).ge.'0' .and. call2(2:2).le.'9' .and. &
        call2(3:3).ge.'0' .and. call2(3:3).le.'9') go to 2

     if(call2(1:1).ge.'0' .and. call2(1:1).le.'9' .and. &
        call1(2:2).ge.'0' .and. call1(2:2).le.'9' .and. &
        call1(3:3).ge.'0' .and. call1(3:3).le.'9') go to 2
		 
     if(call1(2:2).ge.'0' .and. call1(2:2).le.'9' .and. &
        call1(3:3).ge.'0' .and. call1(3:3).le.'9' .and. &
        call2(2:2).ge.'0' .and. call2(2:2).le.'9' .and. &
        call2(3:3).ge.'0' .and. call2(3:3).le.'9') go to 2

     call chkfalse(decoded,mycall12,falsedec)
     if(falsedec) then
!print *,decoded ! diag only
        go to 4
     endif
  endif
  
  do i=1,EGRIDMAX
    if(grid(1:2).eq.egrid(i)) then

     if(call1(1:1).ge.'0' .and. call1(1:1).le.'9' .and. &
        call2(1:1).ge.'0' .and. call2(1:1).le.'9') go to 2

     if(call1(1:1).ge.'0' .and. call1(1:1).le.'9' .and. &
        call2(2:2).ge.'0' .and. call2(2:2).le.'9' .and. &
        call2(3:3).ge.'0' .and. call2(3:3).le.'9') go to 2

     if(call2(1:1).ge.'0' .and. call2(1:1).le.'9' .and. &
        call1(2:2).ge.'0' .and. call1(2:2).le.'9' .and. &
        call1(3:3).ge.'0' .and. call1(3:3).le.'9') go to 2
		 
     if(call1(2:2).ge.'0' .and. call1(2:2).le.'9' .and. &
        call1(3:3).ge.'0' .and. call1(3:3).le.'9' .and. &
        call2(2:2).ge.'0' .and. call2(2:2).le.'9' .and. &
        call2(3:3).ge.'0' .and. call2(3:3).le.'9') go to 2

       call chkfalse(decoded,mycall12,falsedec)
       if(falsedec) then
!print *,decoded ! diag only
          go to 4
       endif
    endif
  enddo
  

  if(call1(1:1).ge.'0' .and. call1(1:1).le.'9') then
     do i=1,PFXDMAX
        if(call1(1:2).eq.pfxd(i)) go to 4
     enddo
     call chkfalse(decoded,mycall12,falsedec)
     if(falsedec) then
!print *,decoded ! diag only
        go to 4
     endif
  endif

  if(call2(1:1).ge.'0' .and. call2(1:1).le.'9') then
     do i=1,PFXDMAX
        if(call2(1:2).eq.pfxd(i)) go to 4
     enddo
     call chkfalse(decoded,mycall12,falsedec)
     if(falsedec) then
!print *,decoded ! diag only
        go to 4
     endif
  endif

  if((call1(2:2).ge.'0' .and. call1(2:2).le.'9') .and. &
     (call1(3:3).ge.'0' .and. call1(3:3).le.'9')) then
      do i=1,PFXLMAX
        if(call1(1:2).eq.pfxl(i)) go to 4
      enddo
     call chkfalse(decoded,mycall12,falsedec)
     if(falsedec) then
!print *,decoded ! diag only
        go to 4
     endif
  endif

  if((call2(2:2).ge.'0' .and. call2(2:2).le.'9') .and. &
     (call2(3:3).ge.'0' .and. call2(3:3).le.'9')) then
      do i=1,PFXLMAX
        if(call2(1:2).eq.pfxl(i)) go to 4
      enddo
     call chkfalse(decoded,mycall12,falsedec)
     if(falsedec) then
!print *,decoded ! diag only
        go to 4
     endif
  endif

! check each callsign in the standard message if SNR is less than -27dB
  if(syncpwr.lt.70.)  then
     call chkfalse(decoded,mycall12,falsedec)
        if(falsedec) go to 4
  endif

  call datacor(datapwr,datacorr)

  qualityfac=datapwr/syncpwr

! check each callsign in the standard message if datapower/syncpower ratio is less than 0.165
  if(qualityfac.lt.0.18)  then
     call chkfalse(decoded,mycall12,falsedec)
     if(falsedec) go to 4
  endif

! check each callsign in the standard message if datacorr is less than 1.8
  if(datacorr.lt.1.8)  then
     call chkfalse(decoded,mycall12,falsedec)
     if(falsedec) go to 4
  endif

  go to 4

2 falsedec=.true.; return

4 return
end subroutine filtersstd
