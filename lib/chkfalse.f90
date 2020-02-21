! last time modified by Igor UA3DJY on 20200215

subroutine chkfalse(decoded,mycall12,falsedec)
 
  character*22 decoded
  character*12 mycall12
  character*12 callsign1,callsign2,basecall
  logical(1) falsedec,lfound

  i2=index(decoded,' ')
  i3=index(decoded((i2+1):),' ')
  ieoc1=i2-1 ! end of 1st callsign
  iboc2=i2+1 ! beginning of 2nd callsign
  ieoc2=i2+i3-1 ! end of 2nd callsign
  falsedec=.true.; lfound=.false.

  if(decoded(1:3).eq.'CQ ' .or. decoded(1:3).eq.'DE ' .or. decoded(1:4).eq.'QRZ ') then
    if(i2.lt.3 .or. i3.lt.3) return ! check FT8 Hint CQ but directional messages
! CQ K1GT/QRP FN52  CQ EV1P/A  CQ EV1P/P  CQ EV1P/M  CQ EV1P/MM (standard messages)
    islash=index(decoded,'/')
    if(islash.ne.0) then
      if(decoded(islash+1:islash+4).eq.'QRP ' .or. decoded(islash+1:islash+2).eq.'A ' &
         .or. decoded(islash+1:islash+2).eq.'P ' &
         .or. decoded(islash+1:islash+2).eq.'M ' .or. decoded(islash+1:islash+3).eq.'MM ') then
        falsedec=.false.
        return
      endif
! CQ R8MB/1 (standard message)
      if(decoded(islash+1:islash+1).ge.'0' .and. decoded(islash+1:islash+1).le.'9' &
         .and. decoded(islash+2:islash+2).eq.' ') then
        falsedec=.false.
        return
      endif
! CQ YU/S56A (standard message)
      if(islash-iboc2.lt.4) then
        callsign2=decoded(islash+1:ieoc2)
!print *,"4","'"//callsign2//"'"
        call searchcalls(callsign2,"            ",lfound)
        if(lfound) then; falsedec=.false.; return; endif
      endif
    endif
! non compound callsigns
    callsign2=decoded(iboc2:ieoc2)
    if(callsign2(1:1).eq.'.') return ! CQ ...... JQ68
!print *,"5","'"//callsign2//"'"
    call searchcalls(callsign2,"            ",lfound)
    if(lfound) then; falsedec=.false.; return; endif
  else
! MM0VPY 73 (standard message)
    if(index(decoded,' 73 ').gt.4) then; falsedec=.false.; return; endif
! check 'call1 + call2 [+]' messages	
    if(i2.lt.4 .or. i3.lt.4) return
    islash=index(decoded,'/')
    if(islash.eq.0) then
      callsign1=decoded(1:ieoc1)
      callsign2=decoded(iboc2:ieoc2)
      if(callsign2(1:1).eq.'.' .or. callsign1(1:1).eq.'.') return !  PJ8WRZ ...... DA82  ...... VF8UWG KD91
! skip mycall for iaptype 2 FT8 messages
!print *,"1","'"//callsign2//"'"
      if(callsign1.ne.mycall12) then; call searchcalls(callsign1,callsign2,lfound)
      else; call searchcalls(callsign2,"            ",lfound)
      endif
      if(lfound) then; falsedec=.false.; return; endif
    else
! '5B/SQ2MGM G4CUS' 'SQ2MGM/1 G4CUS'
      if(islash.lt.ieoc1) then
        nfirsth=islash-1
        nsecondh=ieoc1-islash
        if(nfirsth.gt.nsecondh) then
          basecall=decoded(1:islash-1)
        else
          basecall=decoded(islash+1:ieoc1)
        endif
        callsign2=decoded(iboc2:ieoc2)
!print *,"2","'"//basecall//"'"
        call searchcalls(callsign2,basecall,lfound)
        if(lfound) then; falsedec=.false.; return; endif
      elseif (islash.gt.iboc2) then
! 'G4CUS 5B/SQ2MGM' 'G4CUS SQ2MGM/1'
        nfirsth=islash-i2-1
        nsecondh=ieoc2-islash
        if(nfirsth.gt.nsecondh) then
          basecall=decoded(i2+1:islash-1)
        else
          basecall=decoded(islash+1:ieoc2)
        endif
        callsign1=decoded(1:ieoc1)
! skip mycall for iaptype 2 FT8 messages
!print *,"3","'"//basecall//"'"
          if(callsign1.ne.mycall12) then; call searchcalls(callsign1,basecall,lfound)
          else; call searchcalls(basecall,"            ",lfound)
          endif
          if(lfound) then; falsedec=.false.; return; endif
      endif
    endif
  endif

  return
end subroutine chkfalse
