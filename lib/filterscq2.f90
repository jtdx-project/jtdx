! last time modified by Igor UA3DJY on 20200215

subroutine filterscq2(decoded,falsedec,iFreeText)
  
  character*22 decoded
  character*12 basecall
  logical(1) falsedec,lfound

  islash=index(decoded,'/')
  i2=index(decoded,' ')
  i3=index(decoded((i2+1):),' ')

  if(islash.ne.0) then
     ieoc=i2+i3-1 ! end of callsign

     if(iFreeText.eq.0) then ! slash in standard message
! CQ K1GT/QRP FN52  CQ EV1P/A  CQ EV1P/P  CQ EV1P/M  CQ EV1P/MM
        if(decoded(islash+1:islash+4).eq.'QRP ' .or. decoded(islash+1:islash+2).eq.'A ' &
           .or. decoded(islash+1:islash+2).eq.'P ' .or. decoded(islash+1:islash+2).eq.'M ' &
           .or. decoded(islash+1:islash+3).eq.'MM ') return
! CQ R8MB/1
        if(decoded(islash+1:islash+1).ge.'0' .and. decoded(islash+1:islash+1).le.'9' &
           .and. decoded(islash+2:islash+2).eq.' ') return
     endif
! CQ 4ID5/G7RPY   QRZ UMQH/VR0KZP AJ8  CQ LU2FQ/J GF07  CQ J/LU2FQ GF07  CQ 3DA0/K1AA
     if(islash.gt.(i2+1) .and. islash.lt.ieoc) then
        nleft=islash-i2-1
        nright=ieoc-islash
        if(nleft.gt.nright) then
           basecall=decoded(i2+1:islash-1)
        else ! check right side
           basecall=decoded(islash+1:ieoc)
        endif
        falsedec=.true.; lfound=.false.
        call searchcalls(basecall,"            ",lfound); if(lfound) falsedec=.false.; if(.not.falsedec) return
! check left side if both sides have equal length
        if(nleft.eq.nright) then
           basecall=decoded(i2+1:islash-1)
           if(basecall(nleft:nleft).gt.'9' .and. basecall(nleft:nleft).le.'Z') then
             call searchcalls(basecall,"            ",lfound); if(lfound) falsedec=.false.; if(.not.falsedec) return
           endif
        endif
     endif
  endif
 ! freemsg CQ UL7DIG+2C-  CQ YR100R KN2  CQ TC1919ATA
  if(iFreeText.eq.1) then
    if(i3.gt.7 .and. i3.le.10) then
       do i=4,13
          if(decoded(i:i).eq.'+' .or. decoded(i:i).eq.'-' .or. decoded(i:i).eq.'.' .or. decoded(i:i).eq.'?') falsedec=.true.
       enddo
    endif
    if(.not.falsedec .and. i3.gt.10) then
 !more than 9-char long special callsigns will be lost
       falsedec=.true.
       return
    endif
  endif

  return
end subroutine filterscq2
