! This source code file was last time modified by Igor UA3DJY on January 16th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine splitmsg(decoded,mycall,dtx,hintedrxfreq)
  
  use jt65_mod8 !  dynhint(300), dynhintcq(150), rxfreq(20), rxfchg(1), ninterval, freq
  
  character call1*6,call2*6,grid2*4
  save

  character mycall*12,decoded*22,direction*2
  logical(1) hintedrxfreq

  direction='00' ! non-CQ message
  nlastrx=0 ! unknown type of the standard message, initialization
  grid2='HQ01' ! unknown grid square, initialization and protection for hintdyn decoder

! nlastrx last received message matrix, used for wideband hintdyn decoder, dynhint(300) structure
! nlastrx   last RX message   / dinterval1 expected inversed msg / dinterval2 expected RX message
!   0           initialization/               none              / none
!   1           GRID          /               -01               / R-01, and might be again GRID
!   2           -01           /               R-01              / RRR/RR73/73, and might be again -01
!   3           R-01          /               RRR/RR73/73       / RRR/RR73/73,  and might be again R-01
!   4           RRR/RR73/73   /               RRR/RR73/73       / none

  if(decoded(1:3).eq.'CQ ' .or. &
     decoded(1:3).eq.'QRZ' .or. decoded(1:3).eq.'DE ') go to 2

  i1=index(decoded,' ')
  if(i1.lt.4) return
  i2=index(decoded(i1+1:),' ')
  if(i2.lt.4) return
  i2=i2+i1
  call1=decoded(1:i1-1)
  lmycall=len_trim(mycall)
  lcall1=len_trim(call1)
  if(lcall1.eq.lmycall) then
     if(call1(1:lcall1).eq.mycall(1:lmycall)) then
        call2=decoded(i1+1:i2-1)
        go to 4
     endif
  endif

  call2=decoded(i1+1:i2-1)
  nlastrx=1 ! shall be GRID if conditions below are not met
  do i=1,9
     if(i2+i.gt.16) exit ! protection for getting out of the decoded*22 length
     if(decoded(i2+i-1:i2+i).eq.' -') nlastrx=2
     if(decoded(i2+i-1:i2+i+1).eq.' R-') nlastrx=3
     if(decoded(i2+i-1:i2+i+1).eq.' RR') nlastrx=4
     if(decoded(i2+i-1:i2+i+1).eq.' 73') nlastrx=4
  enddo

  if(nlastrx.eq.1) then ! there is grid square in the message
     i3=index(decoded(i2+1:),' ')
     if(i3.eq.5) then
        grid2=decoded(i2+1:i2+1+i3-1)
     endif
  endif
!print *,call1,' ',call2,nlastrx
!print *,nlastrx
!print *,grid2

  do i=1,300
    if(dynhint(i)%ninterval.lt.0) then
       dynhint(i)%ninterval=ninterval
       dynhint(i)%call1=call1
       dynhint(i)%call2=call2
       dynhint(i)%grid2=grid2
       dynhint(i)%nlastrx=nlastrx
       dynhint(i)%freq=freq
       dynhint(i)%dt=dtx
       exit
    endif
  enddo

2 if(decoded(1:3).eq.'CQ ' .and. decoded(6:6).ne.' ') then ! CQ message
     i2=index(decoded(4:),' ')
     i2=i2+3
     call2=decoded(4:i2-1)
     direction='01'
  endif

  if(decoded(1:3).eq.'CQ ' .and. decoded(6:6).eq.' ') then ! directional CQ message
     i2=index(decoded(7:),' ')
     i2=i2+6
     call2=decoded(7:i2-1)
     direction=decoded(4:5)
  endif
  
4 if(hintedrxfreq) then
    do i=1,20
    if(rxfreq(i)%ninterval.lt.0) then
       rxfreq(i)%ninterval=ninterval
       rxfreq(i)%direction=direction
       rxfreq(i)%call2=call2
       rxfreq(i)%freq=freq
       rxfreq(i)%dt=dtx
       exit
    endif
    enddo
  endif

!do i=1,30
!print *, dynhint(i)%ninterval
!print *, dynhint(i)%call1
!enddo
!do i=1,20
!if(rxfreq(i)%ninterval.ge.1) then
!print *, rxfreq(i)%direction
!print *, rxfreq(i)%call2
!endif
!enddo

return
end subroutine splitmsg
