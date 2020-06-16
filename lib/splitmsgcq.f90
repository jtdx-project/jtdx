subroutine splitmsgcq(decoded,dtx)
  
  use jt65_mod8 !  dynhint(300), dynhintcq(150), rxfreq(20), rxfchg(1), ninterval, freq
  
  character decoded*22,direction*2,callsign*6,grid*4

  direction=''; callsign=''; grid=''; 
  i1=index(decoded,' '); i2=index(decoded(i1+1:),' ')

  if(i2.eq.3) then
    direction=decoded(4:5)
    i2=i2+i1; i3=index(decoded(i2+1:),' '); i3=i3+i2
    if(len_trim(decoded(i2+1:i3-1)).gt.6) return
    callsign=decoded(i2+1:i3-1); grid=decoded(i3+1:i3+4)
    go to 2
  endif

  if(i2.gt.3) then
    direction='01' ! non-directional CQ message
    i2=i2+i1
    callsign=decoded(i1+1:i2-1); grid=decoded(i2+1:i2+4)
    go to 2
  endif

2 do i=1,300
    if(dynhintcq(i)%ninterval.lt.0) then
      dynhintcq(i)%ninterval=ninterval; dynhintcq(i)%direction=direction; dynhintcq(i)%callsign=callsign
      dynhintcq(i)%grid=grid; dynhintcq(i)%freq=freq; dynhintcq(i)%dt=dtx
      exit
    endif
  enddo

!do i=1,30
!print *, dynhintcq(i)%ninterval
!print *, dynhintcq(i)%direction
!print *, dynhintcq(i)%callsign
!print *, dynhintcq(i)%grid
!enddo

  return
end subroutine splitmsgcq
