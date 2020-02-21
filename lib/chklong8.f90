! This source code file was last time modified by Igor UA3DJY on 20190227

subroutine chklong8(callsign,falsedec)

  parameter (NMASK10=6,NMASK11=4)
  character callsign*12,callmask*12
  logical(1) falsedec
  character*12 mask10(NMASK10),mask11(NMASK11)
  data mask10/'0011000000  ','0011100000  ','0011110000  ','1011000000  ','1011100000  ','1011110000  '/
  data mask11/'00111000000 ','00111100000 ','10111000000 ','10111100000 '/

! <...> I78PY2MTFLF RR73    
! <...> CM5XFIVGPIP RR73  
! WG0XC120LS9 <...> RR73    
! <...> A7MNPJDX/QO RRR   TODO list
! CQ P2PSR6UWBHS

!valid masks for 11-char callsign:
! DL2019ABCDE TC630ABCDEF 9A200ABCDEF 9A2019ABCDE
! 00111100000 00111000000 10111000000 10111100000
  callmask='            '
! need to exclude 8J/8N/8K Japanese callsigns from checking
  if(index(callsign,'/').le.0 .and. callsign(1:2).ne.'8J' .and. callsign(1:2).ne.'8N') then
    if(len_trim(callsign).eq.10) then
      do i=1,10
        if(callsign(i:i).ge.'0' .and. callsign(i:i).le.'9') then; callmask(i:i)='1'; else; callmask(i:i)='0'; endif
      enddo
!print *,callsign,callmask
      falsedec=.true.
      do i=1,NMASK10
        if(callmask.eq.mask10(i)) then; falsedec=.false.; return; endif 
      enddo
    endif
    if(len_trim(callsign).eq.11) then
      do i=1,11
        if(callsign(i:i).ge.'0' .and. callsign(i:i).le.'9') then; callmask(i:i)='1'; else; callmask(i:i)='0'; endif
      enddo
!print *,callsign,callmask
      falsedec=.true.
      do i=1,NMASK11
        if(callmask.eq.mask11(i)) then; falsedec=.false.; return; endif 
      enddo
    endif
  endif

  return
end subroutine chklong8
