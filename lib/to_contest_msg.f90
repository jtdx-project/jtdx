subroutine to_contest_msg(msg0,msg)

! If the message has "R grid4" istead of "grid4", remove the "R "
! and substitute the diametrically opposite grid.
  
  character*6 g1,g2
  character*22 msg0,msg
  logical isgrid
  isgrid(g1)=g1(1:1).ge.'A' .and. g1(1:1).le.'R' .and. g1(2:2).ge.'A' .and. &
       g1(2:2).le.'R' .and. g1(3:3).ge.'0' .and. g1(3:3).le.'9' .and.       &
       g1(4:4).ge.'0' .and. g1(4:4).le.'9' .and. g1(1:4).ne.'RR73'

  i0=index(msg0,' R ') + 3                  !Check for ' R ' in message
  g1=msg0(i0:i0+3)//'  '
  if(isgrid(g1)) then                       !Check for ' R grid'
     call grid2deg(g1,dlong,dlat)
     dlong=dlong+180.0
     if(dlong.gt.180.0) dlong=dlong-360.0
     dlat=-dlat
     call deg2grid(dlong,dlat,g2)           !g2=antipodes grid
     msg=msg0(1:i0-3)//g2(1:4)              !Send message with g2
  else
     msg=msg0
  endif

  return
end subroutine to_contest_msg
