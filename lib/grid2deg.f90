subroutine grid2deg(grid0,dlong,dlat)

! Converts Maidenhead grid locator to degrees of West longitude
! and North latitude.

  character*8 grid0,grid
  character*1 g1,g2,g3,g4,g5,g6,g7,g8

  grid=grid0
  i=ichar(grid(5:5))
  if(grid(5:5).eq.' ' .or. i.le.64 .or. i.ge.128) grid(5:6)='ll'
  i2=ichar(grid(7:7))
  if(grid(7:7).eq.' ' .or. i2.le.47 .or. i2.ge.58) grid(7:8)='55'

  if(grid(1:1).ge.'a' .and. grid(1:1).le.'z') grid(1:1)=          &
        char(ichar(grid(1:1))+ichar('A')-ichar('a'))
  if(grid(2:2).ge.'a' .and. grid(2:2).le.'z') grid(2:2)=          &
        char(ichar(grid(2:2))+ichar('A')-ichar('a'))
  if(grid(5:5).ge.'A' .and. grid(5:5).le.'Z') grid(5:5)=          &
        char(ichar(grid(5:5))-ichar('A')+ichar('a'))
  if(grid(6:6).ge.'A' .and. grid(6:6).le.'Z') grid(6:6)=          &
        char(ichar(grid(6:6))-ichar('A')+ichar('a'))

  g1=grid(1:1)
  g2=grid(2:2)
  g3=grid(3:3)
  g4=grid(4:4)
  g5=grid(5:5)
  g6=grid(6:6)
  g7=grid(7:7)
  g8=grid(8:8)

  nlong = 180 - 20*(ichar(g1)-ichar('A'))
  n20d = 2*(ichar(g3)-ichar('0'))
  xminlong2 = (ichar(g7)-ichar('0')) / 10.0
  xminlong = 5*(ichar(g5)-ichar('a')+xminlong2)
  dlong = nlong - n20d - xminlong/60.0
  nlat = -90+10*(ichar(g2)-ichar('A')) + ichar(g4)-ichar('0')
  xminlat2 = (ichar(g8)-ichar('0')) / 10.0
  xminlat = 2.5*(ichar(g6)-ichar('a')+xminlat2)
  dlat = nlat + xminlat/60.0
!  print *, grid, nlong, n20d, xminlong2, xminlong, dlong, nlat, xminlat2, xminlat, dlat
  return
end subroutine grid2deg
