subroutine deg2grid(dlong0,dlat,grid)

  real dlong                        !West longitude (deg)
  real dlat                         !Latitude (deg)
  character grid*6

  dlong=dlong0
  if(dlong.lt.-180.0) dlong=dlong+360.0
  if(dlong.gt.180.0) dlong=dlong-360.0

! Convert to units of 5 min of longitude, working east from 180 deg.
  nlong=int(60.0*(180.0-dlong)/5.0)
  n1=nlong/240                      !20-degree field
  n2=(nlong-240*n1)/24              !2 degree square
  n3=nlong-240*n1-24*n2             !5 minute subsquare
  grid(1:1)=char(ichar('A')+n1)
  grid(3:3)=char(ichar('0')+n2)
  grid(5:5)=char(ichar('a')+n3)

! Convert to units of 2.5 min of latitude, working north from -90 deg.
  nlat=int(60.0*(dlat+90)/2.5)
  n1=nlat/240                       !10-degree field
  n2=(nlat-240*n1)/24               !1 degree square
  n3=nlat-240*n1-24*n2              !2.5 minuts subsquare
  grid(2:2)=char(ichar('A')+n1)
  grid(4:4)=char(ichar('0')+n2)
  grid(6:6)=char(ichar('a')+n3)

  return
end subroutine deg2grid
