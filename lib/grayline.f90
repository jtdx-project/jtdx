subroutine grayline(nyear,month,nday,uth,mygrid,nduration,isun)

  character*8 mygrid
  real LST
  real lat,lon

  call grid2deg(MyGrid,elon,lat)
  lon=-elon

  uth0=uth-0.5*nduration/60.0
  uth1=uth+0.5*nduration/60.0

  call sun(nyear,month,nday,uth0,lon,lat,RASun,DecSun,LST,        &
       AzSun,ElSun0,mjd,day)
  call sun(nyear,month,nday,uth1,lon,lat,RASun,DecSun,LST,        &
       AzSun,ElSun1,mjd,day)

  elchk=-0.8333
  isun=-1
  if(elsun0.lt.elchk .and. elsun1.ge.elchk) then
     isun=0
  else if(elsun0.gt.elchk .and. elsun1.le.elchk) then
     isun=2
  else if(elsun1.gt.elchk) then
     isun=1
  else
     isun=3
  endif

  return
end subroutine grayline

