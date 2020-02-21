subroutine sun(y,m,DD,UT,lon,lat,RA,Dec,LST,Az,El,mjd,day)

  implicit none

  integer y                         !Year
  integer m                         !Month
  integer DD                        !Day
  integer mjd                       !Modified Julian Date
  real UT                           !UT!in hours
  real RA,Dec                       !RA and Dec of sun

! NB: Double caps here are single caps in the writeup.

! Orbital elements of the Sun (also N=0, i=0, a=1):
  real w                            !Argument of perihelion
  real e                            !Eccentricity
  real MM                           !Mean anomaly
  real Ls                           !Mean longitude

! Other standard variables:
  real v                            !True anomaly
  real EE                           !Eccentric anomaly
  real ecl                          !Obliquity of the ecliptic
  real d                            !Ephemeris time argument in days
  real r                            !Distance to sun, AU
  real xv,yv                        !x and y coords in ecliptic
  real lonsun                       !Ecliptic long and lat of sun
!Ecliptic coords of sun (geocentric)
  real xs,ys
!Equatorial coords of sun (geocentric)
  real xe,ye,ze
  real lon,lat
  real GMST0,LST,HA
  real xx,yy,zz
  real xhor,yhor,zhor
  real Az,El

  real day
  real rad
  data rad/57.2957795/

! Time in days, with Jan 0, 2000 equal to 0.0:
  d=367*y - 7*(y+(m+9)/12)/4 + 275*m/9 + DD - 730530 + UT/24.0
  mjd=d + 51543
  ecl = 23.4393 - 3.563e-7 * d

! Compute updated orbital elements for Sun:
  w = 282.9404 + 4.70935e-5 * d
  e = 0.016709 - 1.151e-9 * d
  MM = mod(356.0470d0 + 0.9856002585d0 * d + 360000.d0,360.d0)
  Ls = mod(w+MM+720.0,360.0)

  EE = MM + e*rad*sin(MM/rad) * (1.0 + e*cos(M/rad))
  EE = EE - (EE - e*rad*sin(EE/rad)-MM) / (1.0 - e*cos(EE/rad))

  xv = cos(EE/rad) - e
  yv = sqrt(1.0-e*e) * sin(EE/rad)
  v = rad*atan2(yv,xv)
  r = sqrt(xv*xv + yv*yv)
  lonsun = mod(v + w + 720.0,360.0)
! Ecliptic coordinates of sun (rectangular):
  xs = r * cos(lonsun/rad)
  ys = r * sin(lonsun/rad)

! Equatorial coordinates of sun (rectangular):
  xe = xs
  ye = ys * cos(ecl/rad)
  ze = ys * sin(ecl/rad)

! RA and Dec in degrees:
  RA = rad*atan2(ye,xe)
  Dec = rad*atan2(ze,sqrt(xe*xe + ye*ye))

  GMST0 = (Ls + 180.0)/15.0
  LST = mod(GMST0+UT+lon/15.0+48.0,24.0)    !LST in hours
  HA = 15.0*LST - RA                        !HA in degrees
  xx = cos(HA/rad)*cos(Dec/rad)
  yy = sin(HA/rad)*cos(Dec/rad)
  zz =             sin(Dec/rad)
  xhor = xx*sin(lat/rad) - zz*cos(lat/rad)
  yhor = yy
  zhor = xx*cos(lat/rad) + zz*sin(lat/rad)
  Az = mod(rad*atan2(yhor,xhor) + 180.0 + 360.0,360.0)
  El = rad*asin(zhor)
  day=d-1.5
  
  return
end subroutine sun
