! This source code file was last time modified by Igor Chernikov UA3DJY on July 12th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine slope(y,npts,xpk)

! Remove best-fit slope from data in y(i).  When fitting the straight line,
! ignore the peak around xpk +/- 2.
  implicit none
  integer i
  integer npts
  real y(npts)
  real xpk,sumw,sumx,sumy,sumx2,sumxy,sumy2,x,a,b,delta,sq,rms
  
  sumw=0.; sumx=0.; sumy=0.; sumx2=0.; sumxy=0.; sumy2=0.
  
  do i=1,npts
     if(abs(i-xpk).gt.2.0) then
        sumw=sumw + 1.0
        x=i
        sumx=sumx + x
        sumy=sumy + y(i)
        sumx2=sumx2 + x*x
        sumxy=sumxy + x*y(i)
        sumy2=sumy2 + y(i)**2
     endif
  enddo

  delta=sumw*sumx2 - sumx**2
  a=(sumx2*sumy - sumx*sumxy) / delta
  b=(sumw*sumxy - sumx*sumy) / delta

  sq=0.
  do i=1,npts
     y(i)=y(i)-(a + b*i)
     if(abs(i-xpk).gt.2.0) sq=sq + y(i)**2
  enddo
  rms=sqrt(sq/(sumw-2.0))
  y=y/rms

  return
end subroutine slope

