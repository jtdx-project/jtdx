! This source code file was last time modified by Igor Chernikov UA3DJY on May 25th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine pctile(x,npts,npct,xpct)

  parameter (NMAX=32768)
  real*4 x(npts)
  real*4 tmp(NMAX)
  real*4 xpct
  
  if(npts.le.0) then
     xpct=1.0
     go to 900
  endif
  if(npts.gt.NMAX) stop

  tmp(1:npts)=x
  call shell(npts,tmp)
  j=nint(npts*0.01*npct)
  if(j.lt.1) j=1
  if(j.gt.npts) j=npts
  xpct=tmp(j)

900 continue
  return
end subroutine pctile
