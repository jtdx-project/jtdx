! This source code file was last time modified by Igor Chernikov UA3DJY on July 07th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine peakup(ym,y0,yp,dx)

real*4 b,c,dx,yp,ym,y0

  b=yp-ym
  c=yp+ym-2*y0
  dx=-((b/c)/2)

  return
end subroutine peakup
