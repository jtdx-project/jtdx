! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine genft8refsig(itone,cref,f0)

  use ft8_mod1, only :twopi,dt
  complex cref(151680) !NFRAME=1920*79
  integer itone(79)
!  real*8 twopi,phi,dphi,dt,xnsps
  real phi,dphi,xnsps,dtf0

  xnsps=1920.d0
  phi=0.d0
  k=1
  dtf0=f0*dt
  do i=1,79
    dphi=twopi*(dtf0+itone(i)/xnsps)
    do is=1,1920
      cref(k)=cmplx(cos(phi),sin(phi))
      phi=mod(phi+dphi,twopi)
      k=k+1
    enddo
  enddo
  return
end subroutine genft8refsig
