! This source code file was last time modified by Igor Chernikov UA3DJY on October 03rd, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine twkfreq65(cx,n5,a)

  complex cx(n5)
  real a(2)
  complex w,wstep
  data twopi/6.283185307/

! Apply AFC corrections to the cx data
  w=1.0
  wstep=1.0
  x0=0.5*n5
  do i=1,n5
     if(mod(i,100).eq.1) then
        x=(i-x0)/float(n5)
        dphi=(a(1) + x*a(2)) * (twopi/1378.125)
        wstep=cmplx(cos(dphi),sin(dphi))
     endif
     w=w*wstep
     cx(i)=w*cx(i)
  enddo
  return
end subroutine twkfreq65
