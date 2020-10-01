subroutine twkfreq1(ca,nbot,npts,ntop,fsample,a,cb)

  use ft8_mod1, only : twopi
  complex, intent(in) :: ca(nbot:ntop)
  complex, intent(out) :: cb(nbot:ntop)
  complex w,wstep
  real a(5)

! Mix the complex signal
  w=1.0; wstep=1.0
!!  x0=0.5*(npts+1)
!!  s=2.0/npts
  do i=0,npts
!!     x=s*(i-x0) ! valid for 'do i=1,npts'
!!     p2=1.5*x*x - 0.5
!!     p3=2.5*(x**3) - 1.5*x
!!     p4=4.375*(x**4) - 3.75*(x**2) + 0.375
!!     dphi=(a(1) + x*a(2) + p2*a(3) + p3*a(4) + p4*a(5)) * (twopi/fsample)
     dphi=a(1) * (twopi/fsample)
     wstep=cmplx(cos(dphi),sin(dphi))
     w=w*wstep
     cb(i)=w*ca(i)
  enddo

  return
end subroutine twkfreq1
