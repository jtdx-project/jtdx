! This source code file was last time modified by Igor UA3DJY on March 9th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

logical function baddata()

  use jt65_mod6

  ninterval=1200
  j=0
  smin=1.e30
  smax=-smin
  iz=49*12000/ninterval
  nadd=ninterval/60

 ! 3457-4800 range: cover broken data being observed under Windows XP
  if(abs(dd(4200)).lt.0.1) dd(4200)=1.0

  do i=1,iz
     sq=0.
     do n=1,ninterval,60
        j=j+60
        x=dd(j)
        sq=sq + x*x
     enddo
     rms=sqrt(sq/nadd)
     smin=min(smin,rms)
     smax=max(smax,rms)
  enddo

  sratio=smax/(smin+1.e-30)
  baddata=.false.
  if(sratio.gt.1.e30) baddata=.true.

  return
end function baddata
