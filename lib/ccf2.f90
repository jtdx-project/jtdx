! This source code file was last time modified by Igor UA3DJY on December 28th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine ccf2(ss,nz,ccfbest,xlagpk,lagpk)

  use jt65_mod4 ! prc(i)
!  parameter (LAGMIN=-86,LAGMAX=324)
  parameter (LAGMIN=-76,LAGMAX=382)

  real ss(nz)
  real ccf(LAGMIN:LAGMAX)

  ccfbest=0.;lagpk=0

  do lag=LAGMIN,LAGMAX
     s0=0.
     s1=0.
     do i=1,126
        j=16*(i-1)+1 + lag
        if(j.ge.1 .and. j.le.nz-8) then
           x=ss(j)
           if(.not.prc(i)) then
              s0=s0 + x
           else
              s1=s1 + x
           endif
        endif
     enddo
     ccf(lag)=s1-s0
     if(ccf(lag).gt.ccfbest) then
        ccfbest=ccf(lag)
        lagpk=lag
        xlagpk=lagpk
     endif
  enddo
  if(lagpk.gt.LAGMIN .and. lagpk.lt.LAGMAX) then
     call peakup(ccf(lagpk-1),ccf(lagpk),ccf(lagpk+1),dx)
!     if(1.0-abs(ccf(lagpk-1)/ccf(lagpk+1)).gt.0.08 .and. ccf(lagpk-1).lt.ccf(lagpk+1)) dx=0.08
!     if(1.0-abs(ccf(lagpk-1)/ccf(lagpk+1)).gt.0.08 .and. ccf(lagpk-1).gt.ccf(lagpk+1)) dx=-0.08
     xlagpk=lagpk+dx
  endif

  return
end subroutine ccf2
