! This source code file was last time modified by Igor UA3DJY on January 12th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine xcor(ss,ss22,ipk,ccf,lagpk,ccf22,lagpk22)

! Computes ccf of a single row of ss and the pseudo-random array pr.  Returns
! peak of the CCF and the lag at which peak occurs.  

  use jt65_mod4
  implicit none
  integer, parameter :: NSZ=3413,NHSYM=276,lag1=-21,lag2=75
  ! 3413 Max length of power spectra
  ! 276 number of half-symbol steps
  ! 2d spectrum, stepped by half-symbols
  real ccf(lag1:lag2),ccf22(lag1:lag2)
  real x,x22,ccfmax,ccfmax22,s0,s1,s20,s21
  integer j,lag,i,lagpk,lagpk22,ipk
  save
  real ss(:,:),ss22(:,:)
  ccfmax=0.; ccfmax22=0.; lagpk=75; lagpk22=75
! nsteps=nhsym=276.2566 =(52*12000-8192)/(2048*12000/11025) calculated in symspec65

  do lag=lag1,lag2
     s0=0.;s1=0.;s20=0.;s21=0.;x=0.;x22=0.
     do i=1,126 !number of symbols
        j=2*i-1+lag
        if(j.ge.1 .and. j.le.nhsym) then
          x=ss(j,ipk)
          x22=ss22(j,ipk)
          if(.not.prc(i)) then
             s0=s0 + x
             s20=s20 + x22
          else
             s1=s1 + x
             s21=s21 + x22
          endif
        endif
     enddo
     ccf(lag)=2*(s1-s0)                        !The 2 is for plotting scale
     ccf22(lag)=2*(s21-s20)
     if(ccf(lag).gt.ccfmax) then
        ccfmax=ccf(lag)
        lagpk=lag
     endif
     if(ccf22(lag).gt.ccfmax22) then
        ccfmax22=ccf22(lag)
        lagpk22=lag
     endif
  enddo

  return
end subroutine xcor
