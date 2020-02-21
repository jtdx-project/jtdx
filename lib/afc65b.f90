! This source code file was last time modified by Igor UA3DJY on December 28th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine afc65b(c5x,npts,fsample,a,ccfbest,dtbest)

! Find delta f, f1, f2 ==> a(1:2)
  complex c5x(npts)
  real a(2),deltaa(2)

  a(1)=0.
  a(2)=0.
  deltaa(1)=2.0
  deltaa(2)=2.0
  nterms=2                                  !number of polynom coefficients
  
!  Start the iteration
  chisqr=0.
  chisqr0=1.e6
  do iter=1,4                               !One iteration is enough?
     do j=1,nterms
        chisq1=fchisq65(c5x,npts,fsample,a,ccfmax,dtmax)
        delta=deltaa(j)/float(iter)
10      a(j)=a(j)+delta
        chisq2=fchisq65(c5x,npts,fsample,a,ccfmax,dtmax)
        if(chisq2.eq.chisq1) go to 10
        if(chisq2.gt.chisq1) then
           delta=-delta                      !Reverse direction
           a(j)=a(j)+delta
           tmp=chisq1
           chisq1=chisq2
           chisq2=tmp
        endif
20      a(j)=a(j)+delta
        chisq3=fchisq65(c5x,npts,fsample,a,ccfmax,dtmax)
        if(chisq3.eq.chisq2) go to 20
        if(chisq3.lt.chisq2) then
           chisq1=chisq2
           chisq2=chisq3
           go to 20
        endif

! Find minimum of parabola defined by last three points
        delta=delta*(1./(1.+(chisq1-chisq2)/(chisq3-chisq2))+0.5)
        a(j)=a(j)-delta
     enddo
     chisqr=fchisq65(c5x,npts,fsample,a,ccfmax,dtmax)
     if(chisqr/chisqr0.gt.0.9999) go to 30
     chisqr0=chisqr
  enddo

30 ccfbest=1.e4*ccfmax*(1378.125/fsample)**2
  dtbest=dtmax

  return
end subroutine afc65b
