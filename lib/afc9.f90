subroutine afc9(c3a,npts,fsample,a,syncpk)

  parameter (NZ2=1512)
  complex c3a(0:NZ2-1)
  complex c3(0:NZ2-1)
  real a(3),deltaa(3)

  a(1)=0.                                   !f0
  a(2)=0.                                   !f1
  a(3)=0.                                   !f2
  deltaa(1)=1.736
  deltaa(2)=1.736
  deltaa(3)=1.0
  nterms=3

! Start the iteration
  chisqr=0.
  chisqr0=1.e6
  c3=c3a
  a3=a(3)
  do iter=1,4
     do j=1,nterms
        if(a(3).ne.a3) call shft(c3a,a(3),a3,c3)
        chisq1=fchisq(c3,npts,fsample,a)
        fn=0.
        delta=deltaa(j)
10      a(j)=a(j)+delta
        if(a(3).ne.a3) call shft(c3a,a(3),a3,c3)
        chisq2=fchisq(c3,npts,fsample,a)
        if(chisq2.eq.chisq1) go to 10
        if(chisq2.gt.chisq1) then
           delta=-delta                      !Reverse direction
           a(j)=a(j)+delta
           tmp=chisq1
           chisq1=chisq2
           chisq2=tmp
        endif
20      fn=fn+1.0
        a(j)=a(j)+delta
        if(a(3).ne.a3) call shft(c3a,a(3),a3,c3)
        chisq3=fchisq(c3,npts,fsample,a)
        if(chisq3.lt.chisq2) then
           chisq1=chisq2
           chisq2=chisq3
           go to 20
        endif

! Find minimum of parabola defined by last three points
        delta=delta*(1./(1.+(chisq1-chisq2)/(chisq3-chisq2))+0.5)
        a(j)=a(j)-delta
        if(j.lt.3) deltaa(j)=deltaa(j)*fn/3.
!        write(*,4000) iter,j,a,-chisq2
!4000    format(i1,i2,3f10.4,f11.3)
     enddo
     if(a(3).ne.a3) call shft(c3a,a(3),a3,c3)
     chisqr=fchisq(c3,npts,fsample,a)
!     write(*,4000) 0,0,a,-chisqr
     if(chisqr/chisqr0.gt.0.99) exit
     chisqr0=chisqr
  enddo

  syncpk=-chisqr
  c3a=c3
!  write(*,4001) a,syncpk
!4001 format(3x,3f10.4,f11.3)

  return
end subroutine afc9

subroutine shft(c3a,a3a,a3,c3)
  complex c3a(0:1359)
  complex  c3(0:1359)

  a3=a3a
  n=nint(a3)
  c3=cshift(c3a,n)
  if(n.gt.0) c3(1360-n:)=0.0
  if(n.lt.0) c3(:n-1)=0.0

  return
end subroutine shft
