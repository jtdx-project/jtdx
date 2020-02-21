subroutine chkss2(ss2,freq,drift,schk)

  real ss2(0:8,85)
  real s(0:8,85)
  include 'jt9sync.f90'

  ave=sum(ss2)/(9*85)
  if(freq+drift.eq.-999999.0) ave=0.      !To silence compiler warning
  s=ss2/ave-1.0

  s1=0.
  do i=1,16
     j=ii(i)
     if(j.le.85) s1=s1 + s(0,j)
  enddo
  schk=s1/16.0

  return
end subroutine chkss2

