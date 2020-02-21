subroutine shell(n,a)
  integer n
  real a(n)
  integer i,j,inc
  real v

  inc=1
1 inc=3*inc+1
  if(inc.le.n) go to 1
2  inc=inc/3

  do i=inc+1,n
     v=a(i)
     j=i
3    if(a(j-inc).gt.v) then
        a(j)=a(j-inc)
        j=j-inc
        if(j.le.inc) go to 4
        go to 3
     endif
4    a(j)=v
  enddo

  if(inc.gt.1) go to 2

  return
end subroutine shell
