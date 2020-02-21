subroutine zplot9(s,freq,drift)

  real s(0:8,85)
  character*1 line(85),mark(0:6)
  data mark/' ',' ','.','-','+','X','$'/
  include 'jt9sync.f90'

  write(32,1000) freq,drift
1000 format('Freq:',f7.1,'   Drift:',f5.1,'  ',60('-'))
  do j=8,0,-1
     do i=1,85
        n=(s(j,i))
        if(n.lt.0) n=0
        if(n.gt.6) n=6
        line(i)=mark(n)
     enddo
     write(32,1010) j,line
1010 format(i1,1x,85a1)
  enddo
  do i=1,85
     line(i)=' '
     if(isync(i).eq.1) line(i)='@'
  enddo
  write(32,1015)
1015 format(87('-'))
  write(32,1020) line
1020 format(2x,85a1)
  call flush(32)

  return
end subroutine zplot9
