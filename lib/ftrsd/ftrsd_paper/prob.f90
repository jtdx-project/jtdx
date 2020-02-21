program prob

  implicit real*8 (a-h,o-z)
  integer*8 binomial
  integer x,s,XX,NN
  real*8 hypergeo
  character arg*8

  nargs=iargc()
  if(nargs.ne.4) then
     print*,'Usage:    prob  x  N  X  s'
     print*,'Example:  prob 35 63 40 40'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) x
  call getarg(2,arg)
  read(arg,*) NN
  call getarg(3,arg)
  read(arg,*) XX
  call getarg(4,arg)
  read(arg,*) s

!  print*,binomial(5, 3)             ! 10
!  print*,binomial(40, 19)           ! 131282408400

  write(*,1010) x,NN,XX,s
1010 format(//' x=',i2,'   N=',i2,'   X=',i2,'   s=',i2)
  write(*,1012) 
1012 format(/' x    P(x|N,X,s)  P(>=x|N,X,s) '/  &
             '-------------------------------')

  hsum=0.d0
  do ix=x,XX
     h=hypergeo(ix,NN,XX,s)
     hsum=hsum + h
     write(*,1020) ix,h,hsum
1020 format(i3,2d13.4)
  enddo

999 end program prob
