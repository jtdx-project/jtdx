program mfsk
! Compute probability of symbol error for non-coherent MFSK
  implicit real*16 (a-h,o-z)
  integer*8 binomial
  integer x,s,XX,NN,M
  real*16 hypergeo, snr, term, sum
  character arg*8

  nargs=iargc()
  if(nargs.ne.1) then
     print*,'Probability of symbol error for noncoherent MFSK'
     print*,'Usage:    mfsk   M'
     print*,'Example:  mfsk  64'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) M

  write(*,1012) 
1012 format('Es/No  P(symbol error)'/  &
             '----------------------')
  do isnr=0,40
    esno=10**(isnr/2.0/10.0)
    hsum=0.d0
    do k=1,M-1
      h=binomial(M-1,k)
      h=h*((-1)**(k+1))/(k+1)
      h=h*exp(-esno*k/(k+1))
      hsum=hsum + h
    enddo
    write(*,'(f4.1,4x,e10.4)') isnr/2.0, hsum
  enddo
999 end program mfsk
