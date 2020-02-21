program bodide
! Compute probability of word error for a bounded distance decoder.
! Hardwired for non-coherent 64-FSK and the JT65 RS (63,12) code on GF(64).
!
! Let ps be symbol error probability. 
! The probability of getting an error pattern with e symbol errors is:
! ps^e * (1-ps)*(n-e)
! The number of error patterns with e errors is binomial(63,e)
! Overall probability of getting a word with e errors is:
! P(e)= binomial(63,e)* ps^e * (1-ps)*(n-e)
! Probability that word is correct is P(0 to 25 errors) = sum{e=0}^{25} P(e)
! Probability that word is wrong is 1-P(0 to 25 errors) 
! P_word_error=1-( sum_{e=0}^{t} P(e) )
!
  implicit real*16 (a-h,o-z)

  integer*8 binomial
  integer x,s,XX,NN,M
  character arg*8

  nargs=iargc()
  if(nargs.ne.1) then
     print*,'Probability of word error for noncoherent 64-FSK with bounded distance decoding'
     print*,'Usage:    bounded_distance   D'
     print*,'Example:  bounded_distance  25'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) nt 
  M=64
  write(*,1012) 
1012 format('Es/No  P(word error)'/  &
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
    ps=hsum
    hsum=0.d0
    do i=0,nt
      h=binomial(63,i)
      h=h*ps**i
      h=h*(1-ps)**(63-i)
      hsum=hsum+h
    enddo
    pw=1-hsum
    write(*,'(f4.1,4x,e10.4,4x,e10.4)') isnr/2.0, ps, pw
  enddo
999 end program bodide 
