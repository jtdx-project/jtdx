subroutine fil4(id1,n1,id2,n2,dd2)

  parameter (NTAPS=15)
  parameter (NDOWN=4)             !Downsample ratio
  integer*2 id1(n1)
  integer*2 id2(*)
  real dd2(*)
  real t(NTAPS)
  data t/NTAPS*0.0/
  real w(NTAPS)
! Filter coefficients:
! Kaiser-Bessel FIR LPF  http://www.arc.id.au/FilterDesign.html
! fsample     = 48000 Hz
! Ntaps       = 15
! fc          = 6000  Hz
! df/2        = 2626  Hz
! Stop Atten  = 30    dB
! fout        = 12000 Hz

  data w/-0.012989, -0.028287, -0.029614, 0.000000, 0.065033,  0.149500,  0.221615, &
          0.250000,  0.221615,  0.149500, 0.065033, 0.000000, -0.029614, -0.028287, -0.012989 /
  save w,t

  n2=n1/NDOWN ! n1=13824 n2=3456
  if(n2*NDOWN.ne.n1) stop 'Error in fil4'
  k=1-NDOWN
  do i=1,n2
     k=k+NDOWN
     t(1:11)=t(5:15)     !Shift old data down t(1:NTAPS-NDOWN)=t(1+NDOWN:NTAPS)
     t(12:15)=id1(k:k+3) !Insert new data at end of t, t(1+NTAPS-NDOWN:NTAPS)=id1(k:k+NDOWN-1)
     dd2(i)=dot_product(w,t)
     id2(i)=nint(dd2(i))
  enddo

  return
end subroutine fil4
