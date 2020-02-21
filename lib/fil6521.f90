! This source code file was last time modified by Igor Chernikov UA3DJY on September 21st, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.


subroutine fil6521(c1,n1,c3,n2)

  integer, parameter :: NTAPS=21,NDOWN=8
  integer, parameter :: NH=NTAPS/2
  complex c1(n1),c2(n1/NDOWN),c3(n1/NDOWN)

! Filter coefficients:
  real b(-NH:NH)
! Kaiser-Bessel FIR LPF  http://www.arc.id.au/FilterDesign.html
! fsample    (Hz)  172.265625   Input sample rate
! Ntaps            21         Number of filter taps
! fc         (Hz)  1          Cutoff frequency
! dF/2       (Hz)  25         Half transition band
! Stop Atten (dB)  90         Stopband attenuation
! fout       (Hz)  172.265625    Output sample rate  
data b/0.000011, 0.000113, 0.000416, 0.001058, 0.002158, 0.003751, 0.005737, &
0.007870, 0.009797, 0.011144, 0.011628, 0.011144, 0.009797, 0.007870, &
0.005737, 0.003751, 0.002158, 0.001058, 0.000416, 0.000113, 0.000011/

  n2=n1/NDOWN
  k0=NDOWN/2
  c2=0.
! Loop over all output samples
  do i=1,n2
     k=k0 + NDOWN*i
        c2(i)=c1(k)
  enddo

  c3=0.
  do i=1,n2
    do j=-NH,NH
    if(i.le.NH .or. i.gt.(n2-NH)) cycle
      c3(i)=c3(i)+c2(i+j)*b(j)
    enddo
  enddo
  c3=c3*540.0
  return
end subroutine fil6521
