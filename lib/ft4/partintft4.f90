! last time modified by Igor UA3DJY on 20200105

subroutine partintft4(ndelay,nutc)

  use ft4_mod1, only : dd4
  real rnd

  if(ndelay.gt.50) ndelay=50
  numsamp=nint((float(ndelay))*1200) ! 12000 sample rate
  dd4(numsamp+1:73728)=dd4(1:(73728-numsamp))
  do i=1,numsamp
     call random_number(rnd)
     dd4(i)=10.0*rnd-5.
  enddo
  write(*,2) nutc,'partial loss of data','d'
2 format(i6.6,2x,a20,21x,a1)
  call flush(6)

  return
end subroutine partintft4