subroutine partintft8(ndelay,nutc)

  use ft8_mod1, only : dd8
  real rnd

  if(ndelay.gt.120) ndelay=120
  numsamp=nint(float(ndelay)*1200)
  dd8(numsamp+1:180000)=dd8(1:(180000-numsamp))
  do i=1,numsamp
     call random_number(rnd)
     dd8(i)=10.0*rnd-5.
  enddo
  write(*,2) nutc,'partial loss of data','d'
2 format(i6.6,2x,a20,21x,a1)
  call flush(6)

  return
end subroutine partintft8