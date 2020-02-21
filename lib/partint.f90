! last time modified by Igor UA3DJY on 20200105

subroutine partint(ndelay,nutc)

  use jt65_mod6
  real rnd

  numsamp=ndelay*1200
  dd(numsamp+1:624000)=dd(1:(624000-numsamp))
     do i=1,numsamp
        call random_number(rnd)
        dd(i)=10.0*rnd-5.
     enddo
  write(*,2) nutc,'partial loss of data','d'
2 format(i4.4,2x,a20,21x,a1)
  call flush(6)

  return
end subroutine partint