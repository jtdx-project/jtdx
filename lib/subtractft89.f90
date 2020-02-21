! This source code file was last time modified by Igor UA3DJY on 20190708
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine subtractft89(itone,f0,dt)

  !$ use omp_lib
  use ft8_mod1, only : dd8,cw,cfilt9,cref9
! NMAX=15*12000,NFFT=15*12000,NFRAME=1920*79
  parameter (NFFT=184320,NMAX=180000,NFRAME=151680)
  integer itone(79)

  nstart=dt*12000+1
!  call genft8refsig(itone,cref9,f0)
  call gen_ft8wavesub(itone,f0,cref9)
  do i=1,nframe
    id=nstart-1+i 
    if(id.ge.1.and.id.le.NMAX) then
      cfilt9(i)=dd8(id)*conjg(cref9(i))
    else
      cfilt9(i)=0.0
    endif
  enddo
  cfilt9(nframe+1:)=0.0
  call four2a(cfilt9,nfft,1,-1,1)
  cfilt9(1:nfft)=cfilt9(1:nfft)*cw(1:nfft)
  call four2a(cfilt9,nfft,1,1,1)
!$omp critical(subtraction)
  do i=1,nframe
     j=nstart+i-1
     if(j.ge.1 .and. j.le.NMAX) dd8(j)=dd8(j)-2*REAL(cfilt9(i)*cref9(i))
  enddo
!$omp end critical(subtraction)
!$OMP FLUSH (dd8)
 
  return
end subroutine subtractft89
