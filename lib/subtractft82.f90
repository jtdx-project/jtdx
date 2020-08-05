subroutine subtractft82(itone,f0,dt,swl)

  !$ use omp_lib
  use ft8_mod1, only : dd8,cw,cfilt2,cref2,NFILT1,NFILT2,endcorr,endcorrswl
! NMAX=15*12000,NFFT=15*12000,NFRAME=1920*79
  parameter (NFFT=180000,NMAX=180000,NFRAME=151680)
  integer itone(79)
  logical(1), intent(in) :: swl

  nstart=dt*12000+1
  call gen_ft8wave(itone,79,1920,2.0,12000.0,f0,cref2,xjunk,1,NFRAME)
  do i=1,nframe
    id=nstart-1+i 
    if(id.ge.1.and.id.le.NMAX) then
      cfilt2(i)=dd8(id)*conjg(cref2(i))
    else
      cfilt2(i)=0.0
    endif
  enddo
  cfilt2(nframe+1:)=0.0
  call four2a(cfilt2,nfft,1,-1,1)
  cfilt2(1:nfft)=cfilt2(1:nfft)*cw(1:nfft)
  call four2a(cfilt2,nfft,1,1,1)
  if(.not.swl) then
    cfilt2(1:NFILT1/2+1)=cfilt2(1:NFILT1/2+1)*endcorr
    cfilt2(nframe:nframe-NFILT1/2:-1)=cfilt2(nframe:nframe-NFILT1/2:-1)*endcorr
  else
    cfilt2(1:NFILT2/2+1)=cfilt2(1:NFILT2/2+1)*endcorrswl
    cfilt2(nframe:nframe-NFILT2/2:-1)=cfilt2(nframe:nframe-NFILT2/2:-1)*endcorrswl
  endif
!$omp critical(subtraction)
  do i=1,nframe
     j=nstart+i-1
     if(j.ge.1 .and. j.le.NMAX) dd8(j)=dd8(j)-2*REAL(cfilt2(i)*cref2(i))
  enddo
!$omp end critical(subtraction)
!$OMP FLUSH (dd8)

  return
end subroutine subtractft82
