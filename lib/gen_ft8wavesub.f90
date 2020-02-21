! This source code file was last time modified by Igor UA3DJY on 20190708
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine gen_ft8wavesub(itone,f0,cwave)

  use ft8_mod1, only : twopi,dt

  parameter(NSYM=79,NSPS=1920,NFRAME=151680)
  complex cwave(nframe)
  real pulse(23040)
  real dphi(0:155519) ! (0:(nsym+2)*nsps-1)
  integer itone(79)
  data ibt0/0/
  save pulse,hmod,ibt0
  
  bt=4.0 ! Temporary compromise?
  ibt=nint(10*bt)
  if(ibt0.ne.ibt) then
     hmod=1.0
! Compute the frequency-smoothing pulse
     do i=1,5760 ! 3*nsps
        tt=(i-2880)/real(nsps) ! (i-1.5*nsps)
        pulse(i)=gfsk_pulse(bt,tt)
     enddo
     ibt0=nint(10*bt)
  endif

! Compute the smoothed frequency waveform.
! Length = (nsym+2)*nsps samples, first and last symbols extended 
  dphi_peak=twopi*hmod/real(nsps)
  dphi=0.0 
  do j=1,nsym       
     ib=(j-1)*nsps
     ie=ib+5759 ! +3*nsps-1
     dphi(ib:ie) = dphi(ib:ie) + dphi_peak*pulse(1:5760)*itone(j) ! pulse(1:3*nsps)
  enddo
! Add dummy symbols at beginning and end with tone values equal to 1st and last symbol, respectively
!  dphi(0:2*nsps-1)=dphi(0:2*nsps-1)+dphi_peak*itone(1)*pulse(nsps+1:3*nsps)
  dphi(0:3839)=dphi(0:3839)+dphi_peak*itone(1)*pulse(1921:5760)

!  dphi(nsym*nsps:(nsym+2)*nsps-1)=dphi(nsym*nsps:(nsym+2)*nsps-1)+dphi_peak*itone(nsym)*pulse(1:2*nsps)
  dphi(nframe:155519)=dphi(nframe:155519)+dphi_peak*itone(nsym)*pulse(1:3840)

! Calculate and insert the audio waveform
  phi=0.0
  dphi = dphi + twopi*f0*dt                          !Shift frequency up by f0
  cwave=0. ! avoid writing to memory we may not have access to
  k=0
  do j=nsps,153599 ! nsps+ nframe(151680) -1   Don't include dummy symbols
    k=k+1
    cwave(k)=cmplx(cos(phi),sin(phi))
    phi=mod(phi+dphi(j),twopi)
  enddo

! Apply envelope shaping to the first and last symbols
  nramp=240 ! =nint(nsps/8.0)
  cwave(1:nramp)=cwave(1:nramp) * (1.0-cos(twopi*(/(i,i=0,nramp-1)/)/(2.0*nramp)))/2.0
!  k1=nsym*nsps-nramp+1 == 151441
!  cwave(k1:k1+nramp-1)=cwave(k1:k1+nramp-1) * (1.0+cos(twopi*(/(i,i=0,nramp-1)/)/(2.0*nramp)))/2.0
  cwave(151441:nframe)=cwave(151441:nframe) * (1.0+cos(twopi*(/(i,i=0,nramp-1)/)/(2.0*nramp)))/2.0

  return
end subroutine gen_ft8wavesub
