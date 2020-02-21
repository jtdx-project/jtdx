! This source code file was last time modified by Igor UA3DJY on April 29th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine chkss10(ss2,freq,drift,schk,qualf,syncro)

  real ss2(0:9,85)
  real s(0:9,85)
  include 'jt10sync.f90'

  ave=sum(ss2)/(10*85)
  if(freq+drift.eq.-999999.0) ave=1.0 ! 0.      !To silence compiler warning
  s=ss2/ave !-1.0

  s1=0.
  do i=1,16
     j=ii(i)
     if(j.le.85) then
        if(lsync(i)) s1=s1 + s(0,j)
        if(.not.lsync(i)) s1=s1 + s(1,j)
     endif
  enddo
  schk=s1/16.0

  ssig1=0.; ssig2=0.; snoise=0.; syncro=0.
  do i=1,85
     if(isync(i).eq.0) snoise=snoise+ss2(0,i)+ss2(1,i)
     if(isync(i).eq.1) then; ssig1=ssig1+ss2(0,i); snoise=snoise+ss2(1,i); endif
     if(isync(i).eq.2) then; ssig2=ssig2+ss2(1,i); snoise=snoise+ss2(0,i); endif
  enddo
  qualf=9.625*(ssig1+ssig2)/snoise !(170-16)/16=9.625  sync/noise tone ratio
  syncro=ssig1/ssig2

!print *,schk,qualf

  return
end subroutine chkss10

