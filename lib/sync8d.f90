subroutine sync8d(cd0,i0,ctwk,itwk,sync,imainpass,lastsync,iqso,lcq,lcallsstd,lcqcand)

! Compute sync power for a complex, downsampled FT8 signal.
  use ft8_mod1, only : csync,csynce,csyncsd,csyncsdcq,csynccq

  parameter(NP2=3199,NDOWN=60)
  complex, intent(in) :: cd0(0:NP2),ctwk(32)
  complex csync1(0:18,32),csync2(32)
  complex ctmp(0:31)
  complex z1,z2,z3,z4,zt1(0:6),zt2(0:6),zt3(0:6),zt4(0:7),zt5(0:18)
  integer, intent(in) :: i0,imainpass,iqso
  logical(1), intent(in) :: lcq,lcallsstd,lcqcand
  logical(1) lastsync

  sync=0.; ctmp=cmplx(0.0,0.0); zt1=0.; zt2=0.; zt3=0.
  do i=0,6 ! Sum over 7 Costas frequencies and three Costas arrays
    ctmp=cmplx(0.0,0.0)
    i1=i0+i*32; i2=i1+36*32; i3=i1+72*32
    csync2=csync(i,1:32)
    if(itwk.eq.1) csync2=ctwk*csync2      !Tweak the frequency
    if(i1.lt.0 .and. i1.gt.-32) then
      ibot=abs(i1)-1; itop=30-ibot; ctmp(0:ibot)=0.; ctmp(ibot+1:31)=cd0(0:itop)
    endif
    if(i3+31.gt.NP2 .and. i3.le.NP2) then
      ibot=NP2-i3; ctmp(0:ibot)=cd0(i3:NP2); ctmp(ibot+1:31)=0.
    endif
    if(i1+31.le.NP2) then
      if(i1.ge.0) then; zt1(i)=sum(cd0(i1:i1+31)*conjg(csync2)); else; zt1(i)=sum(ctmp*conjg(csync2)); endif
    endif
    if(i2.ge.0 .and. i2+31.le.NP2) zt2(i)=sum(cd0(i2:i2+31)*conjg(csync2))
    if(i3.ge.0) then
      if(i3+31.le.NP2) then; zt3(i)=sum(cd0(i3:i3+31)*conjg(csync2)); else; zt3(i)=sum(ctmp*conjg(csync2)); endif
    endif
  enddo
  do i=0,6
    if(imainpass.eq.3 .or. imainpass.eq.4 .or. imainpass.eq.8)  then
      if(i.lt.6) then
        z1=(zt1(i)+zt1(i+1))/2; z2=(zt2(i)+zt2(i+1))/2; z3=(zt3(i)+zt3(i+1))/2 ! use i=5 z values for i=6
      endif
    else
      z1=zt1(i); z2=zt2(i); z3=zt3(i)
    endif
    if(.not.lastsync) then
      if(imainpass.eq.1 .or. imainpass.eq.5 .or. imainpass.eq.9) then; sync = sync + SQRT(real(z1)**2 + aimag(z1)**2) &
         + SQRT(real(z2)**2 + aimag(z2)**2) + SQRT(real(z3)**2 + aimag(z3)**2)
      else if(imainpass.eq.2 .or. imainpass.eq.6 .or. imainpass.eq.7) then; sync = sync + real(z1)**2 + aimag(z1)**2 &
         + real(z2)**2 + aimag(z2)**2 + real(z3)**2 + aimag(z3)**2
      else if(imainpass.eq.3 .or. imainpass.eq.4 .or. imainpass.eq.8)  then; sync = sync + abs(real(z1)) + abs(aimag(z1)) &
         + abs(real(z2)) + abs(aimag(z2)) + abs(real(z3)) + abs(aimag(z3))
      endif
    else
      sync = sync + real(z1)**2 + aimag(z1)**2 + real(z2)**2 + aimag(z2)**2 + real(z3)**2 + aimag(z3)**2
    endif
  enddo

  if(itwk.eq.1 .and. lcqcand .and. iqso.eq.1) then
    zt4=0.
    do i=0,7
      ctmp=cmplx(0.0,0.0)
      csync2=csynccq(i,1:32); csync2=ctwk*csync2      !Tweak the frequency
      i4=i0+(i+7)*32
      if(i4.lt.0 .and. i4.gt.-32) then
        ibot=abs(i4)-1; itop=30-ibot; ctmp(0:ibot)=0.; ctmp(ibot+1:31)=cd0(0:itop)
      endif
      if(i4+31.le.NP2) then
        if(i4.ge.0) then; zt4(i)=sum(cd0(i4:i4+31)*conjg(csync2))
        else; zt4(i)=sum(ctmp*conjg(csync2)); endif
      endif
    enddo
    do i=0,7
      if(imainpass.eq.3 .or. imainpass.eq.4 .or. imainpass.eq.8)  then
        if(i.lt.7) z4=(zt4(i)+zt4(i+1))/2 ! use i=6 z4 value for i=7
      else
       z4=zt4(i)
      endif
      if(imainpass.eq.1 .or. imainpass.eq.5 .or. imainpass.eq.9) then; sync = sync + SQRT(real(z4)**2 + aimag(z4)**2)
      else if(imainpass.eq.2 .or. imainpass.eq.6 .or. imainpass.eq.7) then; sync = sync + real(z4)**2 + aimag(z4)**2
      else if(imainpass.eq.3 .or. imainpass.eq.4 .or. imainpass.eq.8) then; sync = sync + abs(real(z4)) + abs(aimag(z4))
      endif
    enddo
  endif

  if(.not.lastsync) then
    if(iqso.gt.1 .and. iqso.lt.4 .and. lcallsstd) csync1=csynce; if(iqso.eq.4 .and. .not.lcq) csync1=csyncsd

    if((iqso.eq.4 .and. .not.lcq) .or. ((iqso.eq.2 .or. iqso.eq.3) .and. lcallsstd)) then
      ctmp=cmplx(0.0,0.0); zt5=0.
      do i=0,18
        csync2=csync1(i,1:32)
        if(itwk.eq.1) csync2=ctwk*csync2      !Tweak the frequency
        i4=i0+(i+7)*32
        if(i4.lt.0 .and. i4.gt.-32) then
          ibot=abs(i4)-1; itop=30-ibot; ctmp(0:ibot)=0.; ctmp(ibot+1:31)=cd0(0:itop)
        endif
        if(i4+31.le.NP2) then
          if(i4.ge.0) then; zt5(i)=sum(cd0(i4:i4+31)*conjg(csync2))
          else; zt5(i)=sum(ctmp*conjg(csync2)); endif
        endif
      enddo
      do i=0,18
        if(imainpass.eq.2 .or. imainpass.eq.6 .or. imainpass.eq.7)  then
          if(i.lt.18) z4=(zt5(i)+zt5(i+1))/2 ! use i=17 z4 value for i=18
        else
         z4=zt5(i)
        endif
        if(imainpass.eq.1 .or. imainpass.eq.5 .or. imainpass.eq.9) then; sync = sync + SQRT(real(z4)**2 + aimag(z4)**2)
        else if(imainpass.eq.2 .or. imainpass.eq.6 .or. imainpass.eq.7) then; sync = sync + real(z4)**2 + aimag(z4)**2
        else if(imainpass.eq.3 .or. imainpass.eq.4 .or. imainpass.eq.8) then; sync = sync + abs(real(z4)) + abs(aimag(z4))
        endif
      enddo
    endif

    if(iqso.eq.4 .and. lcq) then
      ctmp=cmplx(0.0,0.0)
      do i=0,57
        csync2=csyncsdcq(i,1:32)
        if(itwk.eq.1) csync2=ctwk*csync2      !Tweak the frequency
        z4=0.; if(i.lt.29) then; k=i+7; else; k=i+14; endif
        i4=i0+k*32
        if(i4.lt.0 .and. i4.gt.-32) then
          ibot=abs(i4)-1; itop=30-ibot; ctmp(0:ibot)=0.; ctmp(ibot+1:31)=cd0(0:itop)
        endif
        if(i4+31.le.NP2) then
          if(i4.ge.0) then; z4=sum(cd0(i4:i4+31)*conjg(csync2))
          else; z4=sum(ctmp*conjg(csync2)); endif
        endif
        if(imainpass.eq.1 .or. imainpass.eq.5 .or. imainpass.eq.9) then; sync = sync + SQRT(real(z4)**2 + aimag(z4)**2)
          else if(imainpass.eq.2 .or. imainpass.eq.6 .or. imainpass.eq.7) then; sync = sync + real(z4)**2 + aimag(z4)**2
          else if(imainpass.eq.3 .or. imainpass.eq.4 .or. imainpass.eq.8) then; sync = sync + abs(real(z4)) + abs(aimag(z4))
        endif
      enddo
    endif
  endif

  return
end subroutine sync8d
