subroutine sync8d(cd0,i0,ctwk,itwk,sync,ipass,lastsync,iqso,lcq,lcallsstd,lcqcand)

! Compute sync power for a complex, downsampled FT8 signal.
  use ft8_mod1, only : csync,csynce,csyncsd,csyncsdcq,csynccq

  complex, intent(in) :: cd0(-800:4000),ctwk(32)
  complex csync1(0:18,32),csync2(32),z1,z2,z3,z4,zt1(0:6),zt2(0:6),zt3(0:6),zt4(0:7),zt5(0:18), &
          z11(224),z22(224),z33(224)
  real sync1(0:20)!,sync2(0:20)
  integer, intent(in) :: i0,ipass,iqso
  logical(1), intent(in) :: lcq,lcallsstd,lcqcand
  logical(1) lastsync

  sync=0.; sync1=0.; zt1=0.; zt2=0.; zt3=0.; z11=0.; z22=0.; z33=0.!; sync2=0.
  k=1
  do i=0,6 ! Sum over 7 Costas frequencies and three Costas arrays
    i1=i0+i*32; i2=i1+1152; i3=i1+2304 ! +36*32, +72*32
    csync2=csync(i,1:32)
    if(itwk.eq.1) csync2=ctwk*csync2      !Tweak the frequency
    z11(k:k+31)=cd0(i1:i1+31)*conjg(csync2);  z22(k:k+31)=cd0(i2:i2+31)*conjg(csync2)
    z33(k:k+31)=cd0(i3:i3+31)*conjg(csync2)
    zt1(i)=sum(z11(k:k+31)); zt2(i)=sum(z22(k:k+31)); zt3(i)=sum(z33(k:k+31))
    k=k+32
  enddo
  if(ipass.eq.1 .or. ipass.eq.5 .or. ipass.eq.9) then
    if(.not.lastsync) then
      do i=0,6
        sync1(i) = SQRT(real(zt1(i))**2 + aimag(zt1(i))**2)
        sync1(i+7) = SQRT(real(zt2(i))**2 + aimag(zt2(i))**2)
        sync1(i+14) = SQRT(real(zt3(i))**2 + aimag(zt3(i))**2)
      enddo
      sync=sum(sync1)
    else
      do i=0,6
        sync = sync + real(zt1(i))**2 + aimag(zt1(i))**2 + real(zt2(i))**2 + aimag(zt2(i))**2 + real(zt3(i))**2 + &
          aimag(zt3(i))**2
      enddo
    endif
  else if(ipass.eq.2 .or. ipass.eq.6 .or. ipass.eq.7) then
    if(.not.lastsync) then
      do i=0,6
        sync1(i) = real(zt1(i))**2 + aimag(zt1(i))**2
        sync1(i+7) = real(zt2(i))**2 + aimag(zt2(i))**2
        sync1(i+14) = real(zt3(i))**2 + aimag(zt3(i))**2
      enddo
      sync=sum(sync1)
    else
      do i=0,6
        sync = sync + real(zt1(i))**2 + aimag(zt1(i))**2 + real(zt2(i))**2 + aimag(zt2(i))**2 + real(zt3(i))**2 + &
          aimag(zt3(i))**2
      enddo
    endif
  else ! if(ipass.eq.3 .or. ipass.eq.4 .or. ipass.eq.8)  then
    do i=0,6
      if(i.lt.6) then
        z1=(zt1(i)+zt1(i+1))/2; z2=(zt2(i)+zt2(i+1))/2; z3=(zt3(i)+zt3(i+1))/2 ! use i=5 z values for i=6
      endif
      if(.not.lastsync) then
        sync = sync + abs(real(z1)) + abs(aimag(z1)) + abs(real(z2)) + abs(aimag(z2)) + abs(real(z3)) + abs(aimag(z3))
      else
        sync = sync + real(z1)**2 + aimag(z1)**2 + real(z2)**2 + aimag(z2)**2 + real(z3)**2 + aimag(z3)**2
      endif
    enddo
  endif

  if(itwk.eq.1 .and. lcqcand .and. iqso.eq.1) then
    zt4=0.
    do i=0,7
      csync2=csynccq(i,1:32); csync2=ctwk*csync2      !Tweak the frequency
      i4=i0+(i+7)*32; zt4(i)=sum(cd0(i4:i4+31)*conjg(csync2))
    enddo
    do i=0,7
      if(ipass.eq.3 .or. ipass.eq.4 .or. ipass.eq.8)  then
        if(i.lt.7) z4=(zt4(i)+zt4(i+1))/2 ! use i=6 z4 value for i=7
      else
       z4=zt4(i)
      endif
      if(ipass.eq.1 .or. ipass.eq.5 .or. ipass.eq.9) then; sync = sync + SQRT(real(z4)**2 + aimag(z4)**2)
      else if(ipass.eq.2 .or. ipass.eq.6 .or. ipass.eq.7) then; sync = sync + real(z4)**2 + aimag(z4)**2
      else if(ipass.eq.3 .or. ipass.eq.4 .or. ipass.eq.8) then; sync = sync + abs(real(z4)) + abs(aimag(z4))
      endif
    enddo
  endif

  if(.not.lastsync) then
    if(iqso.gt.1 .and. iqso.lt.4 .and. lcallsstd) csync1=csynce; if(iqso.eq.4 .and. .not.lcq) csync1=csyncsd

    if((iqso.eq.4 .and. .not.lcq) .or. ((iqso.eq.2 .or. iqso.eq.3) .and. lcallsstd)) then
      zt5=0.
      do i=0,18
        csync2=csync1(i,1:32)
        if(itwk.eq.1) csync2=ctwk*csync2      !Tweak the frequency
        i4=i0+(i+7)*32; zt5(i)=sum(cd0(i4:i4+31)*conjg(csync2))
      enddo
      do i=0,18
        if(ipass.eq.2 .or. ipass.eq.6 .or. ipass.eq.7)  then
          if(i.lt.18) z4=(zt5(i)+zt5(i+1))/2 ! use i=17 z4 value for i=18
        else
         z4=zt5(i)
        endif
        if(ipass.eq.1 .or. ipass.eq.5 .or. ipass.eq.9) then; sync = sync + SQRT(real(z4)**2 + aimag(z4)**2)
        else if(ipass.eq.2 .or. ipass.eq.6 .or. ipass.eq.7) then; sync = sync + real(z4)**2 + aimag(z4)**2
        else if(ipass.eq.3 .or. ipass.eq.4 .or. ipass.eq.8) then; sync = sync + abs(real(z4)) + abs(aimag(z4))
        endif
      enddo
    endif

    if(iqso.eq.4 .and. lcq) then
      do i=0,57
        csync2=csyncsdcq(i,1:32)
        if(itwk.eq.1) csync2=ctwk*csync2      !Tweak the frequency
        z4=0.; if(i.lt.29) then; k=i+7; else; k=i+14; endif
        i4=i0+k*32; z4=sum(cd0(i4:i4+31)*conjg(csync2))
        if(ipass.eq.1 .or. ipass.eq.5 .or. ipass.eq.9) then; sync = sync + SQRT(real(z4)**2 + aimag(z4)**2)
          else if(ipass.eq.2 .or. ipass.eq.6 .or. ipass.eq.7) then; sync = sync + real(z4)**2 + aimag(z4)**2
          else if(ipass.eq.3 .or. ipass.eq.4 .or. ipass.eq.8) then; sync = sync + abs(real(z4)) + abs(aimag(z4))
        endif
      enddo
    endif
  endif

  return
end subroutine sync8d
