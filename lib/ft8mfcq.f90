! This source code file was last time modified by Igor UA3DJY on 20200203

subroutine ft8mfcq(s8,itone,msgd,msg37,lft8sd)

  use ft8_mod1, only : idtone25
  real, intent(in) :: s8(0:7,79)
  real s8d(0:7,58)
  character*37 msgd,msg37,msgsent37
  integer itone(79),mrs(58),mrs2(58)
  integer*1 msgbits(77)
  logical(1) lft8sd

  if(len_trim(msgd).lt.6) return
  i3=-1; n3=-1
  call genft8sd(msgd,i3,n3,msgsent37,msgbits,itone)
  idtone25(1,1:29)=itone(8:36)
  idtone25(1,30:58)=itone(44:72)

  thresh=0.0; i1=0; i2=0
  do i=1,58; if(i.le.29) then; s8d(0:7,i)=s8(0:7,i+7); else; s8d(0:7,i)=s8(0:7,i+14); endif; enddo
  do j=1,58
    s1=-1.0E6; s2=-1.0E6
    do i=0,7; if(s8d(i,j).gt.s1) then; s1=s8d(i,j); i1=i; endif; enddo
    do i=0,7; if(i.ne.i1 .and. s8d(i,j).gt.s2) then; s2=s8d(i,j); i2=i; endif; enddo
    mrs(j)=i1; mrs2(j)=i2
  enddo
  ref0=0.0
  do i=1,58; ref0=ref0 + s8d(mrs(i),i); enddo

  ipk=0; u1=0.0; u2=0.0
  do k=1,25
    psum=0.; ref=ref0
    do j=1,58; i3=idtone25(k,j); psum=psum + s8d(i3,j); if(i3.eq.mrs(j)) ref=ref - s8d(i3,j) + s8d(mrs2(j),j); enddo
    p=psum/ref
    if(p.gt.u1) then; u2=u1; u1=p; ipk=k
    else; if(p.gt.u2) u2=p
    endif
  enddo

  if(ipk.eq.1) then
    qual=100.0*(u1-u2)
    thresh=(qual+10.0)*(u1-0.6)
    if(thresh.gt.4.0 .and. qual.gt.2.6 .and. u1.gt.0.77) then
!print *,'matched',thresh
!print *,msgsd76(ipk)
!      if(msgd(1:10).eq.'CQ DE AA00') return ! triggered by memory corruption?
      lft8sd=.true.; msg37=msgd; return
    endif
  endif

  return
end subroutine ft8mfcq
