subroutine ft8sd(s8,srr,itone,msgd,msg37,lft8sd,lcq)

  use ft8_mod1, only : mycall
  real, intent(in) :: s8(0:7,79)
  real s8_1(0:7,79)
  character msgd*37,msg37*37,msg372*37,msgsent37*37,c1*12,c2*12
  character*37 msg4(4)
  integer idtone4(4,58),itone4(4,79)
  integer itone(79),itonedem(58),idtone(58),ip1(1)
  integer*1 msgbits(77)
  logical(1), intent(in) :: lcq
  logical(1) lft8sd,lmatched(58)

  if(index(msgd,' RR73').gt.0 .or. index(msgd,' 73').gt.0) return ! do not process 73 messages
! do not process messages with grid
  if(.not.(msgd(1:3).eq.'CQ ' .or. index(msgd,'-').gt.0 .or. index(msgd,'+').gt.0 .or. index(msgd,' RRR').gt.0)) return

  lmatched=.false.; s8_1=s8; itonedem=11

  if(.not.lcq) then
    c1='            '; c2='            '
    ispc1=index(msgd,' '); ispc2=index(msgd((ispc1+1):),' ')+ispc1
    if(len(msgd(1:ispc1-1)).le.12 .and. len(msgd(ispc1+1:ispc2-1)).le.12) then
      c1=msgd(1:ispc1-1); c2=msgd(ispc1+1:ispc2-1)
    else
      return
    endif
    if(len_trim(c1).lt.3 .or. len_trim(c2).lt.3 .or. c2.eq.trim(mycall)) return
    do i=1,4
      msg4(i)='                                     '
      if(i.eq.1) msg4(i)=msgd
      if(i.eq.2) msg4(i)=trim(c1)//' '//trim(c2)//' RRR'
      if(i.eq.3) msg4(i)=trim(c1)//' '//trim(c2)//' RR73'
      if(i.eq.4) msg4(i)=trim(c1)//' '//trim(c2)//' 73'
    enddo
  endif

  if(lcq) then
    i3=-1; n3=-1
    msg372=msgd
    call genft8sd(msg372,i3,n3,msgsent37,msgbits,itone)
    idtone(1:29)=itone(8:36)
    idtone(30:58)=itone(44:72)
  else
    do i=1,4
      msg372=msg4(i)
      i3=-1; n3=-1
      call genft8sd(msg372,i3,n3,msgsent37,msgbits,itone)
      idtone4(i,1:29)=itone(8:36)
      idtone4(i,30:58)=itone(44:72)
      itone4(i,1:79)=itone(1:79)
    enddo
  endif

  do i=1,58
    if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
    else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
  enddo

  imax=0
  if(lcq) then
    nmatch1=0; ncrcpaty1=0
    do k=1,58
      if(idtone(k).eq.itonedem(k)) then; nmatch1=nmatch1+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty1=ncrcpaty1+1; endif
    enddo
    if(nmatch1.gt.26) then; lft8sd=.true.; msg37=msgd; return; endif
  else
    nmatchditer1=0; nbaseiter1=0; ncrcpatyiter1=0;
    do i=1,4
      nmatch1=0; nbase1=0; ncrcpaty1=0
      do k=1,58
        if(idtone4(i,k).eq.itonedem(k)) then
          nmatch1=nmatch1+1; if(k.lt.23) nbase1=nbase1+1; if(k.gt.25) ncrcpaty1=ncrcpaty1+1 ! CRC+parity
        endif
      enddo
      if(nmatch1.gt.nmatchditer1) then
        imax=i; nmatchditer1=nmatch1; nbaseiter1=nbase1; ncrcpatyiter1=ncrcpaty1
      endif
    enddo
    if(imax.eq.0) return
    if(srr.gt.3.0 .and. nbaseiter1.lt.12) return ! prevent false decodes from the strong signals
    nmatch1=nmatchditer1; ncrcpaty1=ncrcpatyiter1!; nbase1=nbaseiter1
    idtone(1:58)=idtone4(imax,1:58)
    if(nmatch1.gt.26 .and. ncrcpaty1.gt.10) then
      lft8sd=.true.; msg37=msg4(imax); itone(1:79)=itone4(imax,1:79); return
    endif
    do k=1,58
      if(idtone(k).eq.itonedem(k)) lmatched(k)=.true. 
    enddo
  endif

  if(nmatch1.ge.16) then
    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
    enddo
    nmatch2=nmatch1; ncrcpaty2=ncrcpaty1
    do k=1,58
      if(lmatched(k)) cycle
      if(idtone(k).eq.itonedem(k)) then; nmatch2=nmatch2+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty2=ncrcpaty2+1; endif
    enddo
    if(nmatch2.gt.38 .and. ncrcpaty2.gt.19) then
      lft8sd=.true.
      if(lcq) then; msg37=msgd; else; msg37=msg4(imax); itone(1:79)=itone4(imax,1:79); endif
      return
    endif

    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
    enddo
    nmatch3=nmatch2; ncrcpaty3=ncrcpaty2
    do k=1,58
      if(lmatched(k)) cycle
      if(idtone(k).eq.itonedem(k)) then; nmatch3=nmatch3+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty3=ncrcpaty3+1; endif
    enddo
    if(nmatch3.gt.44 .and. ncrcpaty3.gt.21) then
      lft8sd=.true.
      if(lcq) then; msg37=msgd; else; msg37=msg4(imax); itone(1:79)=itone4(imax,1:79); endif
      return
    endif

    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
    enddo
    nmatch4=nmatch3; ncrcpaty4=ncrcpaty3
    do k=1,58
      if(lmatched(k)) cycle
      if(idtone(k).eq.itonedem(k)) then; nmatch4=nmatch4+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty4=ncrcpaty4+1; endif
    enddo
    if(nmatch4.gt.47 .and. ncrcpaty4.gt.23) then
      lft8sd=.true.
      if(lcq) then; msg37=msgd; else; msg37=msg4(imax); itone(1:79)=itone4(imax,1:79); endif
      return
    endif

    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
    enddo
    nmatch5=nmatch4; ncrcpaty5=ncrcpaty4
    do k=1,58
      if(lmatched(k)) cycle
      if(idtone(k).eq.itonedem(k)) then; nmatch5=nmatch5+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty5=ncrcpaty5+1; endif
    enddo
    if(nmatch5.gt.50 .and. (nmatch1.gt.21 .or. nmatch2.gt.31 .or. nmatch3.gt.38 .or. nmatch4.gt.46) .and. &
       ncrcpaty5.gt.25) then
      lft8sd=.true.
      if(lcq) then; msg37=msgd; else; msg37=msg4(imax); itone(1:79)=itone4(imax,1:79); endif
      return
    endif

    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
    enddo
    nmatch6=nmatch5; ncrcpaty6=ncrcpaty5
    do k=1,58
      if(lmatched(k)) cycle
      if(idtone(k).eq.itonedem(k)) then; nmatch6=nmatch6+1; lmatched(k)=.true.; if(k.gt.25) ncrcpaty6=ncrcpaty6+1; endif
    enddo
    if(nmatch6.gt.54 .and. (nmatch1.gt.22 .or. nmatch2.gt.27 .or. nmatch3.gt.35 .or. &
       (nmatch2-nmatch1).gt.9 .or. (nmatch3-nmatch2).gt.10) .and. ncrcpaty6.gt.29) then
      lft8sd=.true.
      if(lcq) then; msg37=msgd; else; msg37=msg4(imax); itone(1:79)=itone4(imax,1:79); endif
      return
    endif

  endif

  return
end subroutine ft8sd
