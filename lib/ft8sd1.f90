subroutine ft8sd1(s8,itone,msgd,msg37,lft8sd,lcq)

  use ft8_mod1, only : mycall
  real, intent(in) :: s8(0:7,79)
  real s8_1(0:7,79)
  character msgd*37,msg37*37,msg372*37,msgsent37*37,c1*12,c2*12
  character*37 msg4(4)
  integer idtone4(4,58),itone4(4,79)
  integer itone(79),itonedem(58),idtone(58),ip1(1)
  integer*1 msgbits(77)
  logical(1), intent(in) :: lcq
  logical(1) lft8sd,lr73,lgrid,lmatched(58)

  lmatched=.false.; lr73=.false.; lgrid=.false.
  itonedem=11

  if(.not.lcq) then
    c1='            '; c2='            '
    ispc1=index(msgd,' '); ispc2=index(msgd((ispc1+1):),' ')+ispc1; ispc3=index(msgd((ispc2+1):),' ')+ispc2
    if(len(msgd(1:ispc1-1)).le.12 .and. len(msgd(ispc1+1:ispc2-1)).le.12) then
      c1=msgd(1:ispc1-1); c2=msgd(ispc1+1:ispc2-1)
    else
      return
    endif
    if(len_trim(c1).lt.3 .or. len_trim(c2).lt.3 .or. c2.eq.trim(mycall)) return
    if(index(msgd,' RR73').gt.0 .or. index(msgd,' 73').gt.0) lr73=.true.
    if(.not.lr73 .and. (ispc3-ispc2).eq.5 .and. msgd(ispc2+1:ispc2+2).ne.'R+' &
       .and. msgd(ispc2+1:ispc2+2).ne.'R-') lgrid=.true.
    if(.not.lgrid .and. .not.lr73) then
      do i=1,4
        msg4(i)='                                     '
        if(i.eq.1) msg4(i)=msgd
        if(i.eq.2) msg4(i)=trim(c1)//' '//trim(c2)//' RRR'
        if(i.eq.3) msg4(i)=trim(c1)//' '//trim(c2)//' RR73'
        if(i.eq.4) msg4(i)=trim(c1)//' '//trim(c2)//' 73'
      enddo
    endif
  endif

  if(lcq .or. lgrid .or. lr73) then
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

  s8_1=s8
  do i=1,58
    if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+7)=0.0
    else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); s8_1(ip1(1),i+14)=0.0; endif
  enddo

  imax=0
  if(lcq .or. lgrid .or. lr73) then
    nmatch1=0; ncrcpaty1=0
    do k=1,58
      if(idtone(k).eq.itonedem(k)) then; nmatch1=nmatch1+1; if(k.gt.25) ncrcpaty1=ncrcpaty1+1; endif
    enddo
    if(nmatch1.gt.29 .and. ncrcpaty1.gt.10) then; lft8sd=.true.; msg37=msgd; return; endif
  else
    nmatchditer1=0; ncrcpatyiter1=0
    do i=1,4
      nmatch1=0; ncrcpaty1=0
      do k=1,58
        if(idtone4(i,k).eq.itonedem(k)) then; nmatch1=nmatch1+1; if(k.gt.25) ncrcpaty1=ncrcpaty1+1; endif
      enddo
      if(nmatch1.gt.nmatchditer1) then; imax=i; nmatchditer1=nmatch1; ncrcpatyiter1=ncrcpaty1; endif
    enddo
    if(imax.eq.0) return
    if(lr73 .and. imax.eq.2) return ! RRR is not a valid message
    nmatch1=nmatchditer1; ncrcpaty1=ncrcpatyiter1
    idtone(1:58)=idtone4(imax,1:58)
    if(nmatch1.gt.29 .and. ncrcpaty1.gt.10) then; lft8sd=.true.; msg37=msg4(imax); itone(1:79)=itone4(imax,1:79); return; endif
  endif

  if(nmatch1.ge.22) then
    do k=1,58
      if(idtone(k).eq.itonedem(k)) lmatched(k)=.true.
    enddo
    do i=1,58
      if(lmatched(i)) cycle
      if(i.le.29) then; ip1=maxloc(s8_1(:,i+7))-1; itonedem(i)=ip1(1)
      else; ip1=maxloc(s8_1(:,i+14))-1; itonedem(i)=ip1(1); endif
    enddo

    nmatch2=nmatch1; ncrcpaty2=ncrcpaty1
    if(lcq .or. lgrid .or. lr73) then
      do k=1,58
        if(lmatched(k)) cycle
        if(idtone(k).eq.itonedem(k)) then; nmatch2=nmatch2+1; if(k.gt.25) ncrcpaty2=ncrcpaty2+1; endif
      enddo
      if(nmatch2.gt.41 .and. ncrcpaty2.gt.19) then; lft8sd=.true.; msg37=msgd; return; endif
    else
      do k=1,58
        if(lmatched(k)) cycle
        if(idtone(k).eq.itonedem(k)) then; nmatch2=nmatch2+1; if(k.gt.25) ncrcpaty2=ncrcpaty2+1; endif
      enddo
      if(nmatch2.gt.41 .and. ncrcpaty2.gt.19) then; lft8sd=.true.; msg37=msg4(imax); itone(1:79)=itone4(imax,1:79); return; endif
    endif
  endif

  return
end subroutine ft8sd1
