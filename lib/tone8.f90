subroutine tone8(lmycallstd,lhiscallstd)

  use ft8_mod1, only : itone56,idtone56,msg,csynce,mycall,hiscall,idtonecqdxcns,idtonedxcns73,mybcall,hisbcall,lhound, &
                       idtonefox73,idtonespec
  complex csig0(151680)
  character msg37*37,msgsent37*37,mycall14*14,hiscall14*14
  character*4 rpt(56)
  integer itone(79),itone1(79)
  integer*1 msgbits(77)
  logical(1), intent(in) :: lmycallstd,lhiscallstd

  data rpt/'-01 ','-02 ','-03 ','-04 ','-05 ','-06 ','-07 ','-08 ','-09 ','-10 ', &
           '-11 ','-12 ','-13 ','-14 ','-15 ','-16 ','-17 ','-18 ','-19 ','-20 ', &
           '-21 ','-22 ','-23 ','-24 ','-25 ','-26 ','R-01','R-02','R-03','R-04', &
           'R-05','R-06','R-07','R-08','R-09','R-10','R-11','R-12','R-13','R-14', &
           'R-15','R-16','R-17','R-18','R-19','R-20','R-21','R-22','R-23','R-24', &
           'R-25','R-26','AA00','RRR ','RR73','73  '/

! taken from ft8_mod1
!    integer itone56(56,79),idtone56(56,58)
!    character*37 msg(56)

  if(lhound .and. len_trim(mybcall).gt.2 .and. len_trim(hisbcall).gt.2) then
    msg37=''; msg37=trim(mybcall)//' '//trim(hisbcall)//' RR73'
    i3=-1; n3=-1
    call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
    idtonefox73(1:29)=itone(8:36)
    idtonefox73(30:58)=itone(44:72)
    msg37=''; msg37=trim(mybcall)//' RR73; '//trim(mybcall)//' <'//trim(hiscall)//'> -12'
    i3=-1; n3=-1
    call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
    idtonespec(1:29)=itone(8:36)
    idtonespec(30:58)=itone(44:72)
  endif

  if(.not.lhiscallstd .and. len(trim(hiscall)).gt.2) then
    msg37=''; msg37='CQ '//trim(hiscall)
    i3=-1; n3=-1
    call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
    idtonecqdxcns(1:29)=itone(8:36)
    idtonecqdxcns(30:58)=itone(44:72)
    msg37=''; msg37='<AA1AAA> '//trim(hiscall)//' 73'
    i3=-1; n3=-1
    call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
    idtonedxcns73(1:29)=itone(8:36)
    idtonedxcns73(30:58)=itone(44:72)
  endif

  if(.not.lhiscallstd .and. .not.lmycallstd) return ! we do not support such message in FT8v2
  if(.not.lhiscallstd .or. .not.lmycallstd) then
    mycall14='              '; mycall14='<'//trim(mycall)//'>'
    hiscall14='              '; hiscall14='<'//trim(hiscall)//'>'
  endif

  if(lhiscallstd .and. lmycallstd) then
    do i=1,56
      msg37=''; msg37=trim(mycall)//' '//trim(hiscall)//' '//trim(rpt(i))
      msg(i)=msg37
      i3=-1; n3=-1
      call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
      if(i.eq.1) itone1=itone
      idtone56(i,1:29)=itone(8:36)
      idtone56(i,30:58)=itone(44:72)
      itone56(i,1:79)=itone(1:79)
    enddo
    go to 2
  endif

  if(.not.lhiscallstd .and. lmycallstd) then
    do i=1,52
      msg37=''; msg37=trim(mycall)//' '//trim(hiscall14)//' '//trim(rpt(i))
      i3=-1; n3=-1
      call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
      if(i.eq.1) itone1=itone
      idtone56(i,1:29)=itone(8:36)
      idtone56(i,30:58)=itone(44:72)
      itone56(i,1:79)=itone(1:79)
      msg37=''; msg37=trim(mycall)//' '//trim(hiscall)//' '//trim(rpt(i))
      msg(i)=msg37
    enddo
    do i=54,56
      msg37=''; msg37=trim(mycall14)//' '//trim(hiscall)//' '//trim(rpt(i))
      i3=-1; n3=-1
      call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
      idtone56(i,1:29)=itone(8:36)
      idtone56(i,30:58)=itone(44:72)
      itone56(i,1:79)=itone(1:79)
      msg37=''; msg37=trim(mycall)//' '//trim(hiscall)//' '//trim(rpt(i))
      msg(i)=msg37
    enddo
    msg37=''; msg37=trim(mycall14)//' '//trim(hiscall)
    msg(53)=msg37
    i3=-1; n3=-1
    call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
    idtone56(53,1:29)=itone(8:36)
    idtone56(53,30:58)=itone(44:72)
    itone56(53,1:79)=itone(1:79)
    go to 2
  endif

  if(lhiscallstd .and. .not.lmycallstd) then
    do i=1,52
      msg37=''; msg37=trim(mycall14)//' '//trim(hiscall)//' '//trim(rpt(i))
      i3=-1; n3=-1
      call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
      if(i.eq.1) itone1=itone
      idtone56(i,1:29)=itone(8:36)
      idtone56(i,30:58)=itone(44:72)
      itone56(i,1:79)=itone(1:79)
      msg37=''; msg37=trim(mycall)//' '//trim(hiscall)//' '//trim(rpt(i))
      msg(i)=msg37
    enddo
    do i=54,56
      msg37=''; msg37=trim(mycall)//' '//trim(hiscall14)//' '//trim(rpt(i))
      i3=-1; n3=-1
      call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
      idtone56(i,1:29)=itone(8:36)
      idtone56(i,30:58)=itone(44:72)
      itone56(i,1:79)=itone(1:79)
      msg37=''; msg37=trim(mycall)//' '//trim(hiscall)//' '//trim(rpt(i))
      msg(i)=msg37
    enddo
    msg37=''; msg37=trim(mycall14)//' '//trim(hiscall)
    msg(53)=msg37
    i3=-1; n3=-1
    call genft8(msg37,i3,n3,0,msgsent37,msgbits,itone)
    idtone56(53,1:29)=itone(8:36)
    idtone56(53,30:58)=itone(44:72)
    itone56(53,1:79)=itone(1:79)
    go to 2
  endif

2 m=13441 ! 7*1920+1
  call gen_ft8wave(itone1,79,1920,2.0,12000.0,0.0,csig0,xjunk,1,151680)
  do j=0,18
    do k=1,32; csynce(j,k)=csig0(m); m=m+60; enddo
  enddo
 
  return
end subroutine tone8
