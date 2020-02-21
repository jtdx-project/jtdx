! last time modified by Igor UA3DJY on 20191118

subroutine tone8(lmycallstd,lhiscallstd)

  use ft8_mod1, only : itone56,idtone56,msg,csynce,twopi,mycall,hiscall
  character msg37*37,msgsent37*37,mycall14*14,hiscall14*14
  character*4 rpt(56)
  integer itone(79)
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

  if(.not.lhiscallstd .and. .not.lmycallstd) return ! we do not support such message in FT8v2
  if(.not.lhiscallstd .or. .not.lmycallstd) then
    mycall14='              '; mycall14='<'//trim(mycall)//'>'
    hiscall14='              '; hiscall14='<'//trim(hiscall)//'>'
  endif

  if(lhiscallstd .and. lmycallstd) then
    do i=1,56
      msg37='                                     '
      msg37=trim(mycall)//' '//trim(hiscall)//' '//trim(rpt(i))
      msg(i)=msg37
      i3=-1; n3=-1
      call genft8(msg37,i3,n3,msgsent37,msgbits,itone)
      idtone56(i,1:29)=itone(8:36)
      idtone56(i,30:58)=itone(44:72)
      itone56(i,1:79)=itone(1:79)
    enddo
    go to 2
  endif

  if(.not.lhiscallstd .and. lmycallstd) then
    do i=1,52
      msg37='                                     '
      msg37=trim(mycall)//' '//trim(hiscall14)//' '//trim(rpt(i))
      i3=-1; n3=-1
      call genft8(msg37,i3,n3,msgsent37,msgbits,itone)
      idtone56(i,1:29)=itone(8:36)
      idtone56(i,30:58)=itone(44:72)
      itone56(i,1:79)=itone(1:79)
      msg37='                                     '
      msg37=trim(mycall)//' '//trim(hiscall)//' '//trim(rpt(i))
      msg(i)=msg37
    enddo
    do i=54,56
      msg37='                                     '
      msg37=trim(mycall14)//' '//trim(hiscall)//' '//trim(rpt(i))
      i3=-1; n3=-1
      call genft8(msg37,i3,n3,msgsent37,msgbits,itone)
      idtone56(i,1:29)=itone(8:36)
      idtone56(i,30:58)=itone(44:72)
      itone56(i,1:79)=itone(1:79)
      msg37='                                     '
      msg37=trim(mycall)//' '//trim(hiscall)//' '//trim(rpt(i))
      msg(i)=msg37
    enddo
    msg37='                                     '
    msg37=trim(mycall14)//' '//trim(hiscall)
    msg(53)=msg37
    i3=-1; n3=-1
    call genft8(msg37,i3,n3,msgsent37,msgbits,itone)
    idtone56(53,1:29)=itone(8:36)
    idtone56(53,30:58)=itone(44:72)
    itone56(53,1:79)=itone(1:79)
    go to 2
  endif

  if(lhiscallstd .and. .not.lmycallstd) then
    do i=1,52
      msg37='                                     '
      msg37=trim(mycall14)//' '//trim(hiscall)//' '//trim(rpt(i))
      i3=-1; n3=-1
      call genft8(msg37,i3,n3,msgsent37,msgbits,itone)
      idtone56(i,1:29)=itone(8:36)
      idtone56(i,30:58)=itone(44:72)
      itone56(i,1:79)=itone(1:79)
      msg37='                                     '
      msg37=trim(mycall)//' '//trim(hiscall)//' '//trim(rpt(i))
      msg(i)=msg37
    enddo
    do i=54,56
      msg37='                                     '
      msg37=trim(mycall)//' '//trim(hiscall14)//' '//trim(rpt(i))
      i3=-1; n3=-1
      call genft8(msg37,i3,n3,msgsent37,msgbits,itone)
      idtone56(i,1:29)=itone(8:36)
      idtone56(i,30:58)=itone(44:72)
      itone56(i,1:79)=itone(1:79)
      msg37='                                     '
      msg37=trim(mycall)//' '//trim(hiscall)//' '//trim(rpt(i))
      msg(i)=msg37
    enddo
    msg37='                                     '
    msg37=trim(mycall14)//' '//trim(hiscall)
    msg(53)=msg37
    i3=-1; n3=-1
    call genft8(msg37,i3,n3,msgsent37,msgbits,itone)
    idtone56(53,1:29)=itone(8:36)
    idtone56(53,30:58)=itone(44:72)
    itone56(53,1:79)=itone(1:79)
    go to 2
  endif

!for sync8d.f90:
!     fs2=12000.0/NDOWN         !Sample rate after downsampling
!     dt2=0.005 ! 1/fs2          !Corresponding sample interval = 1 / Sample rate after downsampling
!     baud=6.25 ! 1.0/32*dt2     !Keying rate = 1 / Symbol duration
2 pstep=0.25d0*atan(1.d0) ! twopi*baud*dt2
  do i=0,18
    phi=0.0
    dphi=pstep*itone56(1,i+8)
    do j=1,32
      csynce(i,j)=cmplx(cos(phi),sin(phi)) !Waveform for base part of the message
      phi=mod(phi+dphi,twopi)
    enddo
  enddo 
 
  return
end subroutine tone8
