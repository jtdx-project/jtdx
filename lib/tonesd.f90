! This source code file was last time modified by Igor UA3DJY on 20190411

subroutine tonesd(msgd,lcq)

  use ft8_mod1, only : csyncsd,csyncsdcq,twopi,itone76,idtone76,msgsd76
  character, intent(in) :: msgd*37
  character msg37*37,msgsent37*37,c1*12,c2*12,grid*6
  character*4 rpt(75)
  integer itone(79)
  integer*1 msgbits(77)
  logical(1), intent(in) :: lcq
  logical(1) lgrid,lr73

  data rpt/'+09 ','+08 ','+07 ','+06 ','+05 ','+04 ','+03 ','+02 ','+01 ','+00 ', &
           '-01 ','-02 ','-03 ','-04 ','-05 ','-06 ','-07 ','-08 ','-09 ','-10 ', &
           '-11 ','-12 ','-13 ','-14 ','-15 ','-16 ','-17 ','-18 ','-19 ','-20 ', &
           '-21 ','-22 ','-23 ','-24 ','-25 ','-26 ','R+09','R+08','R+07','R+06', &
           'R+05','R+04','R+03','R+02','R+01','R+00','R-01','R-02','R-03','R-04', &
           'R-05','R-06','R-07','R-08','R-09','R-10','R-11','R-12','R-13','R-14', &
           'R-15','R-16','R-17','R-18','R-19','R-20','R-21','R-22','R-23','R-24', &
           'R-25','R-26','RRR ','RR73','73  '/

! taken from ft8_mod1
!    integer itone76(76,79),idtone76(76,58)
!    character*37 msgsd76(76)
!    complex csyncsd(0:18,32),csyncsdcq(0:71,32)

  lgrid=.false.; lr73=.false.

  if(lcq) then
    msg37=msgd
    i3=-1; n3=-1
    call genft8sd(msg37,i3,n3,msgsent37,msgbits,itone)
  else
    c1='            '; c2='            '
    ispc1=index(msgd,' '); ispc2=index(msgd((ispc1+1):),' ')+ispc1; ispc3=index(msgd((ispc2+1):),' ')+ispc2;
    if(len(msgd(1:ispc1-1)).le.12 .and. len(msgd(ispc1+1:ispc2-1)).le.12) then
      c1=msgd(1:ispc1-1); c2=msgd(ispc1+1:ispc2-1)
    endif
    if(index(msgd,' RR73').gt.0 .or. index(msgd,' 73').gt.0) lr73=.true.
    if(.not.lr73 .and. (ispc3-ispc2).eq.5 .and. msgd(ispc2+1:ispc2+2).ne.'R+' .and. msgd(ispc2+1:ispc2+2).ne.'R-') &
      lgrid=.true.
    if(lgrid) then; grid=msgd(ispc2+1:ispc3-1); if(len_trim(grid).ne.4) grid='AA00'
    else; grid='AA00'
    endif
    do i=1,75
      msg37='                                     '
      msg37=trim(c1)//' '//trim(c2)//' '//trim(rpt(i))
      msgsd76(i)=msg37
      i3=-1; n3=-1
      call genft8sd(msg37,i3,n3,msgsent37,msgbits,itone)
      idtone76(i,1:29)=itone(8:36)
      idtone76(i,30:58)=itone(44:72)
      itone76(i,1:79)=itone(1:79)
    enddo
      msg37='                                     '
      msg37=trim(c1)//' '//trim(c2)//' '//trim(grid)
      msgsd76(76)=msg37
      i3=-1; n3=-1
      call genft8sd(msg37,i3,n3,msgsent37,msgbits,itone)
      idtone76(76,1:29)=itone(8:36)
      idtone76(76,30:58)=itone(44:72)
      itone76(76,1:79)=itone(1:79)
  endif

!for sync8d.f90:
!     fs2=12000.0/NDOWN         !Sample rate after downsampling
!     dt2=0.005 ! 1/fs2          !Corresponding sample interval = 1 / Sample rate after downsampling
!     baud=6.25 ! 1.0/32*dt2     !Keying rate = 1 / Symbol duration
  pstep=0.25d0*atan(1.d0) ! twopi*baud*dt2
  if(lcq) then
    do i=0,57
      phi=0.0
      if(i.lt.29) then; dphi=pstep*itone(i+8); else; dphi=pstep*itone(i+15); endif
      do j=1,32
        csyncsdcq(i,j)=cmplx(cos(phi),sin(phi))
        phi=mod(phi+dphi,twopi)
      enddo
    enddo
  else
    do i=0,18
      phi=0.0
      dphi=pstep*itone76(1,i+8)
      do j=1,32
        csyncsd(i,j)=cmplx(cos(phi),sin(phi)) !Waveform for base part of the message
        phi=mod(phi+dphi,twopi)
      enddo
    enddo
  endif
 
  return
end subroutine tonesd
