! This source code file was last time modified by Igor UA3DJY on 20200203

subroutine ft8mf1(s8,itone,msgd,msg37,lft8sd)

  use ft8_mod1, only : itone76,idtone76,msgsd76
  real, intent(in) :: s8(0:7,79)
  real s8d(0:7,58)
  character msgd*37,msg37*37,msggrid*37,c1*12,c2*12,grid*4
  integer itone(79),mrs(58),mrs2(58)
  logical(1) lft8sd,lr73,lgrid,lreport,lrreport,lrrr

  lr73=.false.; lgrid=.false.; lreport=.false.; lrreport=.false.; lrrr=.false.
  c1='            '; c2='            '
  ispc1=index(msgd,' '); ispc2=index(msgd((ispc1+1):),' ')+ispc1; ispc3=index(msgd((ispc2+1):),' ')+ispc2
  if(len(msgd(1:ispc1-1)).le.12 .and. len(msgd(ispc1+1:ispc2-1)).le.12) then
    c1=msgd(1:ispc1-1); c2=msgd(ispc1+1:ispc2-1)
  else
    return
  endif
  if(len_trim(c1).lt.3 .or. len_trim(c2).lt.3) return
  if(msgd(ispc2+1:ispc3-1).eq.'RR73' .or. msgd(ispc2+1:ispc3-1).eq.'73') lr73=.true.
  if(.not.lr73 .and. (ispc3-ispc2).eq.5 .and. msgd(ispc2+1:ispc2+2).ne.'R+' .and. msgd(ispc2+1:ispc2+2).ne.'R-') &
    lgrid=.true.
  if(msgd(ispc2+1:ispc2+1).eq.'+' .or. msgd(ispc2+1:ispc2+1).eq.'-') lreport=.true.
  if(msgd(ispc2+1:ispc2+2).eq.'R+' .or. msgd(ispc2+1:ispc2+2).eq.'R-') lrreport=.true.
  if(msgd(ispc2+1:ispc3-1).eq.'RRR') lrrr=.true.

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
  do k=1,76
    psum=0.; ref=ref0
    do j=1,58; i3=idtone76(k,j); psum=psum + s8d(i3,j); if(i3.eq.mrs(j)) ref=ref - s8d(i3,j) + s8d(mrs2(j),j); enddo
    p=psum/ref
    if(p.gt.u1) then; u2=u1; u1=p; ipk=k
    else; if(p.gt.u2) u2=p
    endif
  enddo
!/'+09 ','+08 ','+07 ','+06 ','+05 ','+04 ','+03 ','+02 ','+01 ','+00 ', &
! '-01 ','-02 ','-03 ','-04 ','-05 ','-06 ','-07 ','-08 ','-09 ','-10 ', &
! '-11 ','-12 ','-13 ','-14 ','-15 ','-16 ','-17 ','-18 ','-19 ','-20 ', &
! '-21 ','-22 ','-23 ','-24 ','-25 ','-26 ','R+09','R+08','R+07','R+06', &
! 'R+05','R+04','R+03','R+02','R+01','R+00','R-01','R-02','R-03','R-04', &
! 'R-05','R-06','R-07','R-08','R-09','R-10','R-11','R-12','R-13','R-14', &
! 'R-15','R-16','R-17','R-18','R-19','R-20','R-21','R-22','R-23','R-24', &
! 'R-25','R-26','RRR ','RR73','73  ','AA00'/
  if(ipk.ne.0) then
    if(lgrid) then
      if(ipk.eq.76) then
        msggrid=msgsd76(76)
        ispc1=index(msggrid,' '); ispc2=index(msggrid((ispc1+1):),' ')+ispc1; ispc3=index(msggrid((ispc2+1):),' ')+ispc2
        grid=msggrid(ispc2+1:ispc3-1)
        if(grid.eq.'AA00') return
      endif
      if(ipk.lt.37 .or. (ipk.gt.72 .and. ipk.lt.76)) return
    endif
    if(lreport) then; if((ipk.gt.36 .and. ipk.lt.73) .or. ipk.eq.76) return; endif
    if(lrreport) then; if(ipk.lt.37 .or. ipk.eq.73 .or. ipk.eq.76) return; endif
    if(lrrr) then; if(ipk.lt.73 .or. ipk.eq.76) return; endif
    if(lr73) then; if(ipk.lt.74 .or. ipk.eq.76) return; endif

    qual=100.0*(u1-u2)
    thresh=(qual+10.0)*(u1-0.6)
    if(thresh.gt.4.0 .and. qual.gt.2.6 .and. u1.gt.0.77) then
!print *,'matched',thresh
!print *,msgsd76(ipk)
!      if(trim(msgsd76(ipk)).eq.'CQ DE AA00') return ! triggered by memory corruption?
      lft8sd=.true.; msg37=msgsd76(ipk); itone(1:79)=itone76(ipk,1:79); return
    endif
  endif

  return
end subroutine ft8mf1
