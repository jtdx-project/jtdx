! This file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine hintdyn(mrs,mrs2,call1,call2,grid2,decoded,hint_dyn,inverse,nlastrx)

  use packjt
  use jt65_mod2, only : s3,correct_hint
  parameter (MAXRPT=33,IMAXRPT=30,IMAXRPT73=3)
  parameter (MAXMSG=MAXRPT)
  real u1
  integer nlastrx
  integer sym2(0:62,MAXMSG),mrs(63),mrs2(63),dgen(12),sym_rev(0:62)
  logical(1) hint_dyn,inverse
  character grid2*4,call1*6,call2*6,msg*22,msg1*22,msg00*22,decoded*22
  character*3 irpt(IMAXRPT)
  character*4 irrpt(IMAXRPT),i73(IMAXRPT73),rpt(MAXRPT),rrpt(MAXRPT),rrptgrid((MAXRPT-1))
  character*22 msg0(MAXMSG)



  data irpt/'-01','-02','-03','-04','-05',         &
           '-06','-07','-08','-09','-10',          &
           '-11','-12','-13','-14','-15',          &
           '-16','-17','-18','-19','-20',          &
           '-21','-22','-23','-24','-25',          &
           '-26','-27','-28','-29','-30'/

  data irrpt/'R-01','R-02','R-03','R-04','R-05',   &
            'R-06','R-07','R-08','R-09','R-10',    &
            'R-11','R-12','R-13','R-14','R-15',    &
            'R-16','R-17','R-18','R-19','R-20',    &
            'R-21','R-22','R-23','R-24','R-25',    &
            'R-26','R-27','R-28','R-29','R-30'/

  data i73/'RRR','73','RR73'/

  data rpt/'-01','-02','-03','-04','-05',          &
           '-06','-07','-08','-09','-10',          &
           '-11','-12','-13','-14','-15',          &
           '-16','-17','-18','-19','-20',          &
           '-21','-22','-23','-24','-25',          &
           '-26','-27','-28','-29','-30',          &
           'RRR','73','RR73'/

  data rrpt/'R-01','R-02','R-03','R-04','R-05',    &
            'R-06','R-07','R-08','R-09','R-10',    &
            'R-11','R-12','R-13','R-14','R-15',    &
            'R-16','R-17','R-18','R-19','R-20',    &
            'R-21','R-22','R-23','R-24','R-25',    &
            'R-26','R-27','R-28','R-29','R-30',    &
            'RRR','73','RR73'/

  data rrptgrid/'R-01','R-02','R-03','R-04','R-05',&
            'R-06','R-07','R-08','R-09','R-10',    &
            'R-11','R-12','R-13','R-14','R-15',    &
            'R-16','R-17','R-18','R-19','R-20',    &
            'R-21','R-22','R-23','R-24','R-25',    &
            'R-26','R-27','R-28','R-29','R-30',    &
            'HQ02','HQ03'/

   save irpt,irrpt,i73,rpt,rrpt,rrptgrid

! nlastrx last received message matrix, used for wideband hintdyn decoder, dynhint(300) structure
! nlastrx   last RX message   / dinterval1 expected inversed msg / dinterval2 expected RX message
!   0           initialization/               none              / none
!   1           GRID          /               -01               / R-01, and might be again GRID
!   2           -01           /               R-01              / RRR/RR73/73, and might be again -01
!   3           R-01          /               RRR/RR73/73       / RRR/RR73/73,  and might be again R-01
!   4           RRR/RR73/73   /               RRR/RR73/73       / none


  hint_dyn=.false.
 
  if(nlastrx.lt.1 .or. nlastrx.gt.4) return

  do i=1,6
     if(call1(i:i).eq.'/') return
     if(call2(i:i).eq.'/') return
  enddo

  if(inverse) then
     if(nlastrx.eq.1) then ! expected -01 message
        do m=1,IMAXRPT
          msg=trim(call1)//' '//trim(call2)//' '//irpt(m)
          call fmtmsg(msg,iz)
          call packmsg(msg,dgen,itype)            !Pack message into 72 bits
          call rs_encode(dgen,sym_rev)            !RS encode
          call interleave63(sym_rev,1)            !Interleave channel symbols
          call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
          sym2(0:62,m)=sym_rev(0:62)
          msg0(m)=msg
        enddo
     endif

     if(nlastrx.eq.2) then! expected R-01 message
        do m=1,IMAXRPT
          msg=trim(call1)//' '//trim(call2)//' '//irrpt(m)
          call fmtmsg(msg,iz)
          call packmsg(msg,dgen,itype)            !Pack message into 72 bits
          call rs_encode(dgen,sym_rev)            !RS encode
          call interleave63(sym_rev,1)            !Interleave channel symbols
          call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
          sym2(0:62,m)=sym_rev(0:62)
          msg0(m)=msg
        enddo
     endif
  
     if(nlastrx.ge.3) then! expected RRR/RR73/73 message
        do m=1,IMAXRPT73
          msg=trim(call1)//' '//trim(call2)//' '//i73(m)
          call fmtmsg(msg,iz)
          call packmsg(msg,dgen,itype)            !Pack message into 72 bits
          call rs_encode(dgen,sym_rev)            !RS encode
          call interleave63(sym_rev,1)            !Interleave channel symbols
          call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
          sym2(0:62,m)=sym_rev(0:62)
          msg0(m)=msg
        enddo

       do m=4,IMAXRPT73+7
          msg='UF0XYZ W9XYZ '//'EN3'//achar(47+m)
          call fmtmsg(msg,iz)
          call packmsg(msg,dgen,itype)            !Pack message into 72 bits
          call rs_encode(dgen,sym_rev)            !RS encode
          call interleave63(sym_rev,1)            !Interleave channel symbols
          call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
          sym2(0:62,m)=sym_rev(0:62)
          msg0(m)=msg
       enddo
     endif
  endif


  if(.not.inverse) then
     if(nlastrx.lt.1 .or. nlastrx.gt.3) return

     if(nlastrx.eq.1) then ! expected R-01, and might be again GRID message
        do m=1,MAXMSG
          if(m.eq.1) msg=trim(call1)//' '//trim(call2)//' '//grid2
          if(m.ge.2) msg=trim(call1)//' '//trim(call2)//' '//rrptgrid(m-1)
          call fmtmsg(msg,iz)
          call packmsg(msg,dgen,itype)            !Pack message into 72 bits
          call rs_encode(dgen,sym_rev)            !RS encode
          call interleave63(sym_rev,1)            !Interleave channel symbols
          call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
          sym2(0:62,m)=sym_rev(0:62)
          msg0(m)=msg
        enddo
     endif

     if(nlastrx.eq.2) then! expected RRR/RR73/73, and might be again -01 message
        do m=1,MAXMSG
          msg=trim(call1)//' '//trim(call2)//' '//rpt(m)
          call fmtmsg(msg,iz)
          call packmsg(msg,dgen,itype)            !Pack message into 72 bits
          call rs_encode(dgen,sym_rev)            !RS encode
          call interleave63(sym_rev,1)            !Interleave channel symbols
          call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
          sym2(0:62,m)=sym_rev(0:62)
          msg0(m)=msg
        enddo
     endif
  
     if(nlastrx.eq.3) then! expected RRR/RR73/73,  and might be again R-01 message
        do m=1,MAXMSG
          msg=trim(call1)//' '//trim(call2)//' '//rrpt(m)
          call fmtmsg(msg,iz)
          call packmsg(msg,dgen,itype)            !Pack message into 72 bits
          call rs_encode(dgen,sym_rev)            !RS encode
          call interleave63(sym_rev,1)            !Interleave channel symbols
          call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
          sym2(0:62,m)=sym_rev(0:62)
          msg0(m)=msg
        enddo
     endif
  endif

  ref0=0.
  do i=1,63
     do j=1,63
        ref0=ref0 + s3(mrs(j)+1,i)
     enddo
  enddo

  u1=-99.0
  u2=u1

! Find u1 and u2 (best and second-best) codeword from a list, using 
! a bank of matched filters on the symbol spectra s3(i,j).
  ipk=1
  msg00='                      '

  if(inverse) then
     if(nlastrx.le.2) maxmsgs=IMAXRPT
     if(nlastrx.ge.3) maxmsgs=IMAXRPT73+7
  endif
  if(.not.inverse) then
     maxmsgs=MAXMSG
  endif

  do k=1,maxmsgs
     psum=0.
     ref=ref0
     do j=1,63
        i=sym2(j-1,k)+1
        psum=psum + s3(i,j)
        if(i.eq.mrs(j)+1) ref=ref - s3(i,j) + s3(mrs2(j)+1,j)
     enddo
     p=psum*63.0/ref

     if(p.gt.u1) then
        if(msg0(k).ne.msg00) u2=u1
        u1=p
        ipk=k
        msg00=msg0(k)
     endif
     if(msg0(k).ne.msg00 .and. p.gt.u2) u2=p
  enddo

  decoded='                      '

  msg1=msg0(ipk)
!print*, inverse, msg1
  if(.not.inverse) then
     do i=9,15
        if(msg1(i:i+2).eq.'HQ0') go to 4
     enddo
  endif

  if(inverse .and. nlastrx.gt.2 .and. msg1(1:6).eq."UF0XYZ") go to 4

  bias=max(1.12*u2,0.35)
  qual=100.0*(u1-bias)
  thresh=(qual+20.0)*(u1-1.0)
  qmin=-2.0
  if(inverse) qmin=0.0
  
  
!if(.not.inverse .and. nlastrx.eq.3) then
!write(6,1010) qual,u1,thresh
!1010 format(f7.2,1x,f6.2,1x,f6.2)
!print *,msg0(ipk)
!endif

  if(inverse .and. nlastrx.gt.2) then
     if(qual.ge.8.0 .or. u1.ge.1.33) then
        decoded=msg0(ipk)
        hint_dyn=.true.
!write(6,1010) qual,u1,thresh
!1010 format(f6.2,1x,f5.3,1x,f5.2)
        correct_hint(1:63)=sym2(0:62,ipk)
     endif
  else
     if((qual.ge.qmin .or. u1.ge.1.4) .and. thresh.gt.6.5) then
        decoded=msg0(ipk)
        hint_dyn=.true.
!write(6,1010) qual,u1,thresh
!1010 format(f6.2,1x,f5.3,1x,f5.2)
        correct_hint(1:63)=sym2(0:62,ipk)
     endif
  endif

4 return
end subroutine hintdyn