! This source code file was created from hint65.f90 source code and last time 
! modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine hintdx(s3,mrs,mrs2,mycall,hiscall,hisgrid,decoded,hint_dx)

  use packjt
  use jt65_mod2
  parameter (MAXRPT=63)
  parameter (MAXMSG=MAXRPT+2)
  real s3(64,63),u1
  integer mrs(63),mrs2(63),dgen(12),sym_rev(0:62),sym2(0:62,MAXMSG)
  logical(1) hint_dx,hint3
  character mycall*6,hiscall*6,hisgrid*4,msg*22,msg00*22,decoded*22
  character*4 rpt(MAXRPT)
  character*22 msg0(MAXMSG)

  data rpt/'-01','-02','-03','-04','-05',          &
           '-06','-07','-08','-09','-10',          &
           '-11','-12','-13','-14','-15',          &
           '-16','-17','-18','-19','-20',          &
           '-21','-22','-23','-24','-25',          &
           '-26','-27','-28','-29','-30',          &
           'R-01','R-02','R-03','R-04','R-05',     &
           'R-06','R-07','R-08','R-09','R-10',     &
           'R-11','R-12','R-13','R-14','R-15',     &
           'R-16','R-17','R-18','R-19','R-20',     &
           'R-21','R-22','R-23','R-24','R-25',     &
           'R-26','R-27','R-28','R-29','R-30',     &
           'RRR','73','RR73'/
  save msg0,sym2

  hint_dx=.false.
  do i=1,6
   if(hiscall(i:i).eq.'/') return
  enddo
  
        do m=1,MAXMSG
           if(m.eq.1) msg=mycall//' '//hiscall//' '//hisgrid
           if(m.eq.2) msg='CQ '//hiscall//' '//hisgrid
           if(m.ge.3) msg=mycall//' '//hiscall//' '//rpt(m-2)
           call fmtmsg(msg,iz)
           call packmsg(msg,dgen,itype)            !Pack message into 72 bits
           call rs_encode(dgen,sym_rev)            !RS encode
           call interleave63(sym_rev,1)            !Interleave channel symbols
           call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
           sym2(0:62,m)=sym_rev(0:62)
           msg0(m)=msg
        enddo

  ref0=0.
  do j=1,63
     ref0=ref0 + s3(mrs(j)+1,j)
  enddo

  u1=0.
  u1=-99.0
  u2=u1

! Find u1 and u2 (best and second-best) codeword from a list, using 
! a bank of matched filters on the symbol spectra s3(i,j).
  ipk=1
  ipk2=0
  msg00='                      '
  do k=1,MAXMSG
        psum=0.
        ref=ref0
        do j=1,63
           i=sym2(j-1,k)+1
           psum=psum + s3(i,j)
           if(i.eq.mrs(j)+1) ref=ref - s3(i,j) + s3(mrs2(j)+1,j)
        enddo
        p=psum/ref

        if(p.gt.u1) then
           if(msg0(k).ne.msg00) then
              ipk2=ipk
              u2=u1
           endif
           u1=p
           ipk=k
           msg00=msg0(k)
        endif
        if(msg0(k).ne.msg00 .and. p.gt.u2) then
           u2=p
           ipk2=k
        endif
  enddo

  decoded='                      '
  bias=max(1.12*u2,0.35)
  qual=100.0*(u1-bias)
  hint3=.false. 
  qmin=0.0 !may be set -2.0 in preamp mode
  if(msg0(ipk)(1:3).eq.'CQ ') qmin=-1.2
  if(msg0(ipk)(1:3).eq.'CQ ' .and. u1.ge.0.524) hint3=.true.
  if(qual.ge.qmin .or. hint3) then
    decoded=msg0(ipk)
    hint_dx=.true.
    correct_hint(1:63)=sym2(0:62,ipk)
  endif 
  return
end subroutine hintdx
