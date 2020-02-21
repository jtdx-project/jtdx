! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine hintrxrmin(s3,mrs,mrs2,mycall,decoded,hint_call3)

  use packjt
  use prog_args
  use jt65_mod2, only : s3,correct_hint
  parameter (MAXCALLS=35000,MAXRPT=30)
  parameter (MAXMSG=MAXRPT*MAXCALLS)
  real s3(64,63),u1
  integer*1 sym2(0:62,MAXMSG)
  integer mrs(63),mrs2(63),dgen(12),sym_rev(0:62)
  logical(1) hint_call3,first
  character*6 call2(MAXCALLS)
  character*4 rpt(MAXRPT)
  character mycall*6,callsign*12,line*180,msg*22,msg00*22,decoded*22
  character*22 msg0(MAXMSG)
  data first/.true./
  data rpt/'-01','-02','-03','-04','-05',          &
           '-06','-07','-08','-09','-10',          &
           '-11','-12','-13','-14','-15',          &
           '-16','-17','-18','-19','-20',          &
           '-21','-22','-23','-24','-25',          &
           '-26','-27','-28','-29','-30'/
  save first,nused,msg0,sym2

  if(first) then
     open(23,file=trim(data_dir)//'/CALL3.TXT',status='unknown')
     icall=0
     j=0
     do i=1,MAXCALLS
        read(23,1002,end=10) line
1002    format(a80)
        if(line(1:4).eq.'ZZZZ') exit
        if(line(1:2).eq.'//') cycle
        i1=index(line,',')
        if(i1.lt.4) cycle
        callsign=line(1:i1-1)
        j=j+1
        call2(j)=callsign(1:6)               !### Fix for compound callsigns!
     enddo
10   ncalls=j
     if(ncalls.lt.1) then
        print *, 'copy CALL3.TXT file to log directory'
        print *, 'http://www.jtdx.tech'
!        stop 'CALL3.TXT is too short or missed?'
     endif
     close(23)

     if(ncalls.eq.0) then
        first=.false.
        return
     endif

     j=0
     do i=1,ncalls
        do m=1,MAXRPT
           j=j+1
           msg=mycall//' '//call2(i)//' '//rpt(m)
           call fmtmsg(msg,iz)
           call packmsg(msg,dgen,itype)            !Pack message into 72 bits
           call rs_encode(dgen,sym_rev)            !RS encode
           call interleave63(sym_rev,1)            !Interleave channel symbols
           call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
           sym2(0:62,j)=sym_rev(0:62)
           msg0(j)=msg
        enddo
     enddo
     nused=j
     first=.false.
  endif

  hint_call3=.false.
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
  do k=1,nused
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
  thresh_h=(qual+8.0)*u1
  qmin=1.0

  if(qual.ge.1.0 .or. u1.ge.0.6) then !maximizing sensitivity using 'OR'
     if(thresh_h.gt.1.2) then !blocking some false decodes using 'AND'
        decoded=msg0(ipk)
        hint_call3=.true.
        correct_hint(1:63)=sym2(0:62,ipk)
     endif
  endif
return
end subroutine hintrxrmin
