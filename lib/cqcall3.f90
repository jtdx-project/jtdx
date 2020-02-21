! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine cqcall3()

  use jt65_mod10  ! used to share stored messages
  use packjt
  use prog_args
 
  integer dgen(12),sym_rev(0:62)
  character*6 call2(MAXCALLS)
  character*4 grid2(MAXCALLS)
  character line*180,callsign*12,grid*4,msg*22

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
        i2=index(line(i1+1:),',')
        if(i2.lt.5) cycle
        i2=i2+i1
        callsign=line(1:i1-1)
        grid=line(i1+1:i2-1)
        j=j+1
        call2(j)=callsign(1:6)               !### Fix for compound callsigns!
        grid2(j)=grid
     enddo
10   ncalls=j
     if(first .and. ncalls.lt.1) then
        print *, 'copy CALL3.TXT file to log directory'
        print *, 'http://www.jtdx.tech'
        first=.false.
!        stop 'CALL3.TXT is too short or missed?'
     endif
     close(23)

     if(ncalls.eq.0) then
        nused=0
        return
     endif
     j=0
     do i=1,ncalls
        j=j+1
        msg='CQ '//call2(i)//' '//grid2(i)
        call fmtmsg(msg,iz)
        call packmsg(msg,dgen,itype)            !Pack message into 72 bits
        call rs_encode(dgen,sym_rev)            !RS encode
        call interleave63(sym_rev,1)            !Interleave channel symbols
        call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
        sym2(0:62,j)=sym_rev(0:62)
        msg0(j)=msg
     enddo
     nused=j

return
end subroutine cqcall3
