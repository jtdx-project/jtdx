! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

program JT65code

! Provides examples of message packing, bit and symbol ordering,
! Reed Solomon encoding, and other necessary details of the JT65
! protocol.

  use packjt
  character*22 msg,msg0,msg1,decoded,cok*3,bad*1,msgtype*10
  integer dgen(12),sent(63),tmp(63),recd(12),era(51)
  include 'testmsg.f90'

  nargs=iargc()
  if(nargs.ne.1) then
     print*,'Usage: jt65code "message"'
     print*,'       jt65code -t'
     go to 999
  endif

  call getarg(1,msg)                     !Get message from command line
  nmsg=1
  if(msg(1:2).eq."-t") then
     testmsg(NTEST+1)="KA1ABC WB9XYZ EN34 OOO"
     testmsg(NTEST+2)="KA1ABC WB9XYZ OOO"
     testmsg(NTEST+3)="RO"
     testmsg(NTEST+4)="RRR"
     testmsg(NTEST+5)="73"
     nmsg=NTEST+5
  endif

  write(*,1010)
1010 format("     Message                 Decoded                Err? Type"/   &
            74("-"))

  do imsg=1,nmsg
     if(nmsg.gt.1) msg=testmsg(imsg)

     call fmtmsg(msg,iz)                    !To upper, collapse mult blanks
     msg0=msg                               !Input message
     call chkmsg(msg,cok,nspecial,flip)     !See if it includes "OOO" report
     msg1=msg                               !Message without "OOO"

     if(nspecial.gt.0) then                  !or is a shorthand message
        if(nspecial.eq.2) decoded="RO"
        if(nspecial.eq.3) decoded="RRR"
        if(nspecial.eq.4) decoded="73"
        itype=-1
        msgtype="Shorthand"
        go to 10
     endif

     call packmsg(msg1,dgen,itype)           !Pack message into 12 six-bit bytes
     msgtype=""
     if(itype.eq.1) msgtype="Std Msg"
     if(itype.eq.2) msgtype="Type 1 pfx"
     if(itype.eq.3) msgtype="Type 1 sfx"
     if(itype.eq.4) msgtype="Type 2 pfx"
     if(itype.eq.5) msgtype="Type 2 sfx"
     if(itype.eq.6) msgtype="Free text"

     call rs_encode(dgen,sent)               !RS encode
     call interleave63(sent,1)               !Interleave channel symbols
     call graycode(sent,63,1,sent)           !Apply Gray code

     call graycode(sent,63,-1,tmp)           !Remove Gray code
     call interleave63(tmp,-1)               !Remove interleaving
     call rs_decode(tmp,era,0,recd,nerr)     !Decode the message
     call unpackmsg(recd,decoded)            !Unpack the user message
     if(cok.eq."OOO") decoded(20:22)=cok
     call fmtmsg(decoded,iz)

10     bad=" "
     if(decoded.ne.msg0) bad="*"
     write(*,1020) imsg,msg0,decoded,bad,itype,msgtype
1020 format(i2,'.',2x,a22,2x,a22,3x,a1,i3,": ",a13)
  enddo

  if(nmsg.eq.1 .and. nspecial.eq.0) then
     write(*,1030) dgen
1030 format(/'Packed message, 6-bit symbols ',12i3) !Display packed symbols

     write(*,1040) sent
1040 format(/'Information-carrying channel symbols'/(i5,20i3))
  endif

999 end program JT65code
