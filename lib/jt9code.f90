program jt9code

! Generate simulated data for testing of WSJT-X

  character msg*22,decoded*22,bad*1,msgtype*13
  integer*4 i4tone(85)                     !Channel symbols (values 0-8)
  include 'testmsg.f90'
  include 'jt9sync.f90'

  nargs=iargc()
  if(nargs.ne.1) then
     print*,'Usage: jt9code "message"'
     print*,'       jt9code -t'
     go to 999
  endif

  call getarg(1,msg)
  nmsg=1
  if(msg(1:2).eq."-t") nmsg=NTEST

  write(*,1010)
1010 format("     Message                 Decoded                Err? Type"/   &
            74("-"))
  do imsg=1,nmsg
     if(nmsg.gt.1) msg=testmsg(imsg)
     call fmtmsg(msg,iz)                !To upper case, collapse multiple blanks
     ichk=0
     call gen9(msg,ichk,decoded,i4tone,itype)   !Encode message into tone #s

     msgtype=""
     if(itype.eq.1) msgtype="Std Msg"
     if(itype.eq.2) msgtype="Type 1 prefix"
     if(itype.eq.3) msgtype="Type 1 suffix"
     if(itype.eq.4) msgtype="Type 2 prefix"
     if(itype.eq.5) msgtype="Type 2 suffix"
     if(itype.eq.6) msgtype="Free text"

     bad=" "
     if(decoded.ne.msg) bad="*"
     write(*,1020) imsg,msg,decoded,bad,itype,msgtype
1020 format(i2,'.',2x,a22,2x,a22,3x,a1,i3,": ",a13)
  enddo

  if(nmsg.eq.1) write(*,1030) i4tone
1030 format(/'Channel symbols'/(30i2))

999 end program jt9code
