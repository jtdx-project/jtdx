program test_wspr

! This program provides examples of the source encoding, convolutional
! error-control coding, bit and symbol ordering, and synchronizing
! information contained in WSPR messages.

  character*22 msg,msg2
  character*23 msg3
  character*1 err2,err3
  integer*1 data0(11)
  logical lfile

! Get command-line argument(s)
  nargs=iargc()
  if(nargs.ne.1) then
     print*,'Usage: test_wspr "message"'
     go to 999
  endif
  call getarg(1,msg)                             !Get message from command line
  call unpk(data0,1,msg3)                !Read the C hashtable
  lfile=msg(1:2).eq."-t"
  if(lfile) open(10,file="messages.txt",status="old")

  do imsg=1,999
     if(lfile) read(10,1001,end=900) msg
1001 format(a22)

     data0=0
     call wqencode(msg,ntype0,data0)             !Source encoding
!     write(*,1002) data0(1:7)
!1002 format('Source-encoded message (50 bits, hex):',7z3.2)
!     data0(8:11)=0

     call wqdecode(data0,msg2,ntype1)         

!  write(*,1020) ntype1
!1020 format('Message type: ',i7)
!  write(*,1030) msg2
!1030 format('Decoded message:  ',a22)

     call unpk(data0,0,msg3)
     do i=1,23
        if(ichar(msg3(i:i)).eq.0) then
           msg3(i:)="                      "
           exit
        endif
     enddo

     err2=' '
     err3=' '
     if(msg2.ne.msg) err2='*'
     if(msg3.ne.msg) err3='*'

     write(*,1040) msg,err2,msg2,err3,msg3
1040 format(a22,1x,a1,1x,a22,1x,a1,1x,a22)
     if(.not.lfile) exit
  enddo
900 call unpk(data0,2,msg3)


999 end program test_wspr
