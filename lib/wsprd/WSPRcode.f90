program wsprcode

! This program provides examples of the source encoding, convolutional
! error-control coding, bit and symbol ordering, and synchronizing
! information contained in WSPR messages.

  parameter (NSYM=162)
  parameter (MAXSYM=176)
  character*22 msg,msg2
  integer*1 data0(7)
  integer*1 data1(7)
  integer*1 dat(NSYM)
  integer*1 softsym(NSYM)

! Define the sync vector:
  integer*1 sync(NSYM)
  data sync/                                      &
       1,1,0,0,0,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,   &
       0,1,0,1,1,1,1,0,0,0,0,0,0,0,1,0,0,1,0,1,   &
       0,0,0,0,0,0,1,0,1,1,0,0,1,1,0,1,0,0,0,1,   &
       1,0,1,0,0,0,0,1,1,0,1,0,1,0,1,0,1,0,0,1,   &
       0,0,1,0,1,1,0,0,0,1,1,0,1,0,1,0,0,0,1,0,   &
       0,0,0,0,1,0,0,1,0,0,1,1,1,0,1,1,0,0,1,1,   &
       0,1,0,0,0,1,1,1,0,0,0,0,0,1,0,1,0,0,1,1,   &
       0,0,0,0,0,0,0,1,1,0,1,0,1,1,0,0,0,1,1,0,   &
       0,0/

! Metric table for decoding from soft symbols
  integer mettab(0:255,0:1)
  data mettab/                                            &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   4,   &
         4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   &
         4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   &
         3,   3,   3,   3,   3,   3,   3,   3,   3,   2,   &
         2,   2,   2,   2,   1,   1,   1,   1,   0,   0,   &
        -1,  -1,  -1,  -2,  -2,  -3,  -4,  -4,  -5,  -6,   &
        -7,  -7,  -8,  -9, -10, -11, -12, -12, -13, -14,   &
       -15, -16, -17, -17, -18, -19, -20, -21, -22, -22,   &
       -23, -24, -25, -26, -26, -27, -28, -29, -30, -30,   &
       -31, -32, -33, -33, -34, -35, -36, -36, -37, -38,   &
       -38, -39, -40, -41, -41, -42, -43, -43, -44, -45,   &
       -45, -46, -47, -47, -48, -49, -49, -50, -51, -51,   &
       -52, -53, -53, -54, -54, -55, -56, -56, -57, -57,   &
       -58, -59, -59, -60, -60, -61, -62, -62, -62, -63,   &
       -64, -64, -65, -65, -66, -67, -67, -67, -68, -69,   &
       -69, -70, -70, -71, -72, -72, -72, -72, -73, -74,   &
       -75, -75, -75, -77, -76, -76, -78, -78, -80, -81,   &
       -80, -79, -83, -82, -81, -82, -82, -83, -84, -84,   &
       -84, -87, -86, -87, -88, -89, -89, -89, -88, -87,   &
       -86, -87, -84, -84, -84, -83, -82, -82, -81, -82,   &
       -83, -79, -80, -81, -80, -78, -78, -76, -76, -77,   &
       -75, -75, -75, -74, -73, -72, -72, -72, -72, -71,   &
       -70, -70, -69, -69, -68, -67, -67, -67, -66, -65,   &
       -65, -64, -64, -63, -62, -62, -62, -61, -60, -60,   &
       -59, -59, -58, -57, -57, -56, -56, -55, -54, -54,   &
       -53, -53, -52, -51, -51, -50, -49, -49, -48, -47,   &
       -47, -46, -45, -45, -44, -43, -43, -42, -41, -41,   &
       -40, -39, -38, -38, -37, -36, -36, -35, -34, -33,   &
       -33, -32, -31, -30, -30, -29, -28, -27, -26, -26,   &
       -25, -24, -23, -22, -22, -21, -20, -19, -18, -17,   &
       -17, -16, -15, -14, -13, -12, -12, -11, -10,  -9,   &
        -8,  -7,  -7,  -6,  -5,  -4,  -4,  -3,  -2,  -2,   &
        -1,  -1,  -1,   0,   0,   1,   1,   1,   1,   2,   &
         2,   2,   2,   2,   3,   3,   3,   3,   3,   3,   &
         3,   3,   3,   4,   4,   4,   4,   4,   4,   4,   &
         4,   4,   4,   4,   4,   4,   4,   4,   4,   4,   &
         4,   4,   4,   4,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5,   5,   5,   5,   5,   5,   5,   5,   5,   &
         5,   5/

! Get command-line argument(s)
  nargs=iargc()
  if(nargs.ne.1) then
     print*,'Usage: WSPRcode "message"'
     go to 999
  endif
  call getarg(1,msg)                             !Get message from command line
  write(*,1000) msg
1000 format('Message: ',a22)

  nbits=50+31               !User bits=50, constraint length=32
  nbytes=(nbits+7)/8
  ndelta=50
  limit=20000

  data0=0
  call wqencode(msg,ntype0,data0)             !Source encoding
  write(*,1002) data0
1002 format(/'Source-encoded message (50 bits, hex):',7z3.2)

  call encode232(data0,nbytes,dat,MAXSYM)     !Convolutional encoding
  call inter_mept(dat,1)                      !Interleaving

  write(*,1004)
1004 format(/'Data symbols:')
  write(*,1006) (dat(i),i=1,NSYM)
1006 format(5x,30i2)

  write(*,1008)
1008 format(/'Sync symbols:')
  write(*,1006) (sync(i),i=1,NSYM)

  write(*,1010)
1010 format(/'Channel symbols:')
  write(*,1006) (2*dat(i)+sync(i),i=1,NSYM)

  call inter_mept(dat,-1)                     !Remove interleaving
  softsym=-dat                                !Simulate soft symbols

! Call the sequential (Fano algorithm) decoder
  call fano232(softsym,nbits,mettab,ndelta,limit,data1,ncycles,metric,nerr)
  call wqdecode(data1,msg2,ntype1)         

  write(*,1020) ntype1
1020 format(/'Message type: ',i7)
  write(*,1030) msg2
1030 format('Decoded message:  ',a22)

999 end program wsprcode
