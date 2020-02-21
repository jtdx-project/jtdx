subroutine genwspr(message,msgsent,itone)
! Encode a WSPR message and generate the array of channel symbols.

  character*22 message,msgsent
  parameter (MAXSYM=176)
  integer*1 symbol(MAXSYM)
  integer*1 data0(11)
  integer*4 itone(162)
  integer npr3(162)
  data npr3/                                      &
      1,1,0,0,0,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,    &
      0,1,0,1,1,1,1,0,0,0,0,0,0,0,1,0,0,1,0,1,    &
      0,0,0,0,0,0,1,0,1,1,0,0,1,1,0,1,0,0,0,1,    &
      1,0,1,0,0,0,0,1,1,0,1,0,1,0,1,0,1,0,0,1,    &
      0,0,1,0,1,1,0,0,0,1,1,0,1,0,1,0,0,0,1,0,    &
      0,0,0,0,1,0,0,1,0,0,1,1,1,0,1,1,0,0,1,1,    &
      0,1,0,0,0,1,1,1,0,0,0,0,0,1,0,1,0,0,1,1,    &
      0,0,0,0,0,0,0,1,1,0,1,0,1,1,0,0,0,1,1,0,    &
      0,0/

  call wqencode(message,ntype,data0)          !Source encoding
  nbytes=(50+31+7)/8
  call encode232(data0,162,symbol)            !Convolutional encoding
  call inter_wspr(symbol,1)                   !Interleaving
  do i=1,162
     itone(i)=npr3(i) + 2*symbol(i)
  enddo
  msgsent=message                             !### To be fixed... ?? ###

  return
end subroutine genwspr
