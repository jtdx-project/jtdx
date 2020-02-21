subroutine chkcrc14a(decoded,nbadcrc)

  use crc
  integer*1 decoded(91)
  integer*1, target:: i1Dec8BitBytes(12)
  character*91 cbits

! Write decoded bits into cbits: 77-bit message plus 14-bit CRC
  write(cbits,1000) decoded
1000 format(91i1)
  read(cbits,1001) i1Dec8BitBytes
1001 format(12b8)
  read(cbits,1002) ncrc14                         !Received CRC14
1002 format(77x,b14)

  i1Dec8BitBytes(10)=iand(i1Dec8BitBytes(10),transfer(128+64+32+16+8,0_1))
  i1Dec8BitBytes(11:12)=0
  icrc14=crc14(c_loc(i1Dec8BitBytes),12)          !CRC14 computed from 77 msg bits

  nbadcrc=1
  if(ncrc14.eq.icrc14) nbadcrc=0
  
  return
end subroutine chkcrc14a
