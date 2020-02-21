! This source code file was last time modified by Igor UA3DJY on February 15th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

module jt65_mod10 
! used to store last callsigns seen in JT modes in the memory
! used in some Hint decoders and for false FTRSD decodes checking

  parameter (MAXCALLS=35000)  ! max 34999 calls in CALL3.TXT
  parameter (MAXMSG=MAXCALLS) ! in chkftrsd MAXMSG is equal to CALL3 calls +1
  integer*1 sym2(0:62,MAXMSG)
  integer nused
  logical(1) first
  character*22 msg0(MAXMSG)
  data first/.true./

  save first,nused,msg0,sym2

end module jt65_mod10