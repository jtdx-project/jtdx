subroutine encode174_91(message77,codeword)
!
! Add a 14-bit CRC to a 77-bit message and return a 174-bit codeword
!
use, intrinsic :: iso_c_binding
use iso_c_binding, only: c_loc,c_size_t
use crc

integer, parameter:: N=174, K=91, M=N-K
character*91 tmpchar
integer*1 codeword(N)
integer*1 gen(M,K)
integer*1 message77(77),message(K)
integer*1 pchecks(M)
integer*1, target :: i1MsgBytes(12)
include "ldpc_174_91_c_generator.f90"
logical first
data first/.true./
save first,gen

if( first ) then ! fill the generator matrix
  gen=0
  do i=1,M
    do j=1,23
      read(g(i)(j:j),"(Z1)") istr
        ibmax=4
        if(j.eq.23) ibmax=3 
        do jj=1, ibmax 
          icol=(j-1)*4+jj
          if( btest(istr,4-jj) ) gen(i,icol)=1
        enddo
    enddo
  enddo
first=.false.
endif

! Add 14-bit CRC to form 91-bit message+CRC14
write(tmpchar,'(77i1)') message77
tmpchar(78:80)='000'
i1MsgBytes=0
read(tmpchar,'(10b8)') i1MsgBytes(1:10)
ncrc14 = crc14 (c_loc (i1MsgBytes), 12)
write(tmpchar(78:91),'(b14)') ncrc14
read(tmpchar,'(91i1)') message

do i=1,M
  nsum=0
  do j=1,K 
    nsum=nsum+message(j)*gen(i,j)
  enddo
  pchecks(i)=mod(nsum,2)
enddo

codeword(1:K)=message
codeword(K+1:N)=pchecks

return
end subroutine encode174_91
