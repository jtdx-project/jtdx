! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine gen65(msg0,ichk,msgsent,itone,itype)

! Encodes a JT65 message to yieild itone(1:126)

  use packjt
  use jt65_mod4
  character*22 msg0
  character*22 message          !Message to be generated
  character*22 msgsent          !Message as it will be received
  integer itone(126)
  integer dgen(13)
  integer sent(63)
  save

  if(msg0(1:1).eq.'@') then
     read(msg0(2:5),*,end=1,err=1) nfreq
     go to 2
1    nfreq=1000
2    itone(1)=nfreq
  else
     message=msg0
     do i=1,22
        if(ichar(message(i:i)).eq.0) then
           message(i:)='                      '
           exit
        endif
     enddo

     do i=1,22                               !Strip leading blanks
        if(message(1:1).ne.' ') exit
        message=message(i+1:)
     enddo

        call packmsg(message,dgen,itype)    !Pack message into 72 bits
        call unpackmsg(dgen,msgsent)        !Unpack to get message sent
        if(ichk.ne.0) go to 999             !Return if checking only

        call rs_encode(dgen,sent)           !Apply Reed-Solomon code
        call interleave63(sent,1)           !Apply interleaving
        call graycode65(sent,63,1)          !Apply Gray code
        nsym=126                            !Symbols per transmission

     k=0
     do j=1,nsym
        if(.not.prc(j)) then
           k=k+1
           itone(j)=sent(k)+2
        else
           itone(j)=0
        endif
     enddo
  endif

999 return
end subroutine gen65
