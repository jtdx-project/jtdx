subroutine genft8(msg,i3,n3,ntxhash,msgsent,msgbits,itone)

! Encode an FT8 message, producing array itone().
  
  use packjt77
  use ft8_mod1, only : icos7,graymap
  character msg*37,msgsent*37,c77*77
  integer*1 msgbits(77),codeword(174)
  integer itone(79)
  logical unpk77_success

  i3=-1
  n3=-1
  call pack77(msg,i3,n3,c77,ntxhash)
  if(ntxhash.eq.1) then
    call unpack77(c77,0,msgsent,unpk77_success,26) !TX message, unpack to get msgsent, use TX hash tables and do not save RX hash values
  else
    call unpack77(c77,0,msgsent,unpk77_success,27) !service message, unpack to get msgsent. Do not save hash values
  endif
  read(c77,'(77i1)',err=1) msgbits
  if(unpk77_success) go to 2
1 msgbits=0
  itone=0
  msgsent='*** bad message ***                  '
  go to 900

entry get_tones_from_77bits(msgbits,itone) 

2  call encode174_91(msgbits,codeword)      !Encode the test message

! Message structure: S7 D29 S7 D29 S7
  itone(1:7)=icos7; itone(37:43)=icos7; itone(73:79)=icos7
  k=7
  do j=1,58
     i=3*j -2
     k=k+1
     if(j.eq.30) k=k+7
     indx=codeword(i)*4 + codeword(i+1)*2 + codeword(i+2)
     itone(k)=graymap(indx)
  enddo

900 return
end subroutine genft8
