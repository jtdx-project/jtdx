subroutine genft4(msg0,ichk,msgsent,msgbits,i4tone)

! Encode an FT4  message
! Input:
!   - msg0     requested message to be transmitted
!   - ichk     if ichk=1, return only msgsent
!   - msgsent  message as it will be decoded
!   - i4tone   array of audio tone values, {0,1,2,3} 

! Frame structure:
! s16 + 87symbols + 2 ramp up/down = 105 total channel symbols
! r1 + s4 + d29 + s4 + d29 + s4 + d29 + s4 + r1

! Message duration: TxT = 105*576/12000 = 5.04 s
  
! use iso_c_binding, only: c_loc,c_size_t

  use packjt77
  include 'ft4_params.f90'  
  character*37 msg0
  character*37 message                    !Message to be generated
  character*37 msgsent                    !Message as it will be received
  character*77 c77
  integer*4 i4tone(NN),itmp(ND)
  integer*1 codeword(2*ND)
  integer*1 msgbits(77),rvec(77) 
  integer icos4a(4),icos4b(4),icos4c(4),icos4d(4)
  logical unpk77_success
  data icos4a/0,1,3,2/
  data icos4b/1,0,2,3/
  data icos4c/2,3,1,0/
  data icos4d/3,2,0,1/
  data rvec/0,1,0,0,1,0,1,0,0,1,0,1,1,1,1,0,1,0,0,0,1,0,0,1,1,0,1,1,0, &
            1,0,0,1,0,1,1,0,0,0,0,1,0,0,0,1,0,1,0,0,1,1,1,1,0,0,1,0,1, &
            0,1,0,1,0,1,1,0,1,1,1,1,1,0,0,0,1,0,1/
  message=msg0

  do i=1, 37
     if(ichar(message(i:i)).eq.0) then
        message(i:37)=' '
        exit
     endif
  enddo
  do i=1,37                               !Strip leading blanks
     if(message(1:1).ne.' ') exit
     message=message(i+1:)
  enddo

  i3=-1
  n3=-1
  call pack77(message,i3,n3,c77)
  call unpack77(c77,0,msgsent,unpk77_success) !Unpack to get msgsent

  if(ichk.eq.1) go to 999
  read(c77,'(77i1)',err=1) msgbits
  if(unpk77_success) go to 2
1 msgbits=0
  itone=0
  msgsent='*** bad message ***                  '
  go to 999

entry get_ft4_tones_from_77bits(msgbits,i4tone)

2 msgbits=mod(msgbits+rvec,2)
  call encode174_91(msgbits,codeword)

! Grayscale mapping:
! bits   tone
! 00     0
! 01     1
! 11     2
! 10     3

  do i=1,ND
    is=codeword(2*i)+2*codeword(2*i-1)
    if(is.le.1) itmp(i)=is
    if(is.eq.2) itmp(i)=3
    if(is.eq.3) itmp(i)=2
  enddo

  i4tone(1:4)=icos4a
  i4tone(5:33)=itmp(1:29)
  i4tone(34:37)=icos4b
  i4tone(38:66)=itmp(30:58)
  i4tone(67:70)=icos4c
  i4tone(71:99)=itmp(59:87)
  i4tone(100:103)=icos4d

999 return
end subroutine genft4
