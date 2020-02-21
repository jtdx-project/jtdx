subroutine fmtmsg(msg,iz)

  character*22 msg

! Convert all letters to upper case
  iz=22
  do i=1,22
     if(msg(i:i).ge.'a' .and. msg(i:i).le.'z')                       &
          msg(i:i)= char(ichar(msg(i:i))+ichar('A')-ichar('a'))
     if(msg(i:i).ne.' ') iz=i
  enddo

  do iter=1,5                           !Collapse multiple blanks into one
     ib2=index(msg(1:iz),'  ')
     if(ib2.lt.1) go to 100
     msg=msg(1:ib2)//msg(ib2+2:)
     iz=iz-1
  enddo

100 return
end subroutine fmtmsg
