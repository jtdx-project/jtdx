subroutine chkmsg(message,cok,nspecial,flip)

  character message*22,cok*3

  nspecial=0
  flip=1.0
  cok="   "

  do i=22,1,-1
     if(message(i:i).ne.' ') go to 10
  enddo
  i=22

10 if(i.ge.11) then
     if((message(i-3:i).eq.' OOO') .or. (message(20:22).eq.' OO')) then
        cok='OOO'
        flip=-1.0
        if(message(20:22).eq.' OO') then
           message=message(1:19)
        else
           message=message(1:i-4)
        endif
     endif
  endif

  if(message(1:3).eq.'RO ')  nspecial=2
  if(message(1:4).eq.'RRR ') nspecial=3
  if(message(1:3).eq.'73 ')  nspecial=4

  return
end subroutine chkmsg
