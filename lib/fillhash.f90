subroutine fillhash(numthreads,lfill)

  use packjt77
  integer, intent(in) :: numthreads
  logical, intent(in) :: lfill
  character*13 cw

  if(lfill) then
    do i=1,numthreads
      do m=1,nlast_calls(i)
        nposition=nthrindex(i)+m
        cw=last_calls(nposition)
!print *,i,m,cw
        n10=ihashcall(cw,10)
        if(n10.ge.0 .and. n10 .le. 1023 .and. cw.ne.mycall13) calls10(n10)=cw

        n12=ihashcall(cw,12)
        if(n12.ge.0 .and. n12 .le. 4095 .and. cw.ne.mycall13) calls12(n12)=cw

        n22=ihashcall(cw,22)
        if(any(ihash22.eq.n22)) then   ! If entry exists, make sure callsign is the most recently received one 
          where(ihash22.eq.n22) calls22=cw
          go to 900
        endif

! New entry: move table down, making room for new one at the top
        ihash22(nzhash:2:-1)=ihash22(nzhash-1:1:-1)

! Add the new entry
        calls22(nzhash:2:-1)=calls22(nzhash-1:1:-1)
        ihash22(1)=n22
        calls22(1)=cw
        if(nzhash.lt.MAXHASH) nzhash=nzhash+1
900     continue
      enddo
    enddo
  else
    nlast_calls=0
  endif
  
  return
end subroutine fillhash
