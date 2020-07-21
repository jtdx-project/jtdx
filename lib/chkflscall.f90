subroutine chkflscall(call_a,call_b,falsedec)

  character*12 call_a,call_b
  logical(1) falsedec,lfound

  if(call_a(1:2).eq.'<.') return
  falsedec=.true.; lfound=.false.
!print *,"11","'"//call_a//"'","'"//call_b//"'"
  if(call_a.eq.'MYCALL      ' .or. call_a.eq.'CQ          ') then
    call searchcalls(call_b,"            ",lfound); if(lfound) falsedec=.false.
  else if(call_b(1:1).eq.'<') then
    call searchcalls(call_a,"            ",lfound); if(lfound) falsedec=.false.
  else
    call searchcalls(call_a,call_b,lfound); if(lfound) falsedec=.false.
  endif

  return
end subroutine chkflscall