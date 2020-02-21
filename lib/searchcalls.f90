! last time modified by Igor UA3DJY on 20200216

subroutine searchcalls(callsign1,callsign2,lfound)
  
  use jt65_mod9, only : callsign,ncall0c,ncalld,ncallef,ncallgh,ncalli,ncallj,ncallk,ncalllm,ncalln,ncallo,ncallpq,ncallr, &
                        ncallst,ncalluv,ncallw,ncallxz,call0c,calld,callef,callgh,calli,callj,callk,calllm,calln,callo,  &
                        callpq,callr,callst,calluv,callw,callxz,ldbvalid ! callsign DB in memory
  character*12 callsign1,callsign2
  logical(1) lfound

  if(.not.ldbvalid) lfound=.true. ! bail out
 
  nlencall1=len_trim(callsign1);  nlencall2=len_trim(callsign2)
  if(nlencall1.gt.7 .and. callsign2.eq."            ") then; lfound=.true.; return; endif
  if(nlencall2.gt.7) callsign2="            " ! we can not put in ALLCALL all special callsigns in advance
  ncycles=2; if(callsign2.eq."            ") ncycles=1
  do j=1,ncycles
    if(j.eq.1) then; callsign=callsign1; else; callsign=callsign2; endif
    if(callsign(1:1).lt."N") then
      if(callsign(1:1).lt."I") then
        if(callsign(1:1).lt."E") then
          if(callsign(1:1).lt."D") then
            do i=1,ncall0c; if(call0c(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncalld; if(calld(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          endif
        else
          if(callsign(1:1).lt."G") then
            do i=1,ncallef; if(callef(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncallgh; if(callgh(i).eq.callsign) then; lfound=.true.; return; endif; enddo
         endif
        endif
      else
        if(callsign(1:1).lt."K") then
          if(callsign(1:1).lt."J") then
            do i=1,ncalli; if(calli(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncallj; if(callj(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          endif
        else
          if(callsign(1:1).lt."L") then
            do i=1,ncallk; if(callk(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncalllm; if(calllm(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          endif
        endif
      endif
    else
      if(callsign(1:1).lt."S") then
        if(callsign(1:1).lt."P") then
          if(callsign(1:1).lt."O") then
            do i=1,ncalln; if(calln(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncallo; if(callo(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          endif
        else
          if(callsign(1:1).lt."R") then
            do i=1,ncallpq; if(callpq(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncallr; if(callr(i).eq.callsign) then; lfound=.true.; return; endif; enddo
         endif
        endif
      else
        if(callsign(1:1).lt."W") then
          if(callsign(1:1).lt."U") then
            do i=1,ncallst; if(callst(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncalluv; if(calluv(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          endif
        else
          if(callsign(1:1).lt."X") then
            do i=1,ncallw; if(callw(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncallxz; if(callxz(i).eq.callsign) then; lfound=.true.; return; endif; enddo
          endif
        endif
      endif
    endif
  enddo
! print *,"blocked ",callsign1,callsign2
  return
end subroutine searchcalls
