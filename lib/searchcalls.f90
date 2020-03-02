! last time modified by Igor UA3DJY on 20200302

subroutine searchcalls(callsign01,callsign02,lfound)
  
  use jt65_mod9, only : ncall0c,ncalld,ncallef,ncallgh,ncalli,ncallj,ncallk,ncalllm,ncalln,ncallo,ncallpq,ncallr, &
                        ncallst,ncalluv,ncallw,ncallxz,call0c,calld,callef,callgh,calli,callj,callk,calllm,calln,callo,  &
                        callpq,callr,callst,calluv,callw,callxz,ldbvalid ! callsign DB in memory
  character*12 callsign0,callsign01,callsign02
  logical(1) lfound

  if(.not.ldbvalid) then; lfound=.true.; return; endif ! bail out
 
  nlencall1=len_trim(callsign01);  nlencall2=len_trim(callsign02)
  if(nlencall1.gt.7 .and. callsign02.eq."            ") then; lfound=.true.; return; endif
  if(nlencall2.gt.7) callsign02="            " ! we can not put in ALLCALL all special callsign0s in advance
  ncycles=2; if(callsign02.eq."            ") ncycles=1
  do j=1,ncycles
    if(j.eq.1) then; callsign0=callsign01; else; callsign0=callsign02; endif
    if(callsign0(1:1).lt."N") then
      if(callsign0(1:1).lt."I") then
        if(callsign0(1:1).lt."E") then
          if(callsign0(1:1).lt."D") then
            do i=1,ncall0c; if(call0c(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncalld; if(calld(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          endif
        else
          if(callsign0(1:1).lt."G") then
            do i=1,ncallef; if(callef(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncallgh; if(callgh(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
         endif
        endif
      else
        if(callsign0(1:1).lt."K") then
          if(callsign0(1:1).lt."J") then
            do i=1,ncalli; if(calli(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncallj; if(callj(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          endif
        else
          if(callsign0(1:1).lt."L") then
            do i=1,ncallk; if(callk(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncalllm; if(calllm(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          endif
        endif
      endif
    else
      if(callsign0(1:1).lt."S") then
        if(callsign0(1:1).lt."P") then
          if(callsign0(1:1).lt."O") then
            do i=1,ncalln; if(calln(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncallo; if(callo(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          endif
        else
          if(callsign0(1:1).lt."R") then
            do i=1,ncallpq; if(callpq(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncallr; if(callr(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
         endif
        endif
      else
        if(callsign0(1:1).lt."W") then
          if(callsign0(1:1).lt."U") then
            do i=1,ncallst; if(callst(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncalluv; if(calluv(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          endif
        else
          if(callsign0(1:1).lt."X") then
            do i=1,ncallw; if(callw(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          else
            do i=1,ncallxz; if(callxz(i).eq.callsign0) then; lfound=.true.; return; endif; enddo
          endif
        endif
      endif
    endif
  enddo
! print *,"blocked ",callsign01,callsign02
  return
end subroutine searchcalls
