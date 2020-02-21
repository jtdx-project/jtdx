! last time modified by Igor UA3DJY on 20191118

subroutine chkspecial8(msg37,msg37_2,nbadcrc)

  use ft8_mod1, only : mycall,hiscall
  character msg37*37,msg37_2*37,call_a*12,call_b*12,call_c*12
  logical(1) falsedec

  call_a='            '; call_b='            '; call_c='            '

! V03VTJ RR73; 2D3VDY <...> +08     call_a call_b call_c     
!V03VTJ <...> RR73         
!2D3VDY <...> +08          
! Y84NN RR73; HO5RFG <...> -16           
!Y84NN <...> RR73          
!HO5RFG <...> -16          
! P7IJQ RR73; MP4LEL <...> +02           
!P7IJQ <...> RR73          
!MP4LEL <...> +02 
 
  ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1; ispc12=index(msg37_2,' ')
  if(ispc1.gt.3 .and. ispc2.gt.7) then
    call_a=msg37(1:ispc1-1); call_b=msg37_2(1:ispc12-1); call_c=msg37(ispc1+1:ispc2-1)
    if(call_a.ne.mycall .and. call_b.ne.mycall .and. call_c.ne.hiscall) then
! call may not start from 'Q', zero and two digits
      if(call_a(1:1).eq.'Q' .or. call_b(1:1).eq.'Q') then
        nbadcrc=1; msg37='                                     '; msg37_2=msg37; return; endif
      if(call_a(1:1).eq.'0' .or. call_b(1:1).eq.'0') then
        nbadcrc=1; msg37='                                     '; msg37_2=msg37; return; endif
      if(call_a(1:1).ge.'0' .and. call_a(1:1).le.'9' .and. call_a(2:2).ge.'0' .and. call_a(2:2).le.'9') then
        nbadcrc=1; msg37='                                     '; msg37_2=msg37; return; endif
      if(call_b(1:1).ge.'0' .and. call_b(1:1).le.'9' .and. call_b(2:2).ge.'0' .and. call_b(2:2).le.'9') then
        nbadcrc=1;  msg37='                                     '; msg37_2=msg37; return; endif

      falsedec=.false.
      call chkflscall(call_a,call_b,falsedec)
      if(falsedec) then; nbadcrc=1; msg37='                                     '; msg37_2=msg37; return; endif
    endif
  endif

  return
end subroutine chkspecial8
