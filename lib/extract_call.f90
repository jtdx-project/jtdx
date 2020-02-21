! This source code file was last time modified by Igor UA3DJY on 20190315

subroutine extract_call(msg37,call2)

  character, intent(in) :: msg37*37
  character, intent(out) :: call2*12
  character part2*12,part3*12

  call2=''; part2=''; part3=''
  ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1; ispc3=index(msg37((ispc2+1):),' ')+ispc2
  part2=msg37(ispc1+1:ispc2-1); part3=msg37(ispc2+1:ispc3-1); npart2len=len_trim(part2)

  if(msg37(1:3).eq.'CQ ' .or. msg37(1:3).eq.'DE ' .or. msg37(1:4).eq.'QRZ ') then
    if(npart2len.gt.4) then; call2=part2
    elseif(npart2len.eq.4) then
      if(part2(2:2).ge.'0' .and. part2(2:2).lt.':' .or. part2(3:3).ge.'0' .and. part2(3:3).lt.':') then; call2=part2
      else; call2=part3
      endif
    elseif(npart2len.eq.3) then
      if(part2(1:1).ge.'A' .and. part2(1:1).lt.'[' .and. part2(2:2).ge.'0' .and. part2(2:2).lt.':') then; call2=part2
      else; call2=part3
      endif
    elseif(npart2len.eq.2) then; call2=part3
    endif
  elseif(ispc1.gt.3 .and. ispc1.lt.13) then; call2=part2
  endif

  return
end subroutine extract_call