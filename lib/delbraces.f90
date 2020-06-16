subroutine delbraces(msg37)

  character msg37*37

  ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1; ispc3=index(msg37((ispc2+1):),' ')+ispc2
  ieoc1=ispc1-1; iboc2=ispc1+1; ieoc2=ispc2-1

  if(msg37(1:1).eq.'<' .and. msg37(2:2).ne.'.') then
    msg37(ieoc1:37)=msg37(ieoc1+1:37)//' '; msg37(1:37)=msg37(2:37)//' '
  else if(msg37(iboc2:iboc2).eq.'<' .and. msg37(iboc2+1:iboc2+1).ne.'.') then
    msg37(ieoc2:37)=msg37(ieoc2+1:37)//' '; msg37(iboc2:37)=msg37(iboc2+1:37)//' '
  else
    iboc3=ispc2+1; ieoc3=ispc3-1
    if(msg37(iboc3:iboc3).eq.'<' .and. msg37(iboc3+1:iboc3+1).ne.'.') then
      msg37(ieoc3:37)=msg37(ieoc3+1:37)//' '; msg37(iboc3:37)=msg37(iboc3+1:37)//' '
    endif
  endif

  return
end subroutine delbraces