! This source code file was last time modified by Igor UA3DJY on February 6th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine splitdupecq(decoded,callcq)
  
  
  character decoded*22,callcq*6

  i1=index(decoded,' ')
  i2=index(decoded(i1+1:),' ')

  if(i2.eq.3) then ! directional CQ message
     i2=i2+i1
     i3=index(decoded(i2+1:),' ')
     i3=i3+i2
if(len_trim(decoded(i2+1:i3-1)).gt.6) go to 2
     callcq=decoded(i2+1:i3-1)
     go to 2
  endif

  if(i2.gt.3) then ! non-directional CQ message
     i2=i2+i1
     callcq=decoded(i1+1:i2-1)
  endif

2 return
end subroutine splitdupecq
