! This source code file was last time modified by Igor UA3DJY on February 6th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine splitmsgdupe(decodedhist,callncq)

  character decodedhist*22,callncq*6

  if(decodedhist(1:3).eq.'CQ ' .or. &
     decodedhist(1:3).eq.'QRZ' .or. decodedhist(1:3).eq.'DE ') return

  i1=index(decodedhist,' ')
  if(i1.lt.4) return
  i2=index(decodedhist(i1+1:),' ')
  if(i2.lt.4) return
  i2=i2+i1
  if(len_trim(decodedhist(i1+1:i2-1)).gt.6) return
  callncq=decodedhist(i1+1:i2-1)


return
end subroutine splitmsgdupe
