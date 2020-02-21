! This source code file was last time modified by Igor Chernikov UA3DJY on December 12th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine filterscq(decoded,falsedec)
  
  character decoded*22
  logical(1) falsedec

! check for false "CQ " decodes

  if(len_trim(decoded).gt.18) then
     if((decoded(1:3).eq."CQ " .or. decoded(1:3).eq."DE ") .and. decoded(8:8).eq."/" .and. &
     decoded(4:7).ne."3DA0") go to 2
  endif

  if(len_trim(decoded).gt.19) then
     if(decoded(1:4).eq."QRZ " .and. decoded(9:9).eq."/" .and. decoded(5:8).ne."3DA0") go to 2
  endif

  go to 4

2 falsedec=.true.; return

4 return
end subroutine filterscq
