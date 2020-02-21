! This source code file was last time modified by Igor UA3DJY on 20181231
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine rms_augap(nutc,lowrms)

  use jt65_mod6, only : dd

  logical lowrms,baddata

  rms1=sqrt(dot_product(dd(100000:100099),dd(100000:100099))/100.0)
  rms2=sqrt(dot_product(dd(200000:200099),dd(200000:200099))/100.0)
  rms3=sqrt(dot_product(dd(300000:300099),dd(300000:300099))/100.0)
  rms4=sqrt(dot_product(dd(400000:400099),dd(400000:400099))/100.0)
  rms5=sqrt(dot_product(dd(500000:500099),dd(500000:500099))/100.0)
  rms=(rms1+rms2+rms3+rms4+rms5)/5.0
  if(rms.lt.2.0) then
     write(*,1016) nutc,'input signal low rms','d'
1016 format(i4.4,2x,a20,19x,a1)
     call flush(6)
     lowrms=.true.
     return
  endif

  if(baddata()) then
     write(*,1017) nutc,'audio gap detected','d'
1017 format(i4.4,2x,a18,21x,a1)
     call flush(6)
	 
     j=0
     do i=1,490
        sq=0.
        do n=1,1200,60
           j=j+60
           x=dd(j)
           sq=sq + abs(x)
        enddo
        if(sq.lt.1.0) then
           dd(j)=1.0; dd(j-600)=1.0
        endif
     enddo
  endif

  return
end subroutine rms_augap