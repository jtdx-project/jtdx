! This source code file was last time modified by Igor Chernikov UA3DJY on July 11th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine flat65(ss,ref)

  integer, parameter :: NSZ=3413,NHSYM=276
  real stmp(NSZ)
  real ss(NHSYM,NSZ)
  real ref(NSZ)

  npct=28                                       !Somewhat arbitrary
  do i=1,NSZ
     call pctile(ss(1,i),nhsym,npct,stmp(i))
  enddo

  nsmo=33
  ia=nsmo/2 + 1
  ib=NSZ - nsmo/2 - 1
  do i=ia,ib
     call pctile(stmp(i-nsmo/2),nsmo,npct,ref(i))
  enddo
  ref(:ia-1)=ref(ia)
  ref(ib+1:)=ref(ib)
  ref=4.0*ref

  return
end subroutine flat65

      
