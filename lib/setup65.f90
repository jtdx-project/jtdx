! This source code file was last time modified by Igor UA3DJY on February 28th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine setup65

! Defines arrays related to the JT65 pseudo-random synchronizing pattern.
! Executed at program start.

 use jt65_mod4
 integer nsym,nsig
 data mr2/0/                !Silence compiler warning

  nsym=126

! Determine locations of data and reference symbols
  k=0
  mr1=0
  do i=1,nsym
     if(.not.prc(i)) then
        k=k+1
        mdat(k)=i
     else
        mr2=i
        if(mr1.eq.0) mr1=i
     endif
  enddo
  nsig=k

! Determine the reference symbols for each data symbol.
  do k=1,nsig
     m=mdat(k)
     mref(k,1)=mr1
     do n=1,10                     !Get ref symbol before data
        if((m-n).gt.0) then
           if (prc(m-n)) go to 10
        endif
     enddo
     go to 12
10   mref(k,1)=m-n
12   mref(k,2)=mr2
     do n=1,10                     !Get ref symbol after data
        if((m+n).le.nsym) then
           if (prc(m+n)) go to 20
        endif
     enddo
     cycle
20   mref(k,2)=m+n
  enddo

  return
end subroutine setup65
