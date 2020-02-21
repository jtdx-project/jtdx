! This source code file was last time modified by Igor UA3DJY on July 22nd, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine hintwidecq(mrs,mrs2,decoded,hint_wide,npass1)

  use jt65_mod10  ! used to share stored messages
  use jt65_mod2, only : s3,correct_hint

  real u1
  integer mrs(63),mrs2(63)
  character msg00*22,decoded*22
  logical(1) hint_wide,npass1

  if(first) then
     call cqcall3()
     first=.false.
  endif
  
  hint_wide=.false.
  if(nused.eq.0) return

  ref0=0.
  do j=1,63
     ref0=ref0 + s3(mrs(j)+1,j)
  enddo

  u1=-99.0
  u2=u1

! Find u1 and u2 (best and second-best) codeword from a list, using 
! a bank of matched filters on the symbol spectra s3(i,j).
  ipk=1
  msg00='                      '
  do k=1,nused
        psum=0.
        ref=ref0
        do j=1,63
           i=sym2(j-1,k)+1
           psum=psum + s3(i,j)
           if(i.eq.mrs(j)+1) ref=ref - s3(i,j) + s3(mrs2(j)+1,j)
        enddo
        p=psum/ref

        if(p.gt.u1) then
           if(msg0(k).ne.msg00) u2=u1
           u1=p
           ipk=k
           msg00=msg0(k)
        endif
        if(msg0(k).ne.msg00 .and. p.gt.u2) u2=p
  enddo

  decoded='                      '
  bias=max(1.12*u2,0.35)
  qual=100.0*(u1-bias)
  thresh=(qual+8.0)*u1

  if(qual.ge.5.6 .or. u1.ge.0.69) then !maximizing sensitivity using 'OR'
!now blocking most of the false decodes 
     if(npass1) then
        if((thresh.gt.7.9 .and. u1.gt.0.63) .or. (thresh.gt.10.5 .and. u1.le.0.63)) then
!print *,qual,u1
!print *,thresh
           decoded=msg0(ipk)
           hint_wide=.true.
           correct_hint(1:63)=sym2(0:62,ipk)
        endif
     endif

     if(.not.npass1) then
        if(u1.gt.0.7 .and. thresh.lt.12.0) return
        if(u1.gt.0.8 .and. thresh.lt.20.0) return
        if(u1.gt.0.9 .and. thresh.lt.30.0) return
        if((thresh.gt.9.6 .and. u1.gt.0.63) .or. (thresh.gt.10.5 .and. u1.le.0.63)) then
!print *,qual,u1
!print *,thresh
           decoded=msg0(ipk)
           hint_wide=.true.
           correct_hint(1:63)=sym2(0:62,ipk)
        endif
     endif 
  endif

return
end subroutine hintwidecq
