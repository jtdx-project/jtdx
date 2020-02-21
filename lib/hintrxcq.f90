! This source code file was last time modified by Igor UA3DJY on July 22nd, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine hintrxcq(mrs,mrs2,decoded,hint_call3)

  use jt65_mod10 ! used to share stored messages
  use jt65_mod2, only : s3,correct_hint

  real u1
  integer mrs(63),mrs2(63)
  logical(1) hint_call3
  character msg00*22,decoded*22

  if(first) then
     call cqcall3()
     first=.false.
  endif

  hint_call3=.false.
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

  if((qual.ge.0.0 .or. u1.ge.0.6) .and. thresh.gt.1.65) then
!print *,qual,u1
!print *,thresh
        decoded=msg0(ipk)
        hint_call3=.true.
        correct_hint(1:63)=sym2(0:62,ipk)
  endif
return
end subroutine hintrxcq
