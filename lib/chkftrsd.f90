! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine chkftrsd(mrs,mrs2,decoded,ftrsd_cfmd)

  use jt65_mod2, only : s3
  use jt65_mod10
  use packjt

  real u1
  integer mrs(63),mrs2(63),dgen(12),sym_rev(0:62)
  character msg*22,msg00*22,decoded*22
  logical(1) ftrsd_cfmd

  if(first) then
     call cqcall3()
     first=.false.
  endif

  if(nused.lt.5000) return
  nactual=nused+1
  msg=decoded
  call fmtmsg(msg,iz)
  call packmsg(msg,dgen,itype)            !Pack message into 72 bits
  call rs_encode(dgen,sym_rev)            !RS encode
  call interleave63(sym_rev,1)            !Interleave channel symbols
  call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
  sym2(0:62,nactual)=sym_rev(0:62)
  msg0(nactual)=decoded
  
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
  do k=1,nactual
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
  if(decoded.eq.msg0(ipk)) ftrsd_cfmd=.true.

return
end subroutine chkftrsd
