! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine hintdx73(mrs,mrs2,mycall,hiscall,decoded,hint_dx)

  use packjt
  use jt65_mod2, only : s3,correct_hint
  
  parameter (MAXMSG=10)
  real u1
  integer mrs(63),mrs2(63),sym2(0:62,MAXMSG),dgen(12),sym_rev(0:62)
  logical(1) hint_dx,first
  character mycall*6,hiscall*6,msg*22,msg1*22,msg00*22,decoded*22,prevcall*6
  character*22 msg0(MAXMSG)
  data first/.true./
  save first,msg0,sym2,prevcall

  hint_dx=.false.
  if(hiscall.eq.'') return
  do i=1,6
   if(hiscall(i:i).eq.'/') return
  enddo
  
  if(first) then
     do m=4,MAXMSG
        msg='UF0XYZ W9XYZ '//'EN3'//achar(47+m)
        call fmtmsg(msg,iz)
        call packmsg(msg,dgen,itype)            !Pack message into 72 bits
        call rs_encode(dgen,sym_rev)            !RS encode
        call interleave63(sym_rev,1)            !Interleave channel symbols
        call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
        sym2(0:62,m)=sym_rev(0:62)
        msg0(m)=msg
     enddo
     first=.false.
  endif

  if(hiscall.eq.prevcall) then
     go to 2
  else
     prevcall=hiscall
  endif

  do m=1,3
     if(m.eq.1) msg=mycall//' '//hiscall//' RRR'
     if(m.eq.2) msg=mycall//' '//hiscall//' RR73'
     if(m.eq.3) msg=mycall//' '//hiscall//' 73'
     call fmtmsg(msg,iz)
     call packmsg(msg,dgen,itype)            !Pack message into 72 bits
     call rs_encode(dgen,sym_rev)            !RS encode
     call interleave63(sym_rev,1)            !Interleave channel symbols
     call graycode(sym_rev,63,1,sym_rev)     !Apply Gray code
     sym2(0:62,m)=sym_rev(0:62)
     msg0(m)=msg
  enddo

2  ref0=0.
  do i=1,63
     do j=1,63
        ref0=ref0 + s3(mrs(j)+1,i)
     enddo
  enddo

  u1=-99.0
  u2=u1

! Find u1 and u2 (best and second-best) codeword from a list, using 
! a bank of matched filters on the symbol spectra s3(i,j).
  ipk=1
  msg00='                      '
  do k=1,MAXMSG
      psum=0.
      ref=ref0
      do j=1,63
         i=sym2(j-1,k)+1
         psum=psum + s3(i,j)
         if(i.eq.mrs(j)+1) ref=ref - s3(i,j) + s3(mrs2(j)+1,j)
      enddo
      p=psum*63.0/ref

      if(p.gt.u1) then
         if(msg0(k).ne.msg00) u2=u1
         u1=p
         ipk=k
         msg00=msg0(k)
      endif
      if(msg0(k).ne.msg00 .and. p.gt.u2) u2=p
  enddo

  decoded='                      '
  msg1=msg0(ipk)
  if(msg1(1:6).eq."UF0XYZ") go to 4
  
  bias=max(1.12*u2,0.35)
  qual=100.0*(u1-bias)
  if(qual.ge.8.0 .or. u1.ge.1.33) then
     decoded=msg1
!print *,qual,u1
!print *,decoded
     hint_dx=.true.
     correct_hint(1:63)=sym2(0:62,ipk)
  endif 
4  return
end subroutine hintdx73
