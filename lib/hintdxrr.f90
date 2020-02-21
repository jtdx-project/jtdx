! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine hintdxrr(mrs,mrs2,mycall,hiscall,decoded,hint_dx)

  use packjt
  use jt65_mod2, only : s3,correct_hint

  parameter (MAXRPT=30)
  parameter (MAXMSG=MAXRPT)
  real u1
  integer mrs(63),mrs2(63),sym2(0:62,MAXMSG),dgen(12),sym_rev(0:62)
  logical(1) hint_dx
  character mycall*6,hiscall*6,msg*22,msg00*22,decoded*22,prevcall*6
  character*4 rpt(MAXRPT)
  character*22 msg0(MAXMSG)

  data rpt/'R-01','R-02','R-03','R-04','R-05',     &
           'R-06','R-07','R-08','R-09','R-10',     &
           'R-11','R-12','R-13','R-14','R-15',     &
           'R-16','R-17','R-18','R-19','R-20',     &
           'R-21','R-22','R-23','R-24','R-25',     &
           'R-26','R-27','R-28','R-29','R-30'/

  save msg0,sym2,prevcall

  hint_dx=.false.
  do i=1,6
   if(hiscall(i:i).eq.'/') return
  enddo

  if(hiscall.eq.prevcall) then
     go to 2
  else
     prevcall=hiscall
  endif
  
        do m=1,MAXMSG
           msg=mycall//' '//hiscall//' '//rpt(m)
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
  bias=max(1.12*u2,0.35)
  qual=100.0*(u1-bias)
  thresh=(qual+20.0)*(u1-1.0)
! maximizing sensitivity .and. blocking some false decodes
  if((qual.ge.-2.0 .or. u1.ge.1.4) .and. thresh.gt.6.5) then


    decoded=msg0(ipk)
!print *,qual,u1,thresh
!print *,qual,thresh
!print *,decoded
    hint_dx=.true.
    correct_hint(1:63)=sym2(0:62,ipk)
  endif 
  return
end subroutine hintdxrr
