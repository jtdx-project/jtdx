! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine hintdxrr10(mrs,mrs2,mycall,hiscall,decoded,hintdec,s3,i4GrayCodedSym)

  use packjt
  parameter (MAXRPT=33)
  parameter (MAXMSG=MAXRPT)
  real u1
  real s3(0:7,69)
  integer mrs(69),mrs2(69),sym2(0:68,MAXMSG),sym_rev(0:68)
  integer*4 i4Msg6BitWords(13)            !72-bit message as 6-bit words
  integer*1 i1Msg8BitBytes(13)            !72 bits and zero tail as 8-bit bytes
  integer*1 i1EncodedBits(207)            !Encoded information-carrying bits
  integer*1 i1ScrambledBits(207)          !Encoded bits after interleaving
  integer*4 i4DataSymbols(69)             !Data symbols (values 0-7)
  integer*4 i4GrayCodedSym(69)        !Gray-coded symbols (values 0-7)
  logical(1) hintdec
  character mycall*6,hiscall*6,msg*22,msg00*22,decoded*22,prevcall*6
  character*4 rpt(MAXRPT)
  character*22 msg0(MAXMSG)

  data rpt/'R-01','R-02','R-03','R-04','R-05',     &
           'R-06','R-07','R-08','R-09','R-10',     &
           'R-11','R-12','R-13','R-14','R-15',     &
           'R-16','R-17','R-18','R-19','R-20',     &
           'R-21','R-22','R-23','R-24','R-25',     &
           'R-26','R-27','R-28','R-29','R-30',     &
           'R-31','R-32','R-33'/

  save msg0,sym2,prevcall

  hintdec=.false.
  if(hiscall.eq.'') return
  do i=1,6
   if(hiscall(i:i).eq.'/') return
  enddo

  if(hiscall.eq.prevcall) then
     go to 2
  else
     prevcall=hiscall
  endif

  nsym2=206

  do m=1,MAXMSG
     msg=mycall//' '//hiscall//' '//rpt(m)
     call fmtmsg(msg,iz)
     call packmsg(msg,i4Msg6BitWords,itype)  !Pack into 12 6-bit bytes
     call entail(i4Msg6BitWords,i1Msg8BitBytes)  !Add tail, make 8-bit bytes
     call encode232(i1Msg8BitBytes,nsym2,i1EncodedBits)   !Encode K=32, r=1/2
     i1EncodedBits(207)=0
     call interleave9(i1EncodedBits,1,i1ScrambledBits)    !Interleave bits
     i1ScrambledBits(207)=0
     call packbits(i1ScrambledBits,69,3,i4DataSymbols)    !Pk 3-bits into words
     call graycode(i4DataSymbols,69,1,sym_rev) !Apply Gray code
     sym2(0:68,m)=sym_rev(0:68)
     msg0(m)=msg
  enddo

2  ref0=0.
   do j=1,69
      ref0=ref0 + s3(mrs(j),j)
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
        do j=1,69
           i=sym2(j-1,k)
           psum=psum + s3(i,j)
           if(i.eq.mrs(j)) ref=ref - s3(i,j) + s3(mrs2(j),j)
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
  thresh=qual*u1
! maximizing sensitivity .and. blocking some false decodes
  if((qual.ge.5.0 .or. u1.ge.0.736) .and. thresh.gt.3.5) then
    decoded=msg0(ipk)
!print *,qual,u1,thresh
!print *,qual,thresh
!print *,decoded
    hintdec=.true.
    i4GrayCodedSym(1:69)=sym2(0:68,ipk)
  endif 
  return
end subroutine hintdxrr10
