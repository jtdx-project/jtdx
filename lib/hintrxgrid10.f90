! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine hintrxgrid10(mrs,mrs2,mycall,decoded,hintdec,s3,i4GrayCodedSym)

  use packjt
  use prog_args
  parameter (MAXCALLS=35000)
  parameter (MAXMSG=MAXCALLS)
  real u1
  real s3(0:7,69)
  integer*1 sym2(0:68,MAXMSG)
  integer mrs(69),mrs2(69),sym_rev(0:68)
  integer*4 i4Msg6BitWords(13)            !72-bit message as 6-bit words
  integer*1 i1Msg8BitBytes(13)            !72 bits and zero tail as 8-bit bytes
  integer*1 i1EncodedBits(207)            !Encoded information-carrying bits
  integer*1 i1ScrambledBits(207)          !Encoded bits after interleaving
  integer*4 i4DataSymbols(69)             !Data symbols (values 0-7)
  integer*4 i4GrayCodedSym(69)            !Gray-coded symbols (values 0-7)
  logical(1) hintdec,first
  character*6 call2(MAXCALLS)
  character*4 grid2(MAXCALLS)
  character line*180,mycall*6,callsign*12,grid*4,msg*22,msg00*22,decoded*22
  character*22 msg0(MAXMSG)
  data first/.true./

  save first,nused,msg0,sym2

  if(first) then
     open(23,file=trim(data_dir)//'/CALL3.TXT',status='unknown')
     j=0
     do i=1,MAXCALLS
        read(23,1002,end=10) line
1002    format(a80)
        if(line(1:4).eq.'ZZZZ') exit
        if(line(1:2).eq.'//') cycle
        i1=index(line,',')
        if(i1.lt.4) cycle
        i2=index(line(i1+1:),',')
        if(i2.lt.5) cycle
        i2=i2+i1
        callsign=line(1:i1-1)
        grid=line(i1+1:i2-1)
        j=j+1
        call2(j)=callsign(1:6)               !### Fix for compound callsigns!
        grid2(j)=grid
     enddo
10   ncalls=j
     if(ncalls.lt.1) then
        print *, 'copy CALL3.TXT file to log directory'
        print *, 'http://www.jtdx.tech' 
!        stop 'CALL3.TXT is too short or missed?'
     endif
     close(23)

     if(ncalls.eq.0) then
        first=.false.
        return
     endif

     j=0; nsym2=206
     do i=1,ncalls
        j=j+1
        msg=mycall//' '//call2(i)//' '//grid2(i)
        call fmtmsg(msg,iz)
        call packmsg(msg,i4Msg6BitWords,itype)  !Pack into 12 6-bit bytes
        call entail(i4Msg6BitWords,i1Msg8BitBytes)  !Add tail, make 8-bit bytes
        call encode232(i1Msg8BitBytes,nsym2,i1EncodedBits)   !Encode K=32, r=1/2
        i1EncodedBits(207)=0
        call interleave9(i1EncodedBits,1,i1ScrambledBits)    !Interleave bits
        i1ScrambledBits(207)=0
        call packbits(i1ScrambledBits,69,3,i4DataSymbols)    !Pk 3-bits into words
        call graycode(i4DataSymbols,69,1,sym_rev) !Apply Gray code
        sym2(0:68,j)=sym_rev(0:68)
        msg0(j)=msg
     enddo
     nused=j
     first=.false.
  endif

  hintdec=.false.

  ref0=0.
  do j=1,69
     ref0=ref0 + s3(mrs(j),j)
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
  thresh=(qual+8.0)*u1

!  if((qual.ge.0.0 .or. u1.ge.0.6) .and. thresh.gt.1.65) then
  if((qual.ge.0.67 .or. u1.ge.0.75) .and. thresh.gt.7.6) then
!print *,qual,u1
!print *,thresh
     decoded=msg0(ipk)
     hintdec=.true.
     i4GrayCodedSym(1:69)=sym2(0:68,ipk)
  endif
return
end subroutine hintrxgrid10
