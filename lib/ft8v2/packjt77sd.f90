! This source code file was last time modified by Igor UA3DJY on 20190309
! All changes are shown in the patch file coming together with the full JTDX source code.

module packjt77sd

! These variables are accessible from outside via "use packjt77":
!  integer n28a,n28b

  contains

subroutine pack77sd(msg0,i3,n3,c77)

  use packjt
  character*37 msg,msg0
  character*13 w(19)
  character*77 c77
  integer nw(19)

  msg=msg0

! Convert msg to upper case; collapse multiple blanks; parse into words.
  call split77(msg,nwords,nw,w)
  i3=-1
  n3=-1
  if(msg(1:3).eq.'CQ ' .or. msg(1:3).eq.'DE ' .or. msg(1:4).eq.'QRZ ') go to 100

! Check Type 1 (Standard 77-bit message) or Type 2, with optional "/P"
100 call pack77_1(nwords,w,i3,n3,c77)
  if(i3.ge.0) go to 900

! It defaults to free text
  i3=0
  n3=0
  msg(14:)='                        '
  call packtext77(msg(1:13),c77(1:71))
  write(c77(72:77),'(2b3.3)') n3,i3

900 return
end subroutine pack77sd

subroutine unpack77sd(c77,msg,unpk77_success)

  parameter (MAXGRID4=32400)
  integer*8 n58
  character*77 c77*77,msg*37,c*38,call_1*13,call_2*13,c11*11,grid4*4,crpt*3
  logical unpk28_success,unpk77_success

  data c/' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ/'/

  unpk77_success=.true.

! Check for bad data
  do i=1,77
     if(c77(i:i).ne.'0' .and. c77(i:i).ne.'1') then
        msg='failed unpack'
        unpk77_success=.false.
        return
     endif
  enddo

  read(c77(72:77),'(2b3)') n3,i3
  msg=repeat(' ',37)
  if(i3.eq.0 .and. n3.eq.0) then
! 0.0  Free text
     call unpacktext77(c77(1:71),msg(1:13))
     msg(14:)='                        '
     msg=adjustl(msg)

  else if(i3.eq.1 .or. i3.eq.2) then
! Type 1 (standard message) or Type 2 ("/P" form for EU VHF contest)
     read(c77,1000) n28a,ipa,n28b,ipb,ir,igrid4,i3
1000 format(2(b28,b1),b1,b15,b3)
     call unpack28(n28a,call_1,unpk28_success)
     if(.not.unpk28_success) unpk77_success=.false.
     call unpack28(n28b,call_2,unpk28_success)
     if(.not.unpk28_success) unpk77_success=.false.
     if(call_1(1:3).eq.'CQ_') call_1(3:3)=' '
     if(index(call_1,'<').le.0) then
        i=index(call_1,' ')
        if(i.ge.4 .and. ipa.eq.1 .and. i3.eq.1) call_1(i:i+1)='/R'
        if(i.ge.4 .and. ipa.eq.1 .and. i3.eq.2) call_1(i:i+1)='/P'
     endif
     if(index(call_2,'<').le.0) then
        i=index(call_2,' ')
        if(i.ge.4 .and. ipb.eq.1 .and. i3.eq.1) call_2(i:i+1)='/R'
        if(i.ge.4 .and. ipb.eq.1 .and. i3.eq.2) call_2(i:i+1)='/P'
     endif
     if(igrid4.le.MAXGRID4) then
        n=igrid4
        j1=n/(18*10*10)
        n=n-j1*18*10*10
        j2=n/(10*10)
        n=n-j2*10*10
        j3=n/10
        j4=n-j3*10
        grid4(1:1)=char(j1+ichar('A'))
        grid4(2:2)=char(j2+ichar('A'))
        grid4(3:3)=char(j3+ichar('0'))
        grid4(4:4)=char(j4+ichar('0'))
        if(ir.eq.0) msg=trim(call_1)//' '//trim(call_2)//' '//grid4
        if(ir.eq.1) msg=trim(call_1)//' '//trim(call_2)//' R '//grid4
        if(msg(1:3).eq.'CQ ' .and. ir.eq.1) unpk77_success=.false.
     else
        irpt=igrid4-MAXGRID4
        if(irpt.eq.1) msg=trim(call_1)//' '//trim(call_2)
        if(irpt.eq.2) msg=trim(call_1)//' '//trim(call_2)//' RRR'
        if(irpt.eq.3) msg=trim(call_1)//' '//trim(call_2)//' RR73'
        if(irpt.eq.4) msg=trim(call_1)//' '//trim(call_2)//' 73'
        if(irpt.ge.5) then
           write(crpt,'(i3.2)') irpt-35
           if(crpt(1:1).eq.' ') crpt(1:1)='+'
           if(ir.eq.0) msg=trim(call_1)//' '//trim(call_2)//' '//crpt
           if(ir.eq.1) msg=trim(call_1)//' '//trim(call_2)//' R'//crpt
        endif
        if(msg(1:3).eq.'CQ ' .and. irpt.ge.2) unpk77_success=.false. 
     endif

  else if(i3.eq.4) then
     read(c77,1050) n12,n58,iflip,nrpt,icq
1050 format(b12,b58,b1,b2,b1)
     if(icq.eq.1) then
       do i=11,1,-1
         j=mod(n58,38)+1
         c11(i:i)=c(j:j)
         n58=n58/38
       enddo
       call_2=adjustl(c11)//'  '
       msg='CQ '//trim(call_2)
       if(msg(1:4).eq.'CQ <') unpk77_success=.false.
     else
       unpk77_success=.false.
     endif
  endif

  return
end subroutine unpack77sd

subroutine pack28(c13,n28)

! Pack a special token, a 22-bit hash code, or a valid base call into a 28-bit
! integer.

  parameter (NTOKENS=2063592,MAX22=4194304)
  logical is_digit,is_letter
  character*13 c13
  character*6 callsign
  character*1 c
  character*4 c4
  character*37 a1
  character*36 a2
  character*10 a3
  character*27 a4
  data a1/' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
  data a2/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
  data a3/'0123456789'/
  data a4/' ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
  
  is_digit(c)=c.ge.'0' .and. c.le.'9'
  is_letter(c)=c.ge.'A' .and. c.le.'Z'

  n28=-1
! Work-around for Swaziland prefix:
  if(c13(1:4).eq.'3DA0') callsign='3D0'//c13(5:7)
! Work-around for Guinea prefixes:
  if(c13(1:2).eq.'3X' .and. c13(3:3).ge.'A' .and.          &
       c13(3:3).le.'Z') callsign='Q'//c13(3:6)

! Check for special tokens first
  if(c13(1:3).eq.'DE ') then
     n28=0
     go to 900
  endif
  
  if(c13(1:4).eq.'QRZ ') then
     n28=1
     go to 900
  endif

  if(c13(1:3).eq.'CQ ') then
     n28=2
     go to 900
  endif

  if(c13(1:3).eq.'CQ_') then
     n=len(trim(c13))
     if(n.ge.4 .and. n.le.7) then
        nlet=0
        nnum=0
        do i=4,n
           c=c13(i:i)
           if(c.ge.'A' .and. c.le.'Z') nlet=nlet+1
           if(c.ge.'0' .and. c.le.'9') nnum=nnum+1
        enddo
        if(nnum.eq.3 .and. nlet.eq.0) then
           read(c13(4:3+nnum),*) nqsy
           n28=3+nqsy
           go to 900
        endif
        if(nlet.ge.1 .and. nlet.le.4 .and. nnum.eq.0) then
           c4=c13(4:n)
           c4=adjustr(c4)
           m=0
           do i=1,4
              j=0
              c=c4(i:i)
              if(c.ge.'A' .and. c.le.'Z') j=ichar(c)-ichar('A')+1
              m=27*m + j
           enddo
           n28=3+1000+m
           go to 900
        endif
     endif
  endif

! Check for standard callsign
  iarea=-1
  n=len(trim(c13))
  do i=n,2,-1
     if(is_digit(c13(i:i))) exit
  enddo
  iarea=i                                   !Call-area digit
  npdig=0                                   !Digits before call area
  nplet=0                                   !Letters before call area
  do i=1,iarea-1
     if(is_digit(c13(i:i))) npdig=npdig+1
     if(is_letter(c13(i:i))) nplet=nplet+1
  enddo
  nslet=0
  do i=iarea+1,n
     if(is_letter(c13(i:i))) nslet=nslet+1
  enddo
  
  n=len(trim(c13))
! This is a standard callsign
  if(iarea.eq.2) callsign=' '//c13(1:5)
  if(iarea.eq.3) callsign=c13(1:6)
  i1=index(a1,callsign(1:1))-1
  i2=index(a2,callsign(2:2))-1
  i3=index(a3,callsign(3:3))-1
  i4=index(a4,callsign(4:4))-1
  i5=index(a4,callsign(5:5))-1
  i6=index(a4,callsign(6:6))-1
  n28=36*10*27*27*27*i1 + 10*27*27*27*i2 + 27*27*27*i3 + 27*27*i4 + &
       27*i5 + i6
  n28=n28 + NTOKENS + MAX22

900 n28=iand(n28,ishft(1,28)-1)
  return
end subroutine pack28


subroutine unpack28(n28_0,c13,success)

  parameter (NTOKENS=2063592,MAX22=4194304)
  logical success
  character*13 c13
  character*37 c1
  character*36 c2
  character*10 c3
  character*27 c4
  data c1/' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
  data c2/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
  data c3/'0123456789'/
  data c4/' ABCDEFGHIJKLMNOPQRSTUVWXYZ'/

  success=.true.
  n28=n28_0
  if(n28.lt.NTOKENS) then
! Special tokens DE, QRZ, CQ, CQ_nnn, CQ_aaaa
     if(n28.eq.0) c13='DE           '
     if(n28.eq.1) c13='QRZ          '
     if(n28.eq.2) c13='CQ           '
     if(n28.le.2) go to 900
     if(n28.le.1002) then
        write(c13,1002) n28-3
1002    format('CQ_',i3.3)
        go to 900
     endif
     if(n28.le.532443) then
        n=n28-1003
        n0=n
        i1=n/(27*27*27)
        n=n-27*27*27*i1
        i2=n/(27*27)
        n=n-27*27*i2
        i3=n/27
        i4=n-27*i3
        c13=c4(i1+1:i1+1)//c4(i2+1:i2+1)//c4(i3+1:i3+1)//c4(i4+1:i4+1)
        c13=adjustl(c13)
        c13='CQ_'//c13(1:10)
        go to 900
     endif
  endif
  n28=n28-NTOKENS
  
! Standard callsign
  n=n28 - MAX22
  i1=n/(36*10*27*27*27)
  n=n-36*10*27*27*27*i1
  i2=n/(10*27*27*27)
  n=n-10*27*27*27*i2
  i3=n/(27*27*27)
  n=n-27*27*27*i3
  i4=n/(27*27)
  n=n-27*27*i4
  i5=n/27
  i6=n-27*i5
  c13=c1(i1+1:i1+1)//c2(i2+1:i2+1)//c3(i3+1:i3+1)//c4(i4+1:i4+1)//     &
       c4(i5+1:i5+1)//c4(i6+1:i6+1)
  c13=adjustl(c13)

900 i0=index(c13,' ')
  if(i0.ne.0 .and. i0.lt.len(trim(c13))) then
     c13='QU1RK'
     success=.false.
  endif
  return
end subroutine unpack28

subroutine split77(msg,nwords,nw,w)

! Convert msg to upper case; collapse multiple blanks; parse into words.

  character*37 msg
  character*13 w(19)
  character*1 c,c0
  character*6 bcall_1
  logical ok1
  integer nw(19)
    
  iz=len(trim(msg))
  j=0
  k=0
  n=0
  c0=' '
  w='             '
  do i=1,iz
     if(ichar(msg(i:i)).eq.0) msg(i:i)=' '
     c=msg(i:i)                                 !Single character
     if(c.eq.' ' .and. c0.eq.' ') cycle         !Skip leading/repeated blanks
     if(c.ne.' ' .and. c0.eq.' ') then
        k=k+1                                   !New word
        n=0
     endif
     j=j+1                                      !Index in msg
     n=n+1                                      !Index in word
     if(c.ge.'a' .and. c.le.'z') c=char(ichar(c)-32)  !Force upper case
     msg(j:j)=c
     if(n.le.13) w(k)(n:n)=c                    !Copy character c into word
     c0=c
  enddo
  iz=j                                          !Message length
  nwords=k                                      !Number of words in msg
  if(nwords.le.0) go to 900
  nw(k)=len(trim(w(k)))
  msg(iz+1:)='                                     '
  if(nwords.lt.3) go to 900
  call chkcall(w(3),bcall_1,ok1)
  if(ok1 .and. w(1)(1:3).eq.'CQ ') then
     w(1)='CQ_'//w(2)(1:10)             !Make "CQ " into "CQ_"
     w(2:12)=w(3:13)                    !Move all remeining words down by one
     nwords=nwords-1
  endif
  
900 return
end subroutine split77

subroutine pack77_1(nwords,w,i3,n3,c77)
! Check Type 1 (Standard 77-bit message) and Type 2 (ditto, with a "/P" call)

  parameter (MAXGRID4=32400)
  character*13 w(19),c13
  character*77 c77
  character*6 bcall_1,bcall_2
  character*4 grid4
  character c1*1,c2*2
  logical is_grid4
  logical ok1,ok2
  is_grid4(grid4)=len(trim(grid4)).eq.4 .and.                        &
       grid4(1:1).ge.'A' .and. grid4(1:1).le.'R' .and.               &
       grid4(2:2).ge.'A' .and. grid4(2:2).le.'R' .and.               &
       grid4(3:3).ge.'0' .and. grid4(3:3).le.'9' .and.               &
       grid4(4:4).ge.'0' .and. grid4(4:4).le.'9'

  if(nwords.lt.2 .or. nwords.gt.4) return
  call chkcall(w(1),bcall_1,ok1)
  call chkcall(w(2),bcall_2,ok2)
  if(w(1)(1:3).eq.'DE ' .or. w(1)(1:3).eq.'CQ_' .or.  w(1)(1:3).eq.'CQ ' .or. &
       w(1)(1:4).eq.'QRZ ') ok1=.true.
  if(w(1)(1:1).eq.'<' .and. index(w(1),'>').ge.5) ok1=.true.
  if(w(2)(1:1).eq.'<' .and. index(w(2),'>').ge.5) ok2=.true.
  if(.not.ok1 .or. .not.ok2) return
  if(w(1)(1:1).eq.'<' .and. index(w(2),'/').gt.0) return
  if(w(2)(1:1).eq.'<' .and. index(w(1),'/').gt.0) return
  if(nwords.eq.2 .and. (.not.ok2 .or. index(w(2),'/').ge.2)) return
  if(nwords.eq.2) go to 10

  c1=w(nwords)(1:1)
  c2=w(nwords)(1:2)
  if(.not.is_grid4(w(nwords)(1:4)) .and. c1.ne.'+' .and. c1.ne.'-'              &
       .and. c2.ne.'R+' .and. c2.ne.'R-' .and. trim(w(nwords)).ne.'RRR' .and.   &
       trim(w(nwords)).ne.'RR73' .and. trim(w(nwords)).ne.'73') return
  if(c1.eq.'+' .or. c1.eq.'-') then
     ir=0
     read(w(nwords),*,err=900) irpt
     irpt=irpt+35
  else if(c2.eq.'R+' .or. c2.eq.'R-') then
     ir=1
     read(w(nwords)(2:),*) irpt
     irpt=irpt+35
  else if(trim(w(nwords)).eq.'RRR') then
     ir=0
     irpt=2
  else if(trim(w(nwords)).eq.'RR73') then
     ir=0
     irpt=3
  else if(trim(w(nwords)).eq.'73') then
     ir=0
     irpt=4
  endif

! 1     WA9XYZ/R KA1ABC/R R FN42           28 1 28 1 1 15   74   Standard msg
! 2     PA3XYZ/P GM4ABC/P R JO22           28 1 28 1 1 15   74   EU VHF contest  

10 i1psuffix=index(w(1)//' ' ,'/P ')
  i2psuffix=index(w(2)//' ','/P ')
  if(nwords.eq.2 .or. nwords.eq.3 .or. (nwords.eq.4 .and.               &
       w(3)(1:2).eq.'R ')) then
     n3=0
     i3=1                          !Type 1: Standard message, possibly with "/R"
     if (i1psuffix.ge.4.or.i2psuffix.ge.4) i3=2 !Type 2, with "/P"
  endif
  c13=bcall_1
  if(c13(1:3).eq.'CQ_' .or. w(1)(1:1).eq.'<') c13=w(1)
  call pack28(c13,n28a)
  c13=bcall_2
  if(w(2)(1:1).eq.'<') c13=w(2)
  call pack28(c13,n28b)
  ipa=0
  ipb=0
  if(i1psuffix.ge.4.or.index(w(1)//' ','/R ').ge.4) ipa=1
  if(i2psuffix.ge.4.or.index(w(2)//' ','/R ').ge.4) ipb=1
  
  grid4=w(nwords)(1:4)
  if(is_grid4(grid4)) then
     ir=0
     if(w(3).eq.'R ') ir=1
     j1=(ichar(grid4(1:1))-ichar('A'))*18*10*10
     j2=(ichar(grid4(2:2))-ichar('A'))*10*10
     j3=(ichar(grid4(3:3))-ichar('0'))*10
     j4=(ichar(grid4(4:4))-ichar('0'))
     igrid4=j1+j2+j3+j4
  else
     igrid4=MAXGRID4 + irpt
  endif
  if(nwords.eq.2) then
     ir=0
     irpt=1
     igrid4=MAXGRID4+irpt
  endif
  write(c77,1000) n28a,ipa,n28b,ipb,ir,igrid4,i3
1000 format(2(b28.28,b1),b1,b15.15,b3.3)
  return

900 return
end subroutine pack77_1

subroutine packtext77(c13,c71)

  character*13 c13,w
  character*71 c71
  character*42 c
  character*1 qa(10),qb(10)
  data c/' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-./?'/

  call mp_short_init
  qa=char(0)
  w=adjustr(c13)
  do i=1,13
     j=index(c,w(i:i))-1
     if(j.lt.0) j=0
     call mp_short_mult(qb,qa(2:10),9,42)     !qb(1:9)=42*qa(2:9)
     call mp_short_add(qa,qb(2:10),9,j)      !qa(1:9)=qb(2:9)+j
  enddo

  write(c71,1010) qa(2:10)
1010 format(b7.7,8b8.8)

  return
end subroutine packtext77

subroutine unpacktext77(c71,c13)

  integer*1   ia(10)
  character*1 qa(10),qb(10)
  character*13 c13
  character*71 c71
  character*42 c
  equivalence (qa,ia),(qb,ib)
  data c/' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-./?'/

  qa(1)=char(0)
  read(c71,1010) qa(2:10)
1010 format(b7.7,8b8.8)

  do i=13,1,-1
     call mp_short_div(qb,qa(2:10),9,42,ir)
     c13(i:i)=c(ir+1:ir+1)
     qa(2:10)=qb(1:9)
  enddo

  return
end subroutine unpacktext77

subroutine mp_short_ops(w,u)
  character*1 w(*),u(*)
  integer i,ireg,j,n,ir,iv,ii1,ii2
  character*1 creg(4)
  save ii1,ii2
  equivalence (ireg,creg)

  entry mp_short_init
  ireg=256*ichar('2')+ichar('1')
  do j=1,4
     if (creg(j).eq.'1') ii1=j
     if (creg(j).eq.'2') ii2=j
  enddo
  return

  entry mp_short_add(w,u,n,iv)
  ireg=256*iv
  do j=n,1,-1
     ireg=ichar(u(j))+ichar(creg(ii2))
     w(j+1)=creg(ii1)
  enddo
  w(1)=creg(ii2)
  return

  entry mp_short_mult(w,u,n,iv)
  ireg=0
  do j=n,1,-1
     ireg=ichar(u(j))*iv+ichar(creg(ii2))
     w(j+1)=creg(ii1)
  enddo
  w(1)=creg(ii2)
  return

  entry mp_short_div(w,u,n,iv,ir)
  ir=0
  do j=1,n
     i=256*ir+ichar(u(j))
     w(j)=char(i/iv)
     ir=mod(i,iv)
  enddo
  return
  
  return
end subroutine mp_short_ops

end module packjt77sd
