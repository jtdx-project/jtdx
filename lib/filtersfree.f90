! This source code file was last time modified by Igor UA3DJY on May 29th, 2018
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine filtersfree(decoded,falsedec)
  
  real datacorr
  character decoded*22,d1*1,d2*2,d3*3,d4*4,d5*5
  logical(1) falsedec

  ndot=0
  nsign=0
  nother=0

  if(decoded(11:12).eq.'73') go to 4
  if(decoded(12:13).eq.'73') go to 4
  if(decoded(1:1).eq.'/') go to 2

  do i=1,13
     d1=decoded(i:i); d2=decoded(i:(i+1)); d3=decoded(i:(i+2)); d4=decoded(i:(i+3)); d5=decoded(i:(i+4))
     if(d2.eq.'73' .or. d2.eq.'TU' .or. d2.eq.'GL') go to 4
!  HA8QJ -07   HA8QJ R-07  
     if(i.gt.4) then
        if((d2.eq.' -' .or. d2.eq.' +') .and. decoded(i+2:i+2).ge.'0' .and. decoded(i+2:i+2).lt.'3') go to 4
        if((d3.eq.' R-' .or. d3.eq.' R+') .and.  decoded(i+3:i+3).ge.'0' .and. decoded(i+3:i+3).lt.'3') go to 4
     endif
     if(d3.eq.'QSL' .or. d3.eq.'TNX' .or. d2.eq.'RR' .or.  &
        d3.eq.'QSO' .or. d3.eq.'CFM' .or. d4.eq.'LOTW' .or. &
        d3.eq.'BND' .or. d3.eq.'QSY' .or. d4.eq.'BAND') go to 4
     if(d3.eq.'MAS' .or. d3.eq.'HNY' .or. d3.eq.'HPY' .or. d3.eq.' DT' .or. &
        d3.eq.'XMS' .or. d5.eq.'EASTE' .or. d5.eq.'PIRAT' .or. d4.eq.'TEST') go to 4
     if(d5.eq.'/HYBR' .or. d5.eq.'/HTML' .or. d5.eq.'/PHOT' .or. d5.eq.'/IMAG' .or. d5.eq.'/JOIN' .or. d5.eq.'QRZ.C' & 
         .or. d4.eq.'/GIF' .or. d4.eq.'/JPG' .or. d4.eq.'/AVI' .or. d4.eq.'/MP4' .or. d4.eq.'/CMD') go to 4
     if(d1.eq.'.') ndot=ndot+1
     if(d1.eq.'-' .or. d1.eq.'+' .or. d1.eq.'?') nsign=nsign+1
     if(d1.eq.'/' .or. d1.eq.'?') nother=nother+1
  enddo

  if(ndot.ge.2 .or. nsign.ge.2 .or. nother.ge.2) go to 2
  if(decoded(1:2).eq.'-0')  go to 2
  if(decoded(1:1).ne.' ' .and. decoded(2:2).eq.' ') go to 2

  if(((decoded(13:13).ge.'A' .and. decoded(13:13).le.'Z') .or.  &
     (decoded(13:13).ge.'0' .and. decoded(13:13).le.'9')) .and. &
     decoded(12:12).eq.' ') go to 2

  if(decoded(1:1).eq.'.' .or. decoded(1:1).eq.'+' .or. decoded(1:1).eq.'?' &
     .or. decoded(1:1).eq.'/' .or. decoded(13:13).eq.'.' .or. decoded(13:13).eq.'+' &
     .or. decoded(13:13).eq.'-' .or. decoded(13:13).eq.'/') go to 2

  if(decoded(2:2).eq.'-' .or. decoded(2:2).eq.'+' .or. decoded(12:12).eq.'.' .or. &
     decoded(12:12).eq.'+' .or. decoded(12:12).eq.'-') go to 2

  if(decoded(12:12).ge.'0' .and. decoded(12:12).le.'9' .and. &
     decoded(13:13).ge.'0' .and. decoded(13:13).le.'9') then

     if(decoded(12:13).ne.'55' .and. decoded(12:13).ne.'73' .and. decoded(12:13).ne.'88') go to 2

  endif
 
  if(decoded(1:1).eq.'0' .and. decoded(2:2).ne.'.') go to 2

  do i=1,12
     if(i.lt.12 .and. decoded(i:i).eq."?" .and. decoded(i+1:i+1).ne." ") go to 2
     if(i.lt.12 .and. decoded(i:i).ne." " .and. decoded(i+1:i+1).eq."+" &
        .and. (decoded(i+2:i+2).le."0" .or. decoded(i+2:i+2).ge."9")) go to 2

     if(decoded(i:i).ge.'A' .and. decoded(i:i).le.'Z' .and. decoded(i+1:i+1).eq.'.' .and. &
        decoded(i+2:i+2).ge.'A' .and. decoded(i+2:i+2).le.'Z') go to 2

     if(decoded(i+1:i+2).eq.'/ ' .or. decoded(i+1:i+2).eq.' /') go to 2

     if(decoded(i:i).ge.'0' .and. decoded(i:i).le.'9' .and. decoded(i+1:i+1).eq.'/' .and. &
        decoded(i+2:i+2).ge.'0' .and. decoded(i+2:i+2).le.'9') go to 2

     if(decoded(i:i).eq.' ' .and. decoded(i+1:i+1).ge.'A' .and. decoded(i+1:i+1).le.'Z' .and.     &
        decoded(i+1:i+1).ne.'F' .and. decoded(i+1:i+1).ne.'G' .and. decoded(i+1:i+1).ne.'I' .and. &
        decoded(i+2:i+2).eq.'/') go to 2

     if(decoded(i:i).eq.' ' .and. decoded(i+1:i+1).ge.'A' .and. decoded(i+1:i+1).le.'Z' .and.     &
        (decoded(i+2:i+2).eq.'.' .or. decoded(i+2:i+2).eq.'+' .or. decoded(i+2:i+2).eq.'-')) go to 2

  enddo

  if(decoded(1:1).eq.'-' .and. decoded(2:2).le.'1' .and. decoded(2:2).ge.'9') go to 2
 
  if(decoded(1:1).ge.'1' .and. decoded(1:1).le.'9' .and.  &
     decoded(2:2).ge.'0' .and. decoded(2:2).le.'9' .and.  &
     decoded(3:3).ge.'0' .and. decoded(3:3).le.'9' .and.  &
     decoded(4:4).ne.'W') go to 2
 
  if(decoded(1:1).ge.'1' .and. decoded(1:1).le.'9' .and.  &
     decoded(2:2).ge.'0' .and. decoded(2:2).le.'9' .and.  &
     decoded(3:3).ne.'W') go to 2

  if(decoded(1:1).ge.'1' .and. decoded(1:1).le.'9' .and.  &
     decoded(2:3).ne.'EL') go to 2

  if(decoded(10:10).ne.'/' .and. decoded(11:11).ne.'/' .and. decoded(12:12).ge.'A' .and. &
     decoded(12:12).le.'Z' .and. decoded(13:13).ge.'0' .and. decoded(12:12).le.'9') go to 2

  call datacor(datapwr,datacorr)

! ban free message if datacorr is less than 1.55
  if(datacorr.lt.1.55) go to 2

  ncount=0
  nchar=0

  do i=1,13
     if(decoded(i:i).eq.' ') exit
     if(decoded(i:i).eq.'/' .or. decoded(i:i).eq.'-' .or. decoded(i:i).eq.'.') nchar=nchar+1
     ncount=ncount+1
  enddo

  if(ncount.eq.13 .and. nchar.ge.0) go to 2  

!print *,"passed"
!print *,decoded

  go to 4

2 falsedec=.true.
!print *,"stopped"
!print *,decoded
 return

4 return
end subroutine filtersfree
