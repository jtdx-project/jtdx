subroutine chkcall(w,bc,cok)

! Check "w" to see if it could be a valid standard callsign or a valid
! compound callsign.
! Return base call "bc" and a logical "cok" indicator.

  character w*13                            !A putative callsign
  character bc*6                            !Base call (tentative)
  character c*1
  logical cok,isdigit,isletter
  
  isdigit(c)=(ichar(c).ge.ichar('0')) .and. (ichar(c).le.ichar('9'))
  isletter(c)=(ichar(c).ge.ichar('A')) .and. (ichar(c).le.ichar('Z'))
  
  cok=.true.
  bc=w(1:6)
  n1=len_trim(w)
  if(n1.gt.11) go to 100
  if(index(w,'.').ge.1) go to 100
  if(index(w,'+').ge.1) go to 100
  if(index(w,'-').ge.1) go to 100
  if(index(w,'?').ge.1) go to 100
  if(n1.gt.6 .and. index(w,'/').le.0) go to 100

  i0=index(w,'/')
  if(max(i0-1,n1-i0).gt.6) go to 100      !Base call must be < 7 characters
  if(i0.ge.2 .and. i0.le.n1-1) then       !Extract base call from compound call
     if(i0-1.le.n1-i0) bc=w(i0+1:n1)//'   '
     if(i0-1.gt.n1-i0) bc=w(1:i0-1)//'   '
  endif

  nbc=len_trim(bc)
  if(nbc.gt.6) go to 100  !Base call should have no more than 6 characters

! One of first two characters (c1 or c2) must be a letter
  if((.not.isletter(bc(1:1))) .and. (.not.isletter(bc(2:2)))) go to 100
  if(bc(1:1).eq.'Q') go to 100              !Calls don't start with Q

! Must have a digit in 2nd or 3rd position
  i1=0
  if(isdigit(bc(2:2))) i1=2
  if(isdigit(bc(3:3))) i1=3
  if(i1.eq.0) go to 100

! Callsign must have a suffix of 1-3 letters
  if(i1.eq.nbc) go to 100
  n=0
  do i=i1+1,nbc
     j=ichar(bc(i:i))
     if(j.lt.ichar('A') .or. j.gt.ichar('Z')) go to 100
     n=n+1
  enddo
  if(n.ge.1 .and. n.le.3) go to 200

100 cok=.false.
     
200 return
end subroutine chkcall
