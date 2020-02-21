! This source code file was last time modified by Igor UA3DJY on April 6th, 2018
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine process_dd(lagcc,nmode,ntxmode,nzhsym)

  use jt65_mod6, only : dd
  use jt9_mod1
  logical(1), intent(in) :: lagcc

  if(.not.lagcc) dd9(0:NPTS9-1)=dd(1:NPTS9) ! NPTS9=618624 for decoding on 52nd second, NPTS9=597888
  if(lagcc .and. (nmode.eq.10 .or. nmode.eq.9)) then
     call agcc(nmode,ntxmode)
  endif

  if(nmode.ne.10 .and. nmode.ne.9) then
!    dd(1+nshift:npts)=dd(1:npts-nshift) ! NPTS=624000
     dd(26001:624000)=dd(1:598000)
!    dd(1:nshift)=0.0
     dd(1:26000)=0.0

     if(lagcc) then
        call agcc(nmode,ntxmode)
        if(nmode.eq.65+9) then
!          id2(0:580464-1)=nint(dd(1+nshift:580464+nshift))
           dd9(0:597999)=dd(26001:624000)
           if(nzhsym.eq.173) dd9(597888:597999)=0.
           dd9(598000:NPTS9-1)=0.
        endif
     endif
  endif

  return
end subroutine process_dd