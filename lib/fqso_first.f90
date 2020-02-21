! This source code file was last time modified by Igor Chernikov UA3DJY on May 25th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine fqso_first(nfqso,nfhinted,ca,ncand)

! If a candidate was found within +/- nfhinted of nfqso, move it into ca(1).

  type candidate
     real freq
     real dt
     real sync
  end type candidate
  type(candidate) ca(300),cb

  dmin=1.e30
  i0=0
  do i=1,ncand
     d=abs(ca(i)%freq-nfqso)
     if(d.lt.dmin) then
        i0=i
        dmin=d
     endif
  enddo

  if(dmin.lt.float(nfhinted)) then
     cb=ca(i0)
     do i=i0,2,-1
        ca(i)=ca(i-1)
     enddo
     ca(1)=cb
  endif

  return
end subroutine fqso_first
