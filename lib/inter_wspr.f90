subroutine inter_wspr(id,ndir)

! Interleave (ndir=1) or de-interleave (ndir=-1) the array id.

  integer*1 id(0:161),itmp(0:161)
  integer j0(0:161)
  logical first
  data first/.true./
  save

  if(first) then
! Compute the interleave table using bit reversal.
     k=-1
     do i=0,255
        n=0
        ii=i
        do j=0,7
           n=n+n
           if(iand(ii,1).ne.0) n=n+1
           ii=ii/2
        enddo
        if(n.le.161) then
           k=k+1
           j0(k)=n
        endif
     enddo
     first=.false.
  endif

  if(ndir.eq.1) then
     do i=0,161
        itmp(j0(i))=id(i)
     enddo
  else
     do i=0,161
        itmp(i)=id(j0(i))
     enddo
  endif

  do i=0,161
     id(i)=itmp(i)
  enddo

  return
end subroutine inter_wspr
