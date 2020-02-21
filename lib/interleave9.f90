subroutine interleave9(ia,ndir,ib)
  integer*1 ia(0:205),ib(0:205)
  integer j0(0:205)
  logical first
  data first/.true./
  save first,j0                     !Save not working, or j0 overwritten ???

  if(first) then
     k=-1
     do i=0,255
        m=i
        n=iand(m,1)
        n=2*n + iand(m/2,1)
        n=2*n + iand(m/4,1)
        n=2*n + iand(m/8,1)
        n=2*n + iand(m/16,1)
        n=2*n + iand(m/32,1)
        n=2*n + iand(m/64,1)
        n=2*n + iand(m/128,1)
        if(n.le.205) then
           k=k+1
           j0(k)=n
        endif
     enddo
!     first=.false.
  endif

  if(ndir.gt.0) then
     do i=0,205
        ib(j0(i))=ia(i)
     enddo
  else
     do i=0,205
        ib(i)=ia(j0(i))
     enddo
  endif

  return
end subroutine interleave9
