subroutine graycode(ia,n,idir,ib)

  integer ia(n),ib(n)
  do i=1,n
     ib(i)=igray(ia(i),idir)
  enddo

  return
end subroutine graycode
