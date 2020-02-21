subroutine entail(dgen,data0)

! Move 72-bit packed data from 6-bit to 8-bit symbols and add a zero tail.
  integer dgen(13)
  integer*1 data0(13)

  i4=0
  k=0
  m=0
  do i=1,12
     n=dgen(i)
     do j=1,6
        k=k+1
        i4=i4+i4+iand(1,ishft(n,j-6))
        i4=iand(i4,255)
        if(k.eq.8) then
           m=m+1
           if(i4.gt.127) i4=i4-256
           data0(m)=i4
           k=0
        endif
     enddo
  enddo
  do m=10,13
     data0(m)=0
  enddo

  return
end subroutine entail

