subroutine chkhist(mrsym,nmax,ipk)

  integer mrsym(63)
  integer hist(0:63)

  hist=0
  do j=1,63
     i=mrsym(j)
     hist(i)=hist(i)+1
  enddo

  nmax=0
  do i=0,63
     if(hist(i).gt.nmax) then
        nmax=hist(i)
        ipk=i+1
     endif
  enddo

  return
end subroutine chkhist
