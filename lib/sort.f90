subroutine sort(n,arr)

  integer n,m,nstack
  real arr(n)
  parameter (m=7,nstack=50)
  integer i,ir,j,jstack,k,l,istack(nstack)
  real a,temp

  jstack=0
  l=1
  ir=n
  n0=n

1 if(ir-l.lt.m) then
     do j=l+1,ir
        a=arr(j)
        do i=j-1,1,-1
           if(arr(i).le.a) goto 2
           arr(i+1)=arr(i)
        enddo
        i=0
2       arr(i+1)=a
     enddo

     if(jstack.eq.0) return

     ir=istack(jstack)
     l=istack(jstack-1)
     jstack=jstack-2

  else
     k=(l+ir)/2
     temp=arr(k)
     arr(k)=arr(l+1)
     arr(l+1)=temp

     if(arr(l+1).gt.arr(ir)) then
        temp=arr(l+1)
        arr(l+1)=arr(ir)
        arr(ir)=temp
     endif

     if(arr(l).gt.arr(ir)) then
        temp=arr(l)
        arr(l)=arr(ir)
        arr(ir)=temp
     endif

     if(arr(l+1).gt.arr(l)) then
        temp=arr(l+1)
        arr(l+1)=arr(l)
        arr(l)=temp
     endif

     i=l+1
     j=ir
     a=arr(l)
3    i=i+1
     if(i.gt.n0) then
        do jj=1,n0
           write(99,3001) jj,arr(jj),i,n,ir
3001       format(i10,e12.3,3i10)
        enddo
        close(99)
        stop 'Bounds error in sort.f90'
     endif
     if(arr(i).lt.a) goto 3

4    j=j-1
     if(arr(j).gt.a) goto 4

     if(j.lt.i) goto 5
     temp=arr(i)
     arr(i)=arr(j)
     arr(j)=temp
     goto 3

5    arr(l)=arr(j)
     arr(j)=a
     jstack=jstack+2
     if(jstack.gt.nstack) stop 'nstack too small in sort'

     if(ir-i+1.ge.j-l) then
        istack(jstack)=ir
        istack(jstack-1)=i
        ir=j-1
     else
        istack(jstack)=j-1
        istack(jstack-1)=l
        l=i
     endif

  endif
  goto 1

end subroutine sort
