real*8 function determ(array,norder)
  implicit real*8 (a-h,o-z)
  real*8 array(10,10)

  determ=1.
  do k=1,norder
     if (array(k,k).ne.0) go to 41
     do j=k,norder
        if(array(k,j).ne.0) go to 31
     enddo
     determ=0.
     go to 60

31   do i=k,norder
        s8=array(i,j)
        array(i,j)=array(i,k)
        array(i,k)=s8
     enddo
     determ=-1.*determ
41   determ=determ*array(k,k)
     if(k.lt.norder) then
        k1=k+1
        do i=k1,norder
           do j=k1,norder
              array(i,j)=array(i,j)-array(i,k)*array(k,j)/array(k,k)
           enddo
        enddo
     end if
  enddo

60 return
end function determ
