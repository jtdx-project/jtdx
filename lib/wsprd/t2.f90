program t2
  
  df=375.0/65536.0
  do i=1,65536
    w=1.0/(1.0 + ((i-32768)/26214.0)**20)
    f=(i-32768)*df
    write(13,1010) f,w
1010 format(2f15.6)
 enddo

end program t2
