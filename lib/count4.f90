program count4

  parameter(NMAX=1000)
  character*47 line
  real snr(NMAX)
  real dt(NMAX)
  real f(NMAX)

  open(10,file='/users/joe/appdata/local/wsjt-x/all.txt',status='old')

  read(10,1000,end=10) line
1000 format(a47)

  nsync1=0
  nsync2=0
  n1=0
  n2=0
  nerr=0

  do i=1,99999
     read(10,1000,end=10) line
     if(line(47:47).ne.' ') cycle                !Skip average decodes
     if(line(20:20).eq.'*') nsync1=nsync1+1
     if(line(20:20).eq.'#') nsync2=nsync2+1
     if(line(22:34).eq.'CQ K1ABC FN42') then
        n2=n2+1                                  !Correlation decode
        read(line,1002) snr(n2),dt(n2),f(n2)
1002    format(4x,f4.0,f5.2,f5.0)
        if(line(42:42).eq.'*') n1=n1+1           !Convolutional decode
     else
        if(line(22:34).ne.'             ') nerr=nerr+1
     endif
  enddo

10 call stats(snr,n2,snrave,snrdev)
  call stats(dt,n2,dtave,dtdev)
  call stats(f,n2,fave,fdev)

write(*,1010) nsync1,nsync2,n1,n2,nerr,snrave,dtave,fave,snrdev,dtdev,fdev
1010 format(5i5,f7.1,f7.2,f7.0/25x,f7.1,f7.2,f7.0)

end program count4

subroutine stats(x,nz,ave,rms)
  real x(nz)

  ave=0.
  rms=0.
  if(nz.gt.0) ave=sum(x)/nz
  x=x-ave
  if(nz.gt.1) rms=sqrt(dot_product(x,x)/(nz-1))

  return
end subroutine stats
