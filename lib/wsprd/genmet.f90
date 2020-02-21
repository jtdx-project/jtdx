program genmet

  character*12 arg
  integer hist(-128:128)
  lim(x)=min(127,max(-128,nint(scale*x)))

  nargs=iargc()
  if(nargs.ne.4) then
     print*,'Usage:   genmet  bw  scale snr   iters'
     print*,'Example: genmet 1.46  20   -24  1000000'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) bw
  call getarg(2,arg)
  read(arg,*) scale
  call getarg(3,arg)
  read(arg,*) snr
  call getarg(4,arg)
  read(arg,*) iters

  hist=0
  s=sqrt(2500.0/bw) * 10.0**(0.05*snr)
  fac=1.0/sqrt(2.0)
  do iter=1,iters
     x1=fac*gran()
     y1=fac*gran()
     x0=fac*gran()
     y0=fac*gran()
     r=(x1+s)**2 + y1*y1 - x0*x0 - y0*y0
     hist(lim(r))=hist(lim(r))+1
  enddo

  xln2=log(2.0)
  do i=-128,127
     p1=hist(i)/dfloat(iters)
     j=-i
     if(j.gt.127) j=127
     p0=hist(j)/dfloat(iters)
     xlhd0=log(max(0.001,2.0*p0/(p0+p1)))/xln2
     xlhd1=log(max(0.001,2.0*p1/(p0+p1)))/xln2
     write(13,1010) i/scale,hist(i)/dfloat(iters)
1010 format(f8.3,f12.9)
     write(14,1012) i+128,xlhd0,xlhd1
1012 format(i4,2f8.3)
  enddo

999 end program genmet


