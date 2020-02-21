subroutine flat4(s,npts0,nflatten)

! Flatten a spectrum for optimum display
! Input:  s(npts)    Linear scale in power
!         nflatten   If nflatten=0, convert to dB but do not flatten
! Output: s(npts)    Flattened, with dB scale


  implicit real*8 (a-h,o-z)
  real*4 s(6827)
  real*4 base
  real*8 x(1000),y(1000),a(5)
  data nseg/10/,npct/10/

  npts=min(6827,npts0)
  if(s(1).gt.1.e29) go to 900         !Boundary between Rx intervals: do nothing
  do i=1,npts
     s(i)=10.0*log10(s(i))
  enddo
  if(nflatten.eq.0) go to 900

  nlen=npts/nseg                      !Length of test segment
  i0=npts/2                           !Midpoint
  k=0
  do n=1,nseg                         !Skip first segment, likely rolloff here
     ib=n*nlen
     ia=ib-nlen+1
     if(n.eq.nseg) ib=npts
     call pctile(s(ia),ib-ia+1,npct,base) !Find lowest npct of points in segment
     do i=ia,ib
        if(s(i).le.base) then
           if (k.lt.1000) k=k+1       !Save these "lower envelope" points
           x(k)=i-i0
           y(k)=s(i)
        endif
     enddo
  enddo
  kz=k
  a=0.
  nterms=5
  
  call polyfit(x,y,y,kz,nterms,0,a,chisqr)  !Fit a low-order polynomial

  do i=1,npts
     t=i-i0
     yfit=a(1)+t*(a(2)+t*(a(3)+t*(a(4)+t*(a(5)))))
     s(i)=s(i)-yfit                    !Subtract the fitted baseline
  enddo

900 return
end subroutine flat4
