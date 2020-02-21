subroutine watterson(c,npts,fs,delay,fspread)

  complex c(0:npts-1)
  complex c2(0:npts-1)
  complex cs1(0:npts-1)
  complex cs2(0:npts-1)

  nonzero=0
  df=fs/npts
  if(fspread.gt.0.0) then
     do i=0,npts-1
        xx=gran()
        yy=gran()
        cs1(i)=0.707*cmplx(xx,yy)
        xx=gran()
        yy=gran()
        cs2(i)=0.707*cmplx(xx,yy)
     enddo
     call four2a(cs1,npts,1,-1,1)     !To freq domain
     call four2a(cs2,npts,1,-1,1)
     do i=0,npts-1
        f=i*df
        if(i.gt.npts/2) f=(i-npts)*df
        x=(f/(0.5*fspread))**2
        a=0.
        if(x.le.50.0) then
           a=exp(-x)
        endif
        cs1(i)=a*cs1(i)
        cs2(i)=a*cs2(i)
        if(abs(f).lt.10.0) then
           p1=real(cs1(i))**2 + aimag(cs1(i))**2
           p2=real(cs2(i))**2 + aimag(cs2(i))**2
           if(p1.gt.0.0) nonzero=nonzero+1
!           write(62,3101) f,p1,p2,db(p1+1.e-12)-60,db(p2+1.e-12)-60
!3101       format(f10.3,2f12.3,2f10.3)
        endif
     enddo
     call four2a(cs1,npts,1,1,1)     !Back to time domain
     call four2a(cs2,npts,1,1,1)
     cs1(0:npts-1)=cs1(0:npts-1)/npts
     cs2(0:npts-1)=cs2(0:npts-1)/npts
  endif

  nshift=nint(0.001*delay*fs)
  c2(0:npts-1)=cshift(c(0:npts-1),nshift)
  sq=0.
  do i=0,npts-1
     if(nonzero.gt.1) then
        c(i)=0.5*(cs1(i)*c(i) + cs2(i)*c2(i))
     else
        c(i)=0.5*(c(i) + c2(i))
     endif
     sq=sq + real(c(i))**2 + aimag(c(i))**2
!     write(61,3001) i/12000.0,c(i)
!3001 format(3f12.6)
  enddo
  rms=sqrt(sq/npts)
  c=c/rms

  return
end subroutine watterson
