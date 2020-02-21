! This source code file was last time modified by Igor UA3DJY on June 5th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine peakdt10(c2,nsps8,nspsd,c3,xdt,tryhint,xdtrxf)

  parameter (NZ2=1512,NZ3=1360)
  complex c2(0:NZ2-1)
  complex c3(0:NZ3-1)
  complex z
  real p(0:3300)
  logical(1) tryhint
  include 'jt10sync.f90'

  lag1=39; lag2=291; lag0=123
  p=0.
  i0=5*nspsd
  tsymbol=nsps8/1500.0
  dtlag=tsymbol/nspsd
  
  if(tryhint) then 
     lagpk=nint(xdtrxf/dtlag)+lag0
     xdt=xdtrxf
     goto 2 
  endif

  do i=0,NZ2-1
     z=1.e-3*sum(c2(max(i-(nspsd-1),0):i))
     p(i0+i)=real(z)**2 + aimag(z)**2      !Integrated power per 1/16 of the symbol
  enddo

  smax=0.
  lagpk=0
  do lag=lag1,lag2
     sum0=0.
     sum1=0.
     j=-nspsd
     do i=1,85
        j=j+nspsd
        if(isync(i).eq.1 .or. isync(i).eq.2) then
           sum1=sum1+p(j+lag)
        else
           sum0=sum0+p(j+lag)
        endif
     enddo
     ss=(sum1/16.0)/(sum0/69.0) - 1.0
     xdt=(lag-lag0)*dtlag
     if(ss.gt.smax) then
        smax=ss
        lagpk=lag
     endif
  enddo

  xdt=(lagpk-lag0)*dtlag

2 do i=0,NZ3-1
     j=i+lagpk-i0-nspsd+1
     if(j.ge.0 .and. j.lt.NZ2) then
        c3(i)=c2(j)
     else
        c3(i)=0.
     endif
  enddo
 
  return
end subroutine peakdt10
