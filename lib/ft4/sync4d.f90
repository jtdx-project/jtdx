subroutine sync4d(cd0,i0,ctwk,itwk,sync)

! Compute sync power for a complex, downsampled FT4 signal.

  include 'ft4_params.f90'
  parameter(NP=NMAX/NDOWN,NSS=NSPS/NDOWN)
  complex cd0(0:NP-1)
  complex csynca(2*NSS),csyncb(2*NSS),csyncc(2*NSS),csyncd(2*NSS)
  complex csync2(2*NSS)
  complex ctwk(2*NSS)
  complex z1,z2,z3,z4
  logical first
  integer icos4a(0:3),icos4b(0:3),icos4c(0:3),icos4d(0:3)
  data icos4a/0,1,3,2/
  data icos4b/1,0,2,3/
  data icos4c/2,3,1,0/
  data icos4d/3,2,0,1/
  data first/.true./
  save first,twopi,csynca,csyncb,csyncc,csyncd,fac

  p(z1)=(real(z1*fac)**2 + aimag(z1*fac)**2)**0.5          !Statement function for power

  if( first ) then
    twopi=8.0*atan(1.0)
    k=1
    phia=0.0
    phib=0.0
    phic=0.0
    phid=0.0
    do i=0,3
      dphia=2*twopi*icos4a(i)/real(NSS) 
      dphib=2*twopi*icos4b(i)/real(NSS) 
      dphic=2*twopi*icos4c(i)/real(NSS) 
      dphid=2*twopi*icos4d(i)/real(NSS) 
      do j=1,NSS/2
        csynca(k)=cmplx(cos(phia),sin(phia)) 
        csyncb(k)=cmplx(cos(phib),sin(phib)) 
        csyncc(k)=cmplx(cos(phic),sin(phic)) 
        csyncd(k)=cmplx(cos(phid),sin(phid)) 
        phia=mod(phia+dphia,twopi)
        phib=mod(phib+dphib,twopi)
        phic=mod(phic+dphic,twopi)
        phid=mod(phid+dphid,twopi)
        k=k+1
      enddo
    enddo
    first=.false.
    fac=1.0/(2.0*NSS)
  endif

  i1=i0                            !four Costas arrays
  i2=i0+33*NSS
  i3=i0+66*NSS
  i4=i0+99*NSS

  z1=0.
  z2=0.
  z3=0.
  z4=0.

  if(itwk.eq.1) csync2=ctwk*csynca      !Tweak the frequency
  z1=0.
  if(i1.ge.0 .and. i1+4*NSS-1.le.NP-1) then
    z1=sum(cd0(i1:i1+4*NSS-1:2)*conjg(csync2))
  elseif( i1.lt.0 ) then
    npts=(i1+4*NSS-1)/2
    if(npts.le.16) then
      z1=0.
    else
      z1=sum(cd0(0:i1+4*NSS-1:2)*conjg(csync2(2*NSS-npts:)))
    endif
  endif

  if(itwk.eq.1) csync2=ctwk*csyncb      !Tweak the frequency
  if(i2.ge.0 .and. i2+4*NSS-1.le.NP-1) z2=sum(cd0(i2:i2+4*NSS-1:2)*conjg(csync2))

  if(itwk.eq.1) csync2=ctwk*csyncc      !Tweak the frequency
  if(i3.ge.0 .and. i3+4*NSS-1.le.NP-1) z3=sum(cd0(i3:i3+4*NSS-1:2)*conjg(csync2))

  if(itwk.eq.1) csync2=ctwk*csyncd      !Tweak the frequency
  z4=0.
  if(i4.ge.0 .and. i4+4*NSS-1.le.NP-1) then
    z4=sum(cd0(i4:i4+4*NSS-1:2)*conjg(csync2))
  elseif( i4+4*NSS-1.gt.NP-1 ) then
    npts=(NP-1-i4+1)/2
    if(npts.le.16) then
      z4=0.
    else
      z4=sum(cd0(i4:i4+2*npts-1:2)*conjg(csync2(1:npts)))
    endif
  endif

  sync = p(z1) + p(z2) + p(z3) + p(z4)

  return
end subroutine sync4d
