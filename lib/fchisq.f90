real function fchisq(c3,npts,fsample,a)

  parameter (NMAX=85*16)
  complex c3(npts)
  complex c4(NMAX)
  real a(3)
  complex z
  data a1,a2,a3/99.,99.,99./
  include 'jt9sync.f90'
  save

  if(a(1).ne.a1 .or. a(2).ne.a2 .or. a(3).ne.a3) then
     a1=a(1)
     a2=a(2)
     a3=a(3)
     call twkfreq(c3,c4,npts,fsample,a)
  endif

! Get sync power.
  nspsd=16
  sum1=0.
  sum0=0.
  k=-1
  do i=1,85
     z=0.
     do j=1,nspsd
        k=k+1
        z=z+c4(k+1)
     enddo
     pp=real(z)**2 + aimag(z)**2     
     if(isync(i).eq.1) then
        sum1=sum1+pp
     else
        sum0=sum0+pp
     endif
  enddo
  sync_4992=(sum1/16.0)/(sum0/69.0) - 1.0             !r4992
  sync_4993=sum1/10000.0                              !r4993+
!  write(80,3001) 1.e-5*sum1,1.e-5*sum0,sync_4992,sync_4993,sync
!3001 format(5f11.4)
  fchisq=-sync_4993

  return
end function fchisq
