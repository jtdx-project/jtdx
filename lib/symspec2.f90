! This source code file was last time modified by Igor UA3DJY on May 30th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine symspec2(c5,nz3,nsps8,nspsd,fsample,freq,drift,snrdb,schk,    &
     i1SoftSymbolsScrambled,mrs,mrs2,s3)

! Compute soft symbols from the final downsampled data

  complex c5(0:4096-1)
  complex z
  integer*1 i1SoftSymbolsScrambled(207)
  real aa(3),ss2(0:8,85),s3(0:7,69)
  integer mrs(69),mrs2(69)
  include 'jt9sync.f90'
  data scale/10.0/

  aa(1)=-1500.0/nsps8
  aa(2)=0.
  aa(3)=0.
  do i=0,8                                         !Loop over the 9 tones
     if(i.ge.1) call twkfreq(c5,c5,nz3,fsample,aa)
     m=0
     k=-1
     do j=1,85                                     !Loop over all symbols
        z=0.
        do n=1,nspsd                               !Sum over 16 samples
           k=k+1
           z=z+c5(k)
        enddo
        ss2(i,j)=real(z)**2 + aimag(z)**2        !Symbol speactra, data and sync
        if(i.ge.1 .and. isync(j).eq.0) then
           m=m+1
           s3(i-1,m)=ss2(i,j)                   !Symbol speactra, data only
        endif
     enddo
  enddo

  call chkss2(ss2,freq,drift,schk)
  
  i1=1; i2=1
! find most reliable symbol tone values
  do j=1,69
     s1=0.0
     do i=0,7
        if(s3(i,j).gt.s1) then
           s1=s3(i,j)
           i1=i                              !Most reliable
        endif
     enddo
     s2=0.0
     do i=0,7
        if(i.ne.i1 .and. s3(i,j).gt.s2) then
           s2=s3(i,j)
           i2=i                              !Second most reliable
        endif
     enddo
     mrs(j)=i1
     mrs2(j)=i2
  enddo

  ss=0.
  sig=0.
  do j=1,69
     smax=0.
     do i=0,7
        smax=max(smax,s3(i,j))
        ss=ss+s3(i,j)
     enddo
     sig=sig+smax
     ss=ss-smax
  enddo
  ave=ss/(69*7)                           !Baseline
  call pctile(ss2,9*85,35,xmed)
  s3=s3/ave
  sig=sig/69.                             !Signal
  t=max(1.0,sig - 1.0)
  snrdb=db(t) - 61.3

!if(abs(freq-2750.).lt.0.2) then
!do i=1,69
!do k=0,7
!print *,s3(k,i)
!enddo
!enddo
!endif

  m0=3
  k=0
  do j=1,69
        smax=0.
        do i=0,7
           if(s3(i,j).gt.smax) smax=s3(i,j)
        enddo

     do m=m0-1,0,-1                   !Get bit-wise soft symbols
        if(m.eq.2) then
           r1=max(s3(4,j),s3(5,j),s3(6,j),s3(7,j))
           r0=max(s3(0,j),s3(1,j),s3(2,j),s3(3,j))
        else if(m.eq.1) then
           r1=max(s3(2,j),s3(3,j),s3(4,j),s3(5,j))
           r0=max(s3(0,j),s3(1,j),s3(6,j),s3(7,j))
        else
           r1=max(s3(1,j),s3(2,j),s3(4,j),s3(7,j))
           r0=max(s3(0,j),s3(3,j),s3(5,j),s3(6,j))
        endif

        k=k+1
        i4=nint(scale*(r1-r0))
        if(i4.lt.-127) i4=-127
        if(i4.gt.127) i4=127
!        i4=i4+128
!        if(i4.le.127) i1SoftSymbolsScrambled(k)=i4
!        if(i4.ge.128) i1SoftSymbolsScrambled(k)=i4-256
        i1SoftSymbolsScrambled(k)=i4
     enddo
  enddo

!  if(abs(freq-2750.).lt.0.2) then
!  do i=1,207
!  print *,i1SoftSymbolsScrambled(i)
!  enddo
!  endif

  return
end subroutine symspec2
