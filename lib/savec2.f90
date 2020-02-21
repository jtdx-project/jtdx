integer function savec2(c2name,ntrseconds,f0m1500)

! Array c0() has complex samples at 1500 Hz sample rate.
! WSPR-2:  downsample by 1/4 to produce c2, centered at 1500 Hz
! WSPR-15: downsample by 1/32 to produce c2, centered at 1612.5 Hz

  parameter (NDMAX=120*1500)         !Sample intervals at 1500 Hz rate
  parameter (MAXFFT=256*1024)

  character*(*) c2name
  character*14 outfile
  real*8 f0m1500
  complex c0
  complex c1(0:MAXFFT-1)
  complex c2(0:65535)
  common/c0com/c0(0:NDMAX-1)

  ntrminutes=ntrseconds/60
  npts=114*1500
  nfft1=262144
  if(ntrminutes.eq.15) then
     npts=890*1500
     nfft1=MAXFFT
  endif
  df1=1500.0/nfft1
  fac=1.0/nfft1
  c1(0:npts-1)=fac*c0(0:npts-1)
  c1(npts:nfft1-1)=0.

  call four2a(c1,nfft1,1,1,1)                 !Complex FFT to frequency domain

! Select the desired frequency range
  nfft2=65536
  nh2=nfft2/2
  if(ntrminutes.eq.2) then
     c2(0:nh2)=c1(0:nh2)
     c2(nh2+1:nfft2-1)=c1(nfft1-nh2+1:nfft1-1)
  else
     i0=nint(112.5/df1)
     c2(0:nh2)=c1(i0:i0+nh2)
     c2(nh2+1:nfft2-1)=c1(i0-nh2+1:i0-1)
  endif

  call four2a(c2,nfft2,1,-1,1)      !Shorter complex FFT, back to time domain

! Write complex time-domain data to disk.
  i1=index(c2name,'.c2')
  outfile=c2name(i1-11:i1+2)
  open(18,file=c2name,status='unknown',access='stream', iostat=ioerr)
  if (ioerr.eq.0) then
     write(18) outfile,ntrminutes,f0m1500,c2(0:45000-1)
     close(18)
  endif
  savec2 = ioerr
end function savec2
