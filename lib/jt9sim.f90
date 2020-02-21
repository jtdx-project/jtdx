! This source code file was last time modified by Igor UA3DJY on May 25th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

program jt9sim

! Generate simulated data for testing of WSJT-X

  use wavhdr
  use packjt
  parameter (NTMAX=120)
  parameter (NMAX=NTMAX*12000)
  type(hdr) h
  integer*2 iwave(NMAX)                  !Generated waveform (no noise)
  real*4 dat(NMAX)
  real*8 f0,f,dt,twopi,phi,dphi,baud,fspan,fsample,freq
  character msg0*22,message*22,msgsent*22,arg*8,fname*11
!  character msg*22

  integer*4 i4tone(85)             !Channel symbols (values 0-8)
!  integer*4 i4DataSymNoGray(69)    !Data Symbols, values 0-7
!  integer*1 i1ScrambledBits(207)   !Unpacked bits, scrambled order
!  integer*1 i1Bits(207)            !Encoded information-carrying bits
!  integer*1 i1SoftSymbols(207)
  include 'jt9sync.f90'

  nargs=iargc()
  if(nargs.ne.6) then
     print*,'Usage:   jt9sim "message"     fspan nsigs minutes SNR nfiles'
     print*,'Example: jt9sim "CQ K1ABC FN42" 200  20      2    -28    1'
     print*,' '
     print*,'Enter message = "" to use entries in msgs.txt.'
     print*,'Enter SNR = 0 to generate a range of SNRs.'
     print*,'Enter SNR = 99 to generate a noiseless signal at frequency fspan'
     go to 999
  endif

  call getarg(1,msg0)  
  call fmtmsg(msg0,iz)
  message=msg0                       !Transmitted message
  call getarg(2,arg)
  read(arg,*) fspan                  !Total freq range (Hz)
  call getarg(3,arg)
  read(arg,*) nsigs                  !Number of signals in each file
  call getarg(4,arg)
  read(arg,*) minutes                !Length of file (1 2 5 10 30 minutes)
  call getarg(5,arg)
  read(arg,*) snrdb                  !S/N in dB (2500 hz reference BW)
  call getarg(6,arg)
  read(arg,*) nfiles                 !Number of files     

  rmsdb=25.
  rms=10.0**(0.05*rmsdb)
  fsample=12000.d0                   !Sample rate (Hz)
  dt=1.d0/fsample                    !Sample interval (s)
  twopi=8.d0*atan(1.d0)
  npts=12000*(60*minutes-6)
  nsps=0
  if(minutes.eq.1)  nsps=6912
  if(minutes.eq.2)  nsps=15360
  if(minutes.eq.5)  nsps=40960
  if(minutes.eq.10) nsps=82944
  if(minutes.eq.30) nsps=252000
  if(nsps.eq.0) stop 'Bad value for minutes.'

!  f0=1400.d0                         !Center frequency (Hz)
   f0=2750.d0                         !Center frequency (Hz) 
!  f0=3000.d0                         !Center frequency (Hz)

!  f0=1500.0
!  if(minutes.eq.5)  f0=1100.
!  if(minutes.eq.10) f0=1050.
!  if(minutes.eq.30) f0=1025.


  call init_random_seed()       ! seed Fortran RANDOM_NUMBER generator
  call sgran()                  ! see C rand generator (used in gran)

  h=default_header(12000,npts)  
  k=0                                !Silence compiler warning

  if(msg0(1:3).eq.'sin') read(msg0(4:),*) sinfreq
  
  if(message.eq."") open(12,file='msgs.txt',status='old')

  write(*,1000)
1000 format('File  N    freq      S/N  Message'/    &
            '---------------------------------------------------')

  do ifile=1,nfiles                            !Loop over all files
     nmin=(ifile-1)*minutes
     ihr=nmin/60
     imin=mod(nmin,60)
     write(fname,1002) ifile         !Output filename
1002 format('000000_',i4.4)
     open(10,file=fname//'.wav',access='stream',status='unknown')

     if(snrdb.lt.90) then
        do i=1,npts
           dat(i)=gran()
        enddo
     else
        dat(1:npts)=0.
     endif

     if(msg0.ne.'                      ') then
        call gen9(message,0,msgsent,i4tone,itype) !Encode message into tone #s
     endif

     rewind 12
     do isig=1,nsigs                            !Loop over multiple signals

        if(msg0.eq.'                      ') then
           read(12,1004) message                !Use pre-generated message texts
1004       format(a22)
           call gen9(message,0,msgsent,i4tone,itype)
        endif

        f=f0
        if(nsigs.gt.1) f=f0 - 0.5d0*fspan + fspan*(isig-1.d0)/(nsigs-1.d0)
        snrdbx=snrdb 
!        snrdbx=snrdb + (ifile-1)*4.0
        sig=10.0**(0.05*snrdbx)
        if(snrdb.gt.90.0) sig=1.0
        write(*,1020) ifile,isig,f,snrdbx,msgsent
1020    format(i3,i4,f10.3,f7.1,2x,a22)

        phi=0.
        baud=12000.d0/nsps
        k=12000                             !Start audio at t = 1.0 s
!        f1=0.0001 * (ifile-1)
        f1=0.
!        print*,ifile-1,f1
        dphi2=0.
        ddphi2=twopi*f1*dt/60.0
        do isym=1,85
           freq=f + i4tone(isym)*baud
           if(msg0(1:3).eq.'sin') freq=sinfreq
           dphi=twopi*freq*dt + dphi2
           do i=1,nsps
              phi=phi + dphi
              dphi2=dphi2 + ddphi2
              if(phi.lt.-twopi) phi=phi+twopi
              if(phi.gt.twopi) phi=phi-twopi
              xphi=phi
              k=k+1
              dat(k)=dat(k) + sig*sin(xphi)  !Use lookup table for i*2 sin(x) ?
           enddo
        enddo
     enddo

     fac=32767.0/nsigs
     if(snrdb.ge.90.0) iwave(1:npts)=nint(fac*dat(1:npts))
     if(snrdb.lt.90.0) iwave(1:npts)=nint(rms*dat(1:npts))

     write(10) h,iwave(1:npts)
     close(10)

! We're done!  Now decode the data symbols from i4tone, as a test.
!     if(msg0.ne.'                      ') then
!        j=0
!        do i=1,85
!           if(isync(i).eq.1) cycle
!           j=j+1
!           i4DataSymNoGray(j)=igray(i4tone(i)-1,-1)
!        enddo
!        call unpackbits(i4DataSymNoGray,69,3,i1ScrambledBits)
!        call interleave9(i1ScrambledBits,-1,i1Bits)
! 
!        do i=1,206
!           i4=-10
!           if(i1Bits(i).eq.1) i4=10
!           i1SoftSymbols(i)=i4
!        enddo
!        limit=1000
!        call jt9fano(i1SoftSymbols,limit,nlim,msg)
!        if(msg.ne.msg0) print*,'Decode error: ',msg0,' ',msg
!     endif
  enddo

999 end program jt9sim
