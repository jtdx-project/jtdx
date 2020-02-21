! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

program jt65sim

! Generate simulated JT65 data for testing WSJT-X

  use wavhdr
  use packjt
  use options
  use jt65_mod4
  parameter (NMAX=54*12000)              ! = 648,000
  parameter (NFFT=10*65536,NH=NFFT/2)
  parameter (MAXCALLS=35000,MAXRPT=60)  ! 34999 calls in CALL3.TXT
  type(hdr) h                            !Header for .wav file
  integer*2 iwave(NMAX)                  !Generated waveform
  integer*4 itone(126)                   !Channel symbols (values 0-65)
  integer dgen(12)                       !Twelve 6-bit data symbols
  integer sent(63)                       !RS(63,12) codeword
  real*4 xnoise(NMAX)                    !Generated random noise
  real*4 dat(NMAX)                       !Generated real data
  complex cdat(NMAX)                     !Generated complex waveform
  complex cspread(0:NFFT-1)              !Complex amplitude for Rayleigh fading
  complex z
  real*8 f0,dt,twopi,phi,dphi,baud,fsample,freq,sps
  character*6 callc3(MAXCALLS)
  character*4 gridc3(MAXCALLS)
  character msg*22,fname*11,csubmode*1,call1*6,call2*6,c,optarg*500,numbuf*32
  character line*180,callsign*12,grid*4,grid2*4,rest*4
  logical :: display_help=.false.,seed_prngs=.true.
  type (option) :: long_options(5) = [ &
    option ('help',.false.,'h','Display this help message',''),                                &
    option ('time-offset',.true.,'t','Mean Time delta value, Gaussian distribution, default SECONDS=0.0','SECONDS'),&
    option ('num-files',.true.,'f','Number of files to generate, default FILES=1','FILES'),    &
    option ('no-prng-seed',.false.,'p','Do not seed PRNGs (use for reproducible tests)',''),   &
    option ('nsignals',.true.,'n','number of signals in file, default =1','') ]
  
  character*4 rpt(MAXRPT),r73(3)
  data rpt/ '-01','-02','-03','-04','-05',         &
           '-06','-07','-08','-09','-10',          &
           '-11','-12','-13','-14','-15',          &
           '-16','-17','-18','-19','-20',          &
           '-21','-22','-23','-24','-25',          &
           '-26','-27','-28','-29','-30',          &
           'R-01','R-02','R-03','R-04','R-05',     &
           'R-06','R-07','R-08','R-09','R-10',     &
           'R-11','R-12','R-13','R-14','R-15',     &
           'R-16','R-17','R-18','R-19','R-20',     &
           'R-21','R-22','R-23','R-24','R-25',     &
           'R-26','R-27','R-28','R-29','R-30'/     
  data r73/'RRR','73','RR73'/
  
! Default parameters:
  csubmode='A'
  mode65=1
  nsigs=1
  fspread=0.
  ncross=0.
  xdt=0.
  snrdb=0.
  nfiles=1

  do
     call getopt('h:t:f:pn:',long_options,c,optarg,narglen,nstat,noffset,nremain,.true.)
     if( nstat .ne. 0 ) then
        exit
     end if
     select case (c)
     case ('h')
        display_help = .true.
     case ('c')
        read (optarg(:narglen), *,err=10) ncross
     case ('t')
        read (optarg(:narglen), *) numbuf
        if (numbuf(1:1) == '\') then
           read (numbuf(2:), *,err=10) xdt
        else
           read (numbuf, *,err=10) xdt
        end if
     case ('f')
        read (optarg(:narglen), *,err=10) nfiles
     case ('p')
        seed_prngs=.false.
     case ('n')
        read (optarg(:narglen), *,err=10) nsigs
     end select
     cycle
10   display_help=.true.
     print *, 'Optional argument format error for option -', c
  end do

  if(display_help .or. nstat.lt.0 .or. nremain.ge.1) then
     print *, ''
     print *, 'Usage: jt65sim [OPTIONS]'
     print *, ''
     print *, '       Generate one or more simulated JT65 signals in .WAV file(s)'
     print *, ''
     print *, 'Example: jt65sim -m B -n 10 -d 0.2 -t 0.0 -f 4'
     print *, ''
     print *, 'OPTIONS: NB Use \ (\\ on *nix shells) to escape -ve arguments'
     print *, ''
     do i = 1, size (long_options)
       call long_options(i) % print (6)
     end do
     go to 999
  endif

  if (seed_prngs) then
     call init_random_seed()    ! seed Fortran RANDOM_NUMBER generator
     call sgran()               ! see C rand generator (used in gran)
  end if

     open(23,file='CALL3.TXT',status='unknown')
     icall=0
     j=0
     do i=1,MAXCALLS
        read(23,1001,end=20) line
1001    format(a80)
        if(line(1:4).eq.'ZZZZ') exit
        if(line(1:2).eq.'//') cycle
        i1=index(line,',')
        if(i1.lt.4) cycle
        i2=index(line(i1+1:),',')
        if(i2.lt.5) cycle
        i2=i2+i1
        callsign=line(1:i1-1)
        grid=line(i1+1:i2-1)
        j=j+1
        callc3(j)=callsign(1:6)               !### Fix for compound callsigns!
        gridc3(j)=grid
     enddo
20   ncalls=j
     close(23)

  rms=100.
  fsample=12000.d0                   !Sample rate (Hz)
  dt=1.d0/fsample                    !Sample interval (s)
  twopi=8.d0*atan(1.d0)
  npts=54*12000                      !Total samples in .wav file
  baud=11025.d0/4096.d0              !Keying rate
  sps=12000.d0/baud                  !Samples per symbol, at fsample=12000 Hz
  nsym=126                           !Number of channel symbols
  h=default_header(12000,npts)
  dfsig=2000.0/nsigs                 !Freq spacing between sigs in file (Hz)

  do ifile=1,nfiles                  !Loop over requested number of files
     write(fname,1002) ifile         !Output filename
1002 format('000000_',i4.4)
     open(10,file=fname//'.wav',access='stream',status='unknown')

     xnoise=0.
     cdat=0.
!     if(snrdb.lt.90) then
        do i=1,npts
           xnoise(i)=gran()          !Generate gaussian noise
        enddo
!     endif

     do isig=1,nsigs                        !Generate requested number of sigs
        call random_number(rr1)
!        f0=5.0+2690.0*rr1
        f0=1500.0+5.0*rr1
        xdtisig=xdt+gran()
        call random_number(rr1)
        snrdb=rr1*(-25.0)-1.0
        xsnr=snrdb
        if(snrdb.eq.0.0) xsnr=-19 - isig

     call random_number(rr1)
     np1=nint(rr1*ncalls)
     call1=callc3(np1)

     call random_number(rr1)
     np2=nint(rr1*ncalls)
     call2=callc3(np2)
     grid2=gridc3(np2)

     call random_number(rr1)
     np3=nint(rr1*(MAXRPT+60)) ! 60 is for CQ ,73 and call1 call2 grid messages
     if(np3.eq.0) rest=rpt(1)
     if(np3.gt.0 .and. np3.le.60) rest=rpt(np3)
     if(np3.gt.60 .and. np3.le.80) rest=grid2
     if(np3.gt.80 .and. np3.le.86) rest=r73(1)
     if(np3.gt.86 .and. np3.le.93) rest=r73(2)
     if(np3.gt.93 .and. np3.le.100) rest=r73(3)
     if(np3.gt.100) then
        call1='CQ'
        rest=grid2
     endif
 
     msg=trim(call1)//' '//trim(call2)//' '//trim(rest)
 
        call packmsg(msg,dgen,itype)        !Pack message into 12 six-bit bytes
        call rs_encode(dgen,sent)           !Encode using RS(63,12)
        call interleave63(sent,1)           !Interleave channel symbols
        call graycode65(sent,63,1)          !Apply Gray code

        k=0
        do j=1,nsym                         !Insert sync and data into itone()
           if(.not.prc(j)) then
              k=k+1
              itone(j)=sent(k)+2
           else
              itone(j)=0
           endif
        enddo

        bandwidth_ratio=2500.0/6000.0
        sig=sqrt(2*bandwidth_ratio)*10.0**(0.05*xsnr)
        if(xsnr.gt.90.0) sig=1.0
        write(*,1020) ifile,isig,f0,csubmode,xsnr,xdtisig,fspread,msg
1020    format(i4,i4,f10.3,2x,a1,2x,f5.1,f6.2,f5.1,1x,a22)

        phi=0.d0
        dphi=0.d0
        k=12000 + xdtisig*12000                 !Start audio at t = xdt + 1.0 s
        isym0=-99
        do i=1,npts                         !Add this signal into cdat()
           isym=floor(i/sps)+1
           if(isym.gt.nsym) exit
           if(isym.ne.isym0) then
              freq=f0 + itone(isym)*baud*mode65
              dphi=twopi*freq*dt
              isym0=isym
           endif
           phi=phi + dphi
           if(phi.gt.twopi) phi=phi-twopi
           xphi=phi
           z=cmplx(cos(xphi),sin(xphi))
           k=k+1
           if(k.ge.1) cdat(k)=cdat(k) + sig*z
        enddo
     enddo

     if(fspread.ne.0) then                  !Apply specified Doppler spread
        df=12000.0/nfft
        twopi=8*atan(1.0)
        cspread(0)=1.0
        cspread(NH)=0.

        do i=1,NH
           f=i*df
           x=f/fspread
           z=0.
           a=0.
           if(x.lt.50.0) then
              a=sqrt(exp(-x*x))
              call random_number(r1)
              phi1=twopi*r1
              z=a*cmplx(cos(phi1),sin(phi1))
           endif
           cspread(i)=z
           z=0.
           if(x.lt.50.0) then
              call random_number(r2)
              phi2=twopi*r2
              z=a*cmplx(cos(phi2),sin(phi2))
           endif
           cspread(NFFT-i)=z
        enddo

        do i=0,NFFT-1
           f=i*df
           if(i.gt.NH) f=(i-nfft)*df
           s=real(cspread(i))**2 + aimag(cspread(i))**2
!          write(13,3000) i,f,s,cspread(i)
!3000      format(i5,f10.3,3f12.6)
        enddo
!        s=real(cspread(0))**2 + aimag(cspread(0))**2
!        write(13,3000) 1024,0.0,s,cspread(0)

        call four2a(cspread,NFFT,1,1,1)             !Transform to time domain

        sum=0.
        do i=0,NFFT-1
           p=real(cspread(i))**2 + aimag(cspread(i))**2
           sum=sum+p
        enddo
        avep=sum/NFFT
        fac=sqrt(1.0/avep)
        cspread=fac*cspread                   !Normalize to constant avg power
        cdat=cspread(1:npts)*cdat             !Apply Rayleigh fading

!        do i=0,NFFT-1
!           p=real(cspread(i))**2 + aimag(cspread(i))**2
!           write(14,3010) i,p,cspread(i)
!3010       format(i8,3f12.6)
!        enddo

     endif

     dat=aimag(cdat) + xnoise                 !Add the generated noise
     fac=32767.0/nsigs
     if(snrdb.ge.90.0) iwave(1:npts)=nint(fac*dat(1:npts))
     if(snrdb.lt.90.0) iwave(1:npts)=nint(rms*dat(1:npts))
     write(10) h,iwave(1:npts)                !Save the .wav file
     close(10)
  enddo

999 end program jt65sim
