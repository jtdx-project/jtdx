program fixwav

  use wavhdr
  parameter (NBANDS=23,NMODES=11)
  parameter(NMAX=120*12000)
  type(hdr) h
  integer*2 id2(NMAX),id(4)
  character*8 mode,mode0
  character*6 band
  character*12 arg
  character*80 infile
  character*1 c
  logical ok

  nargs=iargc()
  if(nargs.ne.1 .and. nargs.ne.5) then
     print*,'Usage: fixwav [fMHz mode submode TRperiod] infile'
     go to 999
  endif
  call getarg(1,infile)
  if(nargs.eq.1) go to 10
  read(infile,*) fMHz
  call getarg(2,mode0)
  call getarg(3,arg)
  nsubmode0=-1
  do i=1,8
     if(arg(1:1).eq.char(ichar('A')-1+i)) nsubmode0=i
  enddo
  if(nsubmode0.lt.0) read(arg,*) nsubmode0
  call getarg(4,arg)
  read(arg,*) ntrperiod0
  call getarg(5,infile)

10 open(10,file=infile,status='old',access='stream')
  read(10) h
  npts=h%ndata/2
  nfsample=h%nsamrate
  read(10) id2(1:npts)
  write(*,1002) h%nchan2,h%nbitsam2,h%nsamrate,npts
1002 format('Channels:',i2,'   Bits/sample:',i3,'   Sample rate:',i6,   &
          '   Npts:',i8)

  call get_wsjtx_wav_params(id2,band,mode,nsubmode,ntrperiod,ok)
  if(nfsample.ne.11025 .and. nfsample.ne.12000) ok=.false.
  if(ok) write(*,1010) band,ntrperiod,mode,char(ichar('A')-1+id2(3))
1010 format('Band: ',a6,'  T/R period:',i4,'   Mode: ',a8,1x,a1)

  if(.not.ok) write(*,'(a)') 'File has no valid WSJT-X params.'
  if(ok .and. nargs.eq.1) go to 999

  write(*,'(a)',advance='no') 'Do you want to add or change them? (Y/N): '
  read*,c
  if(c.ne.'y' .and. c.ne.'Y') go to 999

  print*,fMHz,mode0,nsubmode0,ntrperiod0
  call set_wsjtx_wav_params(fMHz,mode0,nsubmode0,ntrperiod0,id2)
  band=""
  mode=""
  nsubmode=0
  ntrperiod=0
  call get_wsjtx_wav_params(id2,band,mode,nsubmode,ntrperiod,ok)
  write(*,1010) band,ntrperiod,mode,char(ichar('A')-1+id2(3))
  rewind 10
  write(10) h,id2(1:npts)

999 end program fixwav
