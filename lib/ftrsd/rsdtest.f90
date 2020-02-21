program rsdtest

  real s3(64,63)
  character msg*22,arg*12
  integer param(0:7)

  nargs=iargc()
  if(nargs.ne.2) then
     print*,'Usage: rsdtest ntrials nfiles'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) ntrials
  call getarg(2,arg)
  read(arg,*) nfiles

  open(10,file='s3_1000.bin',access='stream', status='old')
  open(22,file='kvasd.dat',access='direct',recl=1024,status='unknown')

  nadd=1
  ifile0=0
  if(nfiles.lt.0) then
     ifile0=-nfiles
     nfiles=99999
  endif

  do ifile=1,nfiles
     read(10,end=999) s3
     if(ifile.lt.ifile0) cycle
     call extract2(s3,nadd,ntrials,param,msg)
     if(ifile.eq.ifile0) exit
  enddo

999 end program rsdtest
