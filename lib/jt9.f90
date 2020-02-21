! This source code file was last time modified by Igor UA3DJY on September 30th, 2018
! All changes are shown in the patch file coming together with the full JTDX source code.

program jt9

! Decoder for JT9. Can run as the back end of JTDX only, with data placed in a shared memory region.

  use options
  use prog_args
  use, intrinsic :: iso_c_binding
  use FFTW3
!  use timer_module, only: timer
!  use timer_impl, only: init_timer, fini_timer
!  use readwav

  include 'jt9com.f90'

  integer(C_INT) iret
!  type(wav_header) wav
  character c
  character(len=500) optarg
  character wisfile*80
  integer :: arglen,stat,offset,remain,mode=0,flow=200,fsplit=2700,          &
       fhigh=4000,nrxfreq=1500,ntrperiod=1,ndepth=1
  logical :: read_files = .true., tx9 = .false., display_help = .false.
  type (option) :: long_options(22) = [ &
    option ('help', .false., 'h', 'Display this help message', ''),          &
    option ('shmem',.true.,'s','Use shared memory for sample data','KEY'),   &
    option ('tr-period', .true., 'p', 'Tx/Rx period, default MINUTES=1',     &
        'MINUTES'),                                                          &
    option ('executable-path', .true., 'e',                                  &
        'Location of subordinate executables (KVASD) default PATH="."',      &
        'PATH'),                                                             &
    option ('data-path', .true., 'a',                                        &
        'Location of writeable data files, default PATH="."', 'PATH'),       &
    option ('temp-path', .true., 't',                                        &
        'Temporary files path, default PATH="."', 'PATH'),                   &
    option ('share-path', .true., 'r',                                       &
        'Share files path, default PATH="."', 'PATH'),                       &
    option ('lowest', .true., 'L',                                           &
        'Lowest frequency decoded (JT65), default HERTZ=200', 'HERTZ'),      &
    option ('highest', .true., 'H',                                          &
        'Highest frequency decoded, default HERTZ=4007', 'HERTZ'),           &
    option ('split', .true., 'S',                                            &
        'Lowest JT9 frequency decoded, default HERTZ=2700', 'HERTZ'),        &
    option ('rx-frequency', .true., 'f',                                     &
        'Receive frequency offset, default HERTZ=1500', 'HERTZ'),            &
    option ('patience', .true., 'w',                                         &
        'FFTW3 planing patience (0-4), default PATIENCE=1', 'PATIENCE'),     &
    option ('fft-threads', .true., 'm',                                      &
        'Number of threads to process large FFTs, default THREADS=1',        &
        'THREADS'),                                                          &
    option ('jt65', .false., '6', 'JT65 mode', ''),                          &
    option ('jt9', .false., '9', 'JT9 mode', ''),                            &
    option ('sub-mode', .true., 'b', 'Sub mode, default SUBMODE=A', 'A'),    &
    option ('depth', .true., 'd',                                            &
        'JT9 decoding depth (1-3), default DEPTH=1', 'DEPTH'),               &
    option ('tx-jt9', .false., 'T', 'Tx mode is JT9', ''),                   &
    option ('my-call', .true., 'c', 'my callsign', 'CALL'),                  &
    option ('my-grid', .true., 'G', 'my grid locator', 'GRID'),              &
    option ('his-call', .true., 'x', 'his callsign', 'CALL'),                &
    option ('his-grid', .true., 'g', 'his grid locator', 'GRID') ]

  character(len=12) :: mycall, hiscall
  character(len=6) :: mygrid, hisgrid
  common/patience/npatience,nthreads
  data npatience/1/,nthreads/1/

  nsubmode = 0

  do
     call getopt('hs:e:a:b:r:m:p:d:f:w:t:964TL:S:H:c:G:x:g:',long_options,c,   &
          optarg,arglen,stat,offset,remain,.true.)
     if (stat .ne. 0) then
        exit
     end if
     select case (c)
        case ('h')
           display_help = .true.
        case ('s')
           read_files = .false.
           shm_key = optarg(:arglen)
        case ('e')
           exe_dir = optarg(:arglen)
        case ('a')
           data_dir = optarg(:arglen)
        case ('r')
           share_dir = optarg(:arglen)
        case ('b')
           nsubmode = ichar (optarg(:1)) - ichar ('A')
        case ('t')
           temp_dir = optarg(:arglen)
        case ('m')
           read (optarg(:arglen), *) nthreads
        case ('p')
           read (optarg(:arglen), *) ntrperiod
        case ('d')
           read (optarg(:arglen), *) ndepth
        case ('f')
           read (optarg(:arglen), *) nrxfreq
        case ('L')
           read (optarg(:arglen), *) flow
        case ('S')
           read (optarg(:arglen), *) fsplit
        case ('H')
           read (optarg(:arglen), *) fhigh
        case ('4')
           mode = 4
        case ('6')
           if (mode.lt.65) mode = mode + 65
        case ('9')
           if (mode.lt.9.or.mode.eq.65) mode = mode + 9
        case ('T')
           tx9 = .true.
        case ('w')
           read (optarg(:arglen), *) npatience
        case ('c')
           read (optarg(:arglen), *) mycall
        case ('G')
           read (optarg(:arglen), *) mygrid
        case ('x')
           read (optarg(:arglen), *) hiscall
        case ('g')
           read (optarg(:arglen), *) hisgrid
     end select
  end do

  if (display_help .or. stat .lt. 0                      &
       .or. (.not. read_files .and. remain .gt. 0)       &
       .or. (read_files .and. remain .lt. 1)) then
!
!     print *, 'Usage: jt9 [OPTIONS] file1 [file2 ...]'
!     print *, '       Reads data from *.wav files.'
!     print *, ''
!     print *, '       jt9 -s <key> [-w patience] [-m threads] [-e path] [-a path] [-t path]'
!     print *, '       Gets data from shared memory region with key==<key>'
!     print *, ''
!     print *, 'OPTIONS:'
!     print *, ''
!     do i = 1, size (long_options)
!       call long_options(i) % print (6)
!     end do
     go to 999
  endif

  iret=fftwf_init_threads()            !Initialize FFTW threading 

! Default to 1 thread, but use nthreads for the big ones
  call fftwf_plan_with_nthreads(1)

! Import FFTW wisdom, if available
  wisfile=trim(data_dir)//'/jt9_wisdom.dat'// C_NULL_CHAR
  iret=fftwf_import_wisdom_from_filename(wisfile)

  if (.not. read_files) call jt9a()          !We're running under control of WSJT-X

999 continue

! Save wisdom and free memory
  iret=fftwf_export_wisdom_to_filename(wisfile)
  call four2a(a,-1,1,1,1)
  call filbig(-1.,0,0.,0,0.,0.,0)        !used for FFT plans
  call fftwf_cleanup_threads()
  call fftwf_cleanup()
end program jt9
