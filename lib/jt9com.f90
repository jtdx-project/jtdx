  use, intrinsic :: iso_c_binding, only: c_int, c_short, c_float, c_char, c_bool
  include 'constants.f90'

  !
  ! these structures must be kept in sync with ../commons.h
  !
  type, bind(C) :: params_block
!     character(kind=c_char) :: datetime(20)
     character(kind=c_char) :: mycall(12)
     character(kind=c_char) :: mybcall(12)
     character(kind=c_char) :: hiscall(12)
     character(kind=c_char) :: hisbcall(12)
!     character(kind=c_char) :: mygrid(6)
     character(kind=c_char) :: hisgrid(6)
     integer(c_int) :: listutc(10)
     integer(c_int) :: napwid
     integer(c_int) :: nQSOProgress
     integer(c_int) :: nftx
     integer(c_int) :: nutc
     integer(c_int) :: ntrperiod
     integer(c_int) :: nfqso
     integer(c_int) :: npts8
     integer(c_int) :: nfa
     integer(c_int) :: nfsplit
     integer(c_int) :: nfb
     integer(c_int) :: ntol
     integer(c_int) :: kin
     integer(c_int) :: nzhsym
     integer(c_int) :: ndepth
     integer(c_int) :: ncandthin
     integer(c_int) :: ndtcenter
     integer(c_int) :: nft8cycles
     integer(c_int) :: nft8swlcycles
     integer(c_int) :: ntxmode
     integer(c_int) :: nmode
     integer(c_int) :: nlist
     integer(c_int) :: nranera
     integer(c_int) :: ntrials10
     integer(c_int) :: ntrialsrxf10
     integer(c_int) :: naggressive
     integer(c_int) :: nharmonicsdepth
     integer(c_int) :: ntopfreq65
     integer(c_int) :: nprepass
     integer(c_int) :: nsdecatt
     integer(c_int) :: nlasttx
     integer(c_int) :: ndelay
     integer(c_int) :: nmt
     integer(c_int) :: nft8rxfsens
     integer(c_int) :: nft4depth
     integer(c_int) :: nsecbandchanged
     logical(c_bool) :: ndiskdat
     logical(c_bool) :: newdat
     logical(c_bool) :: nagain
     logical(c_bool) :: nagainfil
     logical(c_bool) :: nswl
     logical(c_bool) :: nfilter
     logical(c_bool) :: nstophint
     logical(c_bool) :: nagcc
     logical(c_bool) :: nhint
     logical(c_bool) :: fmaskact
     logical(c_bool) :: showharmonics
     logical(c_bool) :: lft8lowth
     logical(c_bool) :: lft8subpass
     logical(c_bool) :: ltxing
     logical(c_bool) :: lhidetest
     logical(c_bool) :: lhidetelemetry
     logical(c_bool) :: lhideft8dupes
     logical(c_bool) :: lhound
     logical(c_bool) :: lhidehash
     logical(c_bool) :: lcommonft8b
     logical(c_bool) :: lmycallstd
     logical(c_bool) :: lhiscallstd
     logical(c_bool) :: lapmyc
     logical(c_bool) :: lmodechanged
     logical(c_bool) :: lbandchanged
     logical(c_bool) :: lenabledxcsearch
     logical(c_bool) :: lwidedxcsearch
     logical(c_bool) :: lmultinst
     logical(c_bool) :: lskiptx1
     logical(c_bool) :: lforcesync
     logical(c_bool) :: learlystart
 end type params_block

  type, bind(C) :: dec_data
     real(c_float) :: ss(184,NSMAX)
     real(c_float) :: savg(NSMAX)
     integer(c_short) :: id2(NMAX)
     real(c_float) :: dd2(NMAX)
     type(params_block) :: params
  end type dec_data
  ! for unknown reason values of the variables at beginning of dec_data list are being
  ! not updated while Decode button is pushed manually, for decoding again keep variables at end
  ! of the list
