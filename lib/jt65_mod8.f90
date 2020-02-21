! This source code file was last time modified by Igor UA3DJY on February 10th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

module jt65_mod8

  type dynhint_struct
    integer ninterval
    character*6 call1
    character*6 call2
    character*4 grid2
    integer nlastrx
    real freq
    real dt
  end type dynhint_struct
  type(dynhint_struct) dynhint(300)

  type dynhintcq_struct
    integer ninterval
    character*2 direction
    character*6 callsign
    character*4 grid
    real freq
    real dt
  end type dynhintcq_struct
  type(dynhintcq_struct) dynhintcq(150)
  
  type rxfreq_struct
    integer ninterval
    character*2 direction ! used to recognize CQ and directional CQ messages, 01 - CQ, 00 - other messages
    character*6 call2
    real freq
    real dt
  end type rxfreq_struct
  type(rxfreq_struct) rxfreq(20)
  
  type rxfreqchg_struct
    integer ninterval
    integer nfqso
    logical rxfchanged
  end type rxfreqchg_struct
  type(rxfreqchg_struct) rxfchg(1)
  
  integer ninterval
  real freq,maxsync2,maxsync2freq
  
  save

end module jt65_mod8