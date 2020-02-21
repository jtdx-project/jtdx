! This source code file was last time modified by Igor UA3DJY on April 13th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

module jt9_mod2

  integer, parameter :: MAXCANDRXF=5, MAXCAND=200

  type candrxf
     real ccfred
     integer i
  end type candrxf
  type(candrxf) carxf(MAXCANDRXF)

  type candidate
     real ccfred
     integer i
  end type candidate
  type(candidate) ca(MAXCAND)
  
end module jt9_mod2