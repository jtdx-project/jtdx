! This source code file was last time modified by Igor UA3DJY on February 21st, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

module jt65_mod11
 ! MAXINTCAND max sum number of candidates for all decoding passes
  integer, parameter :: MAXCAND=300,MAXINTCAND=500

  type candidate
     real freq
     real dt
     real sync
  end type candidate
  type(candidate) ca(MAXCAND)
  
  type dt_check
    real cand_dtbest
    logical cand_decoded
  end type dt_check
  type(dt_check) dtchk(MAXINTCAND)
  
end module jt65_mod11