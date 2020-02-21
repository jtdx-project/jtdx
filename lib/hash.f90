subroutine hash(string,len,ihash)
  use iso_c_binding, only: c_loc,c_size_t
  use hashing
  parameter (MASK15=32767)
!  character*(*), target :: string
  character*1, target :: string
     i=nhash(c_loc(string),int(len,c_size_t),146)
     ihash=iand(i,MASK15)
  return
end subroutine hash
