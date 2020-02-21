subroutine my_hash(mycall)

  use packjt77
  character*(*) mycall
  character*13 c13

  c13=mycall//'          '
  call save_hash_call(c13,n10,n12,n22)
  
  return
end subroutine my_hash
