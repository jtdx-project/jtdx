module hashing
  interface
     integer(c_int32_t) function nhash (key, length, initval) bind(C, name="nhash")
       use iso_c_binding, only: c_ptr, c_size_t, c_int32_t
       type(c_ptr), intent(in), value :: key
       integer(c_size_t), intent(in), value :: length
       integer(c_int32_t), intent(in), value :: initval
     end function nhash
  end interface
end module hashing
