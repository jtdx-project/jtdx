module iso_c_utilities

  use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_f_pointer, c_associated
  implicit none

  public :: c_to_f_string, c_f_dyn_string

  private

  character(c_char), dimension(1), save, target :: dummy_string = "?"

  interface        ! strlen is a standard C function from <string.h>
     ! int strlen(char *string)
     function strlen (string) result (len) bind (c, name="strlen")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       implicit none
       type(c_ptr), value :: string
       integer(kind=c_size_t) :: len
     end function strlen

     ! void free(void * p)
     subroutine c_free (p) bind (c, name="free")
       use, intrinsic :: iso_c_binding, only: c_ptr
       implicit none
       type(c_ptr), value :: p
     end subroutine c_free
  end interface

contains

  !
  ! Cast C string pointer to Fortran string pointer
  !
  ! Warning! - C data must outlive result scope
  !
  function c_to_f_string (c_str) result (f_str)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_char
    implicit none
    type(c_ptr), intent(in) :: c_str
    character(kind=c_char, len=:), pointer :: f_str
    character(kind=c_char), pointer :: arr(:)
    interface        ! strlen is a standard C function from <string.h>
       ! int strlen(char *string)
       function strlen (string) result (len) bind (c, name="strlen")
         use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
         implicit none
         type(c_ptr), value :: string
         integer(kind=c_size_t) :: len
       end function strlen
    end interface
    call c_f_pointer (c_str, arr, [strlen (c_str)])
    call get_scalar_pointer (size (arr), arr, f_str)
  end function c_to_f_string

  subroutine get_scalar_pointer (scalar_len, scalar, fptr)
    ! Convert a null-terminated C string into a Fortran character pointer
    use, intrinsic :: iso_c_binding, only: c_char
    integer, intent(in) :: scalar_len
    character(kind=c_char, len=scalar_len), intent(in), target :: scalar(1)
    character(kind=c_char, len=:), pointer :: fptr
    fptr => scalar(1)
  end subroutine get_scalar_pointer

  function c_f_dyn_string (cptr) result (fstr)
    ! Convert a null-terminated malloc'ed C string into a Fortran character array
    type(c_ptr), intent(in) :: cptr ! The C address
    character(kind=c_char), allocatable :: fstr(:)
    character(kind=c_char), pointer :: fptr(:)
    interface        ! strlen is a standard C function from <string.h>
       ! void free(void * p)
       subroutine c_free (p) bind (c, name="free")
         use, intrinsic :: iso_c_binding, only: c_ptr
         implicit none
         type(c_ptr), value :: p
       end subroutine c_free
    end interface
    if (c_associated (cptr)) then
       call c_f_pointer (fptr=fptr, cptr=cptr, shape=[strlen(cptr)])
    else
       ! To avoid segfaults, associate FPTR with a dummy target:
       fptr => dummy_string
    end if
    fstr = fptr
    call c_free (cptr)
  end function c_f_dyn_string

end module
