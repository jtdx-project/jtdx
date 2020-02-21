module timer_c_wrapper
  use :: iso_c_binding, only: c_ptr
  use timer_module, only: timer, null_timer
  implicit none

  !
  ! C interoperable callback setup
  !
  abstract interface
     subroutine c_timer_callback (context, dname, k)
       use, intrinsic :: iso_c_binding, only: c_ptr, c_char
       implicit none
       type(c_ptr), value, intent(in) :: context
       character(c_char), intent(in) :: dname(*)
       integer, intent(in), value :: k
     end subroutine c_timer_callback
  end interface

  public :: init, fini

  private

  !
  ! the following are singleton items which assumes that any timer
  ! implementation should only assume one global instance, probably a
  ! struct or class object whose address is stored the context below
  !
  type(c_ptr), private :: the_context
  procedure(C_timer_callback), pointer, private :: the_callback

contains
  subroutine timer_callback_wrapper (dname, k)
    use, intrinsic :: iso_c_binding, only: c_null_char
    implicit none
    character(len=8), intent(in) :: dname
    integer, intent(in) :: k
    call the_callback (the_context, trim (dname) // c_null_char, k)
  end subroutine timer_callback_wrapper

  subroutine init (context, callback)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_f_procpointer
    use iso_c_utilities, only: c_to_f_string
    use timer_module, only: timer
    implicit none
    type(c_ptr), value, intent(in) :: context
    type(c_funptr), value, intent(in) :: callback
    the_context=context
    call c_f_procpointer (callback, the_callback)
    timer => timer_callback_wrapper
  end subroutine init

  subroutine fini ()
    implicit none
    timer => null_timer
  end subroutine fini

end module timer_c_wrapper
