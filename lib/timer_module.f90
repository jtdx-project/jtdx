module timer_module
  implicit none

  abstract interface
     subroutine timer_callback (dname, k)
       character(len=8), intent(in) :: dname
       integer, intent(in) :: k
     end subroutine timer_callback
  end interface

  public :: null_timer
  procedure(timer_callback), pointer :: timer => null_timer

contains
  !
  ! default Fortran implementation which does nothing
  !
  subroutine null_timer (dname, k)
    implicit none
    character(len=8), intent(in) :: dname
    integer, intent(in) :: k
  end subroutine null_timer
end module timer_module
