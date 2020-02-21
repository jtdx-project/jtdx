module timer_impl
  !$ use omp_lib
  use :: iso_c_binding, only: c_ptr
  use timer_module, only: timer_callback
  implicit none

  public :: init_timer, fini_timer
  integer, public :: limtrace=0

  private

  integer, parameter :: MAXCALL=100
  integer :: lu=6
  real :: dut
  integer :: i,nmax=0,ncall(MAXCALL),nlevel(MAXCALL),nparent(MAXCALL)
  character(len=8) :: name(MAXCALL),space='        '
  logical :: on(MAXCALL)
  real :: total,sum,sumf,ut(MAXCALL),ut0(MAXCALL)
  !$ integer :: j,l,m,ntid(MAXCALL)

  !
  ! C interoperable callback setup
  !
  public :: C_init_timer
  abstract interface
     subroutine C_timer_callback (context, dname, k)
       use, intrinsic :: iso_c_binding, only: c_ptr
       implicit none
       type(c_ptr), intent(in) :: context
       character(len=8), intent(in) :: dname
       integer, intent(in) :: k
     end subroutine C_timer_callback
  end interface
  type(c_ptr), private :: the_context
  procedure(C_timer_callback), pointer, private :: the_C_callback

contains
  subroutine timer_callback_wrapper (dname, k)
    implicit none
    character(len=8), intent(in) :: dname
    integer, intent(in) :: k
    call the_C_callback (the_context, dname, k)
  end subroutine timer_callback_wrapper

  subroutine C_init_timer (context, callback) bind(C)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_f_procpointer
    use iso_c_utilities, only: c_to_f_string
    use timer_module, only: timer
    implicit none
    type(c_ptr), intent(in) :: context
    type(c_funptr), intent(in) :: callback
    the_context=context
    call c_f_procpointer (callback, the_C_callback)
    timer => timer_callback_wrapper
  end subroutine C_init_timer

  !
  ! default Fortran implementation which is thread safe using OpenMP
  !
  subroutine default_timer (dname, k)

    ! Times procedure number n between a call with k=0 (tstart) and with
    ! k=1 (tstop). Accumulates sums of these times in array ut (user time).
    ! Also traces all calls (for debugging purposes) if limtrace.gt.0
    !
    ! If this is used with OpenMP than the /timer_private/ common
    ! block must be copyed into each thread of a thread team by using
    ! the copyin() clause on the !$omp parallel directive that creates
    ! the team.

    implicit none

    character(len=8), intent(in) :: dname
    integer, intent(in) :: k

    real :: ut1,eps=0.000001
    integer :: n,ndiv,ntrace=0
    !$ integer :: tid
    character(len=8) :: tname
    include 'timer_common.inc'

    !$omp critical(timer)
    if(limtrace.lt.0) go to 999
    if(k.gt.1) go to 40                        !Check for "all done" (k>1)
    onlevel(0)=0

    !$ tid=omp_get_thread_num()
    do n=1,nmax                                !Check for existing name/parent[/thread]
       if(name(n).eq.dname &
                                !$ .and.ntid(n).eq.tid &
            ) then
          if (on(n)) then
             if (nparent(n).eq.onlevel(level-1)) goto 20
          else
             if (nparent(n).eq.onlevel(level)) goto 20
          end if
       end if
    enddo

    nmax=nmax+1                                !This is a new one
    n=nmax
    !$ ntid(n)=tid
    ncall(n)=0
    on(n)=.false.
    ut(n)=eps
    name(n)=dname

20  if(k.eq.0) then                                !Get start times (k=0)
       if(on(n)) then
          print*,'Error in timer: ',dname,' already on.'
       end if
       level=level+1 !Increment the level
       on(n)=.true.
       !     call system_clock(icount,irate)
       !     ut0(n)=float(icount)/irate
       !     call cpu_time(ut0(n))
       ut0(n)=secnds(0.0)

       ncall(n)=ncall(n)+1
       if(ncall(n).gt.1.and.nlevel(n).ne.level) then
          !recursion is happening
          !
          !TODO: somehow need to account for this deeper call at the
          !shallowest instance in the call chain and this needs to be
          !done without incrementing anything here other than counters
          !and timers
          !
          nlevel(n)=-1
       else
          nlevel(n)=level
       endif
       nparent(n)=onlevel(level-1)
       onlevel(level)=n

    else if(k.eq.1) then        !Get stop times and accumulate sums. (k=1)
       if(on(n)) then
          on(n)=.false.
          !        call system_clock(icount,irate)
          !        ut1=float(icount)/irate
          !        call cpu_time(ut1)
          ut1=secnds(0.0)

          ut(n)=ut(n)+ut1-ut0(n)
       endif
       level=level-1
    endif

    ntrace=ntrace+1
    if(ntrace.lt.limtrace) write(lu,1020) ntrace,tname,k,level,nparent(n)
1020 format(i8,': ',a8,3i5)
    go to 998

    ! Write out the timer statistics

40  write(lu,1040)
1040 format(/' Name                 Time  Frac     dTime',       &
         ' dFrac    Calls'/58('-'))

    !$ !walk backwards through the database rolling up thread data by call chain
    !$ do i=nmax,1,-1
    !$    do j=1,i-1
    !$       l=j
    !$       m=i
    !$       do while (name(l).eq.name(m))
    !$          l=nparent(l)
    !$          m=nparent(m)
    !$          if (l.eq.0.or.m.eq.0) exit
    !$       end do
    !$       if (l.eq.0.and.m.eq.0) then
    !$          !same call chain so roll up data
    !$          ncall(j)=ncall(j)+ncall(i)
    !$          ut(j)=ut(j)+ut(i)
    !$          do n=1,nmax
    !$            if (nparent(n).eq.i) nparent(n)=j
    !$          end do
    !$          name(i)=space
    !$          exit
    !$       end if
    !$    end do
    !$ end do

    if(k.gt.100) then
       ndiv=k-100
       do i=1,nmax
          ncall(i)=ncall(i)/ndiv
          ut(i)=ut(i)/ndiv
       enddo
    endif

    total=ut(1)
    sum=0.
    sumf=0.
    call print_root(1)
    write(lu,1070) sum,sumf
1070 format(58('-')/32x,f10.3,f6.2)
    nmax=0
    eps=0.000001
    ntrace=0
    level=0
    onlevel(0)=0

998 flush(lu)

999 continue

    !$omp end critical(timer)
    return
  end subroutine default_timer

  recursive subroutine print_root(i)
    implicit none
    integer, intent(in) :: i
    character(len=16) :: sname
    real :: dutf, utf
    integer :: j, kk

    if (i.le.nmax) then
       if (name(i).ne.space) then
          dut=ut(i)
          do j=i,nmax
             if (name(j).ne.space.and.nparent(j).eq.i) dut=dut-ut(j)
          enddo
          if(dut.lt.0.0) dut=0.0
          utf=ut(i)/total
          dutf=dut/total
          sum=sum+dut
          sumf=sumf+dutf
          kk=nlevel(i)
          sname=space(1:kk)//name(i)//space(1:8-kk)
          write(lu,2000) sname,ut(i),utf,dut,dutf,ncall(i)
2000      format(a16,2(f10.3,f6.2),i9)
          do j=i,nmax
             if(nparent(j).eq.i) call print_root(j)
          enddo
       end if
    end if
    return
  end subroutine print_root

  subroutine init_timer (filename)
    use, intrinsic :: iso_c_binding, only: c_char
    use timer_module, only: timer
    implicit none
    character(len=*), optional, intent(in) :: filename
    include 'timer_common.inc'
    data level/0/, onlevel/11 * 0/
    if (present (filename)) then
       open (newunit=lu, file=filename, status='unknown')
    else
       open (newunit=lu, file='timer.out', status='unknown')
    end if
    timer => default_timer
  end subroutine init_timer

  subroutine fini_timer ()
    use timer_module, only: timer, null_timer
    implicit none
    timer => null_timer
    close (lu)
  end subroutine fini_timer

end module timer_impl
