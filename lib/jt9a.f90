subroutine jt9a()
  use, intrinsic :: iso_c_binding, only: c_f_pointer
  use prog_args
!  use timer_module, only: timer
!  use timer_impl, only: init_timer !, limtrace
  use jt65_mod6
  use ft8_mod1, only : dd8
  use ft4_mod1, only : dd4
  include 'jt9com.f90'

! These routines connect the shared memory region to the decoder.
  interface
     function address_jtdxjt9()
       use, intrinsic :: iso_c_binding, only: c_ptr
       type(c_ptr) :: address_jtdxjt9
     end function address_jtdxjt9
  end interface

  integer*1 attach_jtdxjt9
  integer size_jtdxjt9
! Multiple instances:
  character*80 mykey
  type(dec_data), pointer, volatile :: shared_data !also makes target volatile
  type(params_block) :: local_params
  logical fileExists

! Multiple instances:
  i0 = len(trim(shm_key))

!  call init_timer (trim(data_dir)//'/timer.out')
!  open(23,file=trim(data_dir)//'/CALL3.TXT',status='unknown')

!  limtrace=-1                            !Disable all calls to timer()

! Multiple instances: set the shared memory key before attaching
  mykey=trim(repeat(shm_key,1))
  i0 = len(mykey)
  i0=setkey_jtdxjt9(trim(mykey))

  i1=attach_jtdxjt9()

10 inquire(file=trim(temp_dir)//'/.lock',exist=fileExists)
  if(fileExists) then
!     call sleep_msec(100)
     call sleep_msec(10)
     go to 10
  endif


  inquire(file=trim(temp_dir)//'/.quit',exist=fileExists)
  if(fileExists) then
     i1=detach_jtdxjt9()
     go to 999
  endif
  if(i1.eq.999999) stop                  !Silence compiler warning

  nbytes=size_jtdxjt9()
  if(nbytes.le.0) then
     print*,'jt9a: Shared memory mem_jtdxjt9 does not exist.'
     print*,"Must start 'jtdxjt9 -s <thekey>' from within WSJT-X."
     go to 999
  endif
  call c_f_pointer(address_jtdxjt9(),shared_data)
  local_params=shared_data%params !save a copy because jtdx.exe carries on accessing
  call flush(6)

  nnmode=local_params%nmode

  if(local_params%nmode.eq.8) then; npts1=180000
  else if(local_params%nmode.eq.4) then; npts1=73728
  else; npts1=NPTS
  endif

  if(local_params%ndiskdat) then
    if(local_params%nmode.eq.8) then ! nblocks values shall match to ihsym/m_hsymStop in mainwindow
      if(local_params%nswl) then; nblocks=51
      else if(local_params%learlystart) then; nblocks=48
      else; nblocks=49
      endif
      local_params%nzhsym=nblocks; nlastsam=nblocks*3456
      dd(1:nlastsam)=shared_data%id2(1:nlastsam)
      dd(nlastsam+1:npts1)=0.
    else
     dd(1:npts1)=shared_data%id2(1:npts1)
    endif
  else
     if(local_params%nmode.eq.8) then
        rms=sum(abs(shared_data%dd2(1:10)))+sum(abs(shared_data%dd2(76001:76010)))+ &
            sum(abs(shared_data%dd2(151670:151680)))
     else if(local_params%nmode.eq.4) then
        rms=sum(abs(shared_data%dd2(1:10)))+sum(abs(shared_data%dd2(30001:30010)))+ &
            sum(abs(shared_data%dd2(60470:60480)))
     else
        rms=sum(abs(shared_data%dd2(1:10)))+sum(abs(shared_data%dd2(300000:300010)))+ &
            sum(abs(shared_data%dd2(623991:624000)))
     endif
     if(rms.gt.0.001) then
        dd(1:npts1)=shared_data%dd2(1:npts1)
!print *,'win7',rms
     else ! workaround for zero data values of dd2 array under WinXP
        dd(1:npts1)=shared_data%id2(1:npts1)
!print *, 'winxp',rms
     endif
  endif

  if(local_params%nmode.eq.8) then; dd8(1:npts1)=dd(1:npts1)
  else if(local_params%nmode.eq.4) then; dd4(1:npts1)=dd(1:npts1)
  endif

!  call timer('decoder ',0)
  call multimode_decoder(local_params)
!  call timer('decoder ',1)

100 inquire(file=trim(temp_dir)//'/.lock',exist=fileExists)
  if(fileExists) go to 10
  call sleep_msec(100)
  go to 100

999 continue
!    call timer('decoder ',101)

  return
end subroutine jt9a
