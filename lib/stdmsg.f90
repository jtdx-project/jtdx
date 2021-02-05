function stdmsg(msg0)

  ! Returns .true. if msg0 a standard "JT-style" message
  
  ! i3.n3
  !  0.0   Free text
  !  0.1   DXpeditiion mode
  !  0.2   EU VHF Contest
  !  0.3   ARRL Field Day <=16 transmitters
  !  0.4   ARRL Field Day >16 transmitters
  !  0.5   telemetry
  !  0.6
  !  0.7
  !  1     Standard 77-bit structured message (optional /R)
  !  2     EU VHF Contest (optional /P)
  !  3     ARRL RTTY Contest
  !  4     Nonstandard calls

  use iso_c_binding, only: c_bool
  use packjt
  use packjt77

  character*37 msg0,msg1
  character*77 c77
  logical(c_bool) :: stdmsg

  msg1=msg0
  i3=-1
  n3=-1
  call pack77(msg1,i3,n3,c77,0)
  stdmsg=(i3.gt.0 .or. n3.gt.0)

!###
!  rewind 82
!  do i=1,nzhash
!     write(82,3082) i,nzhash,callsign(i),ihash10(i),ihash12(i),ihash22(i)
!3082 format(2i5,2x,a13,3i10)
!  enddo
!  flush(82)
!###
  
  return
end function stdmsg
