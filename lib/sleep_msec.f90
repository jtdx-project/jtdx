subroutine sleep_msec(n)
  call usleep(n*1000)
  return
end subroutine sleep_msec
