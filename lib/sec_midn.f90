real function sec_midn()
  sec_midn=secnds(0.0)
  return
end function sec_midn

subroutine sleep_msec(n)

  call usleep(1000*n)

  return
end subroutine sleep_msec
