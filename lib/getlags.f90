subroutine getlags(nsps8,lag0,lag1,lag2)
  if(nsps8.eq.864) then
     lag1=39
     lag2=291
     lag0=123
  else if(nsps8.eq.1920) then
     lag1=70
     lag2=184
     lag0=108
  else if(nsps8.eq.5120) then
     lag1=84
     lag2=129
     lag0=99
  else if(nsps8.eq.10368) then
     lag1=91
     lag2=112
     lag0=98
  else if(nsps8.eq.31500) then
     lag1=93
     lag2=102
     lag0=96
  else 
     stop 'Error in getlags'
  endif

  return
end subroutine getlags
