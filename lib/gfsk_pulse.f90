real function gfsk_pulse(b,t)
  pi=4.*atan(1.0)
  c=pi*sqrt(2.0/log(2.0))
  gfsk_pulse=0.5*(erf(c*b*(t+0.5))-erf(c*b*(t-0.5)))
  return
end function gfsk_pulse
