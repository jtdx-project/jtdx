real function db(x)
  db=-99.0
  if(x.gt.1.259e-10) db=10.0*log10(x)
  return
end function db
