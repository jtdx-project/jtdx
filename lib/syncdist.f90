ip=maxloc(s81)
if(icos7(k-1).eq.(ip(1)-1)) then
  nsmax(1)=nsmax(1)+1
else
  s81(ip(1)-1)=0.; ip=maxloc(s81)
  if(icos7(k-1).eq.(ip(1)-1)) then
    nsmax(2)=nsmax(2)+1
  else
    s81(ip(1)-1)=0.; ip=maxloc(s81)
    if(icos7(k-1).eq.(ip(1)-1)) then
      nsmax(3)=nsmax(3)+1
    else
      s81(ip(1)-1)=0.; ip=maxloc(s81)
      if(icos7(k-1).eq.(ip(1)-1)) then
        nsmax(4)=nsmax(4)+1
      else
        s81(ip(1)-1)=0.; ip=maxloc(s81)
        if(icos7(k-1).eq.(ip(1)-1)) then
          nsmax(5)=nsmax(5)+1
        else
          s81(ip(1)-1)=0.; ip=maxloc(s81)
          if(icos7(k-1).eq.(ip(1)-1)) then
            nsmax(6)=nsmax(6)+1
          else
            s81(ip(1)-1)=0.; ip=maxloc(s81)
            if(icos7(k-1).eq.(ip(1)-1)) then
              nsmax(7)=nsmax(7)+1
            else
              s81(ip(1)-1)=0.; ip=maxloc(s81)
              if(icos7(k-1).eq.(ip(1)-1)) nsmax(8)=nsmax(8)+1
            endif
          endif
        endif
      endif
    endif
  endif
endif
