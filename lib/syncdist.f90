        if(icos7(k-1).eq.(ip(1)-1)) then
          snmax(1)=snmax(1)+1
        else
          s81(ip(1)-1)=0.; ip=maxloc(s81)
          if(icos7(k-1).eq.(ip(1)-1)) then
            snmax(2)=snmax(2)+1
          else
            s81(ip(1)-1)=0.; ip=maxloc(s81)
            if(icos7(k-1).eq.(ip(1)-1)) then
              snmax(3)=snmax(3)+1
            else
              s81(ip(1)-1)=0.; ip=maxloc(s81)
              if(icos7(k-1).eq.(ip(1)-1)) then
                snmax(4)=snmax(4)+1
              else
                s81(ip(1)-1)=0.; ip=maxloc(s81)
                if(icos7(k-1).eq.(ip(1)-1)) then
                  snmax(5)=snmax(5)+1
                else
                  s81(ip(1)-1)=0.; ip=maxloc(s81)
                  if(icos7(k-1).eq.(ip(1)-1)) then
                    snmax(6)=snmax(6)+1
                  else
                    s81(ip(1)-1)=0.; ip=maxloc(s81)
                    if(icos7(k-1).eq.(ip(1)-1)) then
                      snmax(7)=snmax(7)+1
                    else
                      s81(ip(1)-1)=0.; ip=maxloc(s81)
                      if(icos7(k-1).eq.(ip(1)-1)) snmax(8)=snmax(8)+1
                    endif
                  endif
                endif
              endif
            endif
          endif
        endif
