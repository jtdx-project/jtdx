! This source code file was last time modified by Igor UA3DJY on 20190108

! call may not start from 'Q', zero and two digits
  if(callsign(1:1).eq.'Q' .or. callsign(1:1).eq.'0' .or. callsign(1:1).eq.'/') then
        nbadcrc=1; msg37='                                     '; return; endif

   if(callsign(1:1).ge.'0' .and. callsign(1:1).le.'9') then
     if(callsign(2:2).ge.'0' .and. callsign(2:2).le.'9') then
       nbadcrc=1; msg37='                                     '; return; endif
     if(callsign(2:2).ge.'A' .and. callsign(2:2).le.'Z' .and. callsign(3:3).ge.'A' .and. callsign(3:3).le.'Z' &
       .and. callsign(4:4).ge.'A' .and. callsign(4:4).le.'Z') then
       nbadcrc=1; msg37='                                     '; return; endif
   endif

   if(callsign(1:1).ge.'A' .and. callsign(1:1).le.'Z') then
     if(callsign(2:2).ge.'A' .and. callsign(2:2).le.'Z' &
       .and. callsign(3:3).ge.'A' .and. callsign(3:3).le.'Z') then
       nbadcrc=1; msg37='                                     '; return; endif
   endif

   if(callsign(1:1).ge.'0' .and. callsign(1:1).le.'9') then
     if(callsign(2:2).ge.'A' .and. callsign(2:2).le.'Z' &
       .and. callsign(3:3).ge.'A' .and. callsign(3:3).le.'Z') then
       if(callsign(1:3).ne.'3DA' .and. callsign(1:3).ne.'3XY') then
         nbadcrc=1; msg37='                                     '; return; endif
     endif
   endif