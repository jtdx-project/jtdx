! This source code file was last time modified by Igor UA3DJY on 20190108

! call may not start from 'Q', zero and two digits
  if(call_a(1:1).eq.'Q' .or. call_b(1:1).eq.'Q') then
    nbadcrc=1; msg37='                                     '; return; endif
  if(call_a(1:1).eq.'0' .or. call_b(1:1).eq.'0') then
    nbadcrc=1; msg37='                                     '; return; endif
  if(call_a(1:1).ge.'0' .and. call_a(1:1).le.'9' .and. call_a(2:2).ge.'0' .and. call_a(2:2).le.'9') then
    nbadcrc=1; msg37='                                     '; return; endif
  if(call_b(1:1).ge.'0' .and. call_b(1:1).le.'9' .and. call_b(2:2).ge.'0' .and. call_b(2:2).le.'9') then
    nbadcrc=1; msg37='                                     '; return; endif