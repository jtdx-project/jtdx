! This source code file was last time modified by Igor Chernikov UA3DJY on November 26th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

  parameter (MAXTEST=35,NTEST=27)
  character*22 testmsg(MAXTEST)
  data testmsg(1:NTEST)/         &
       "CQ WB9XYZ EN34",         &
       "CQ DX WB9XYZ EN34",      &
       "QRZ WB9XYZ EN34",        &
       "KA1ABC WB9XYZ EN34",     &
       "KA1ABC WB9XYZ RO",       &
       "KA1ABC WB9XYZ -21",      &
       "KA1ABC WB9XYZ R-19",     &
       "KA1ABC WB9XYZ RRR",      &
       "KA1ABC WB9XYZ 73",       &
       "KA1ABC WB9XYZ",          &
       "CQ 000 WB9XYZ EN34",     &
       "CQ 999 WB9XYZ EN34",     &
       "CQ EU WB9XYZ EN34",      &
       "CQ WY WB9XYZ EN34",      &
       "ZL/KA1ABC WB9XYZ",       &
       "KA1ABC ZL/WB9XYZ",       &
       "KA1ABC/4 WB9XYZ",        &
       "KA1ABC WB9XYZ/4",        &
       "CQ ZL4/KA1ABC",          &
       "DE ZL4/KA1ABC",          &
       "QRZ ZL4/KA1ABC",         &
       "CQ WB9XYZ/VE4",          &
       "HELLO WORLD",            &
       "ZL4/KA1ABC 73",          &
       "KA1ABC XL/WB9XYZ",       &
       "KA1ABC WB9XYZ/W4",       &
       "123456789ABCDEFGH"/
