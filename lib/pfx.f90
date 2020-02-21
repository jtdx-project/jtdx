  parameter (NZ=339)                     !Total number of prefixes
  parameter (NZ2=12)                     !Total number of suffixes
  character*1 sfx(NZ2)
  character*5 pfx(NZ)

  data sfx/'P','0','1','2','3','4','5','6','7','8','9','A'/
  data pfx/                                                             &
       '1A   ','1S   ','3A   ','3B6  ','3B8  ','3B9  ','3C   ','3C0  ', &
       '3D2  ','3D2C ','3D2R ','3DA  ','3V   ','3W   ','3X   ','3Y   ', &
       '3YB  ','3YP  ','4J   ','4L   ','4S   ','4U1I ','4U1U ','4W   ', &
       '4X   ','5A   ','5B   ','5H   ','5N   ','5R   ','5T   ','5U   ', &
       '5V   ','5W   ','5X   ','5Z   ','6W   ','6Y   ','7O   ','7P   ', &
       '7Q   ','7X   ','8P   ','8Q   ','8R   ','9A   ','9G   ','9H   ', &
       '9J   ','9K   ','9L   ','9M2  ','9M6  ','9N   ','9Q   ','9U   ', &
       '9V   ','9X   ','9Y   ','A2   ','A3   ','A4   ','A5   ','A6   ', &
       'A7   ','A9   ','AP   ','BS7  ','BV   ','BV9  ','BY   ','C2   ', &
       'C3   ','C5   ','C6   ','C9   ','CE   ','CE0X ','CE0Y ','CE0Z ', &
       'CE9  ','CM   ','CN   ','CP   ','CT   ','CT3  ','CU   ','CX   ', &
       'CY0  ','CY9  ','D2   ','D4   ','D6   ','DL   ','DU   ','E3   ', &
       'E4   ','EA   ','EA6  ','EA8  ','EA9  ','EI   ','EK   ','EL   ', &
       'EP   ','ER   ','ES   ','ET   ','EU   ','EX   ','EY   ','EZ   ', &
       'F    ','FG   ','FH   ','FJ   ','FK   ','FKC  ','FM   ','FO   ', &
       'FOA  ','FOC  ','FOM  ','FP   ','FR   ','FRG  ','FRJ  ','FRT  ', &
       'FT5W ','FT5X ','FT5Z ','FW   ','FY   ','M    ','MD   ','MI   ', &
       'MJ   ','MM   ',        'MU   ','MW   ','H4   ','H40  ','HA   ', &
       'HB   ','HB0  ','HC   ','HC8  ','HH   ','HI   ','HK   ','HK0  ', &
       'HK0M ','HL   ','HM   ','HP   ','HR   ','HS   ','HV   ','HZ   ', &
       'I    ','IS   ','IS0  ',        'J2   ','J3   ','J5   ','J6   ', &
       'J7   ','J8   ','JA   ','JDM  ','JDO  ','JT   ','JW   ',         &
       'JX   ','JY   ','K    ','KG4  ','KH0  ','KH1  ','KH2  ','KH3  ', &
       'KH4  ','KH5  ','KH5K ','KH6  ','KH7  ','KH8  ','KH9  ','KL   ', &
       'KP1  ','KP2  ','KP4  ','KP5  ','LA   ','LU   ','LX   ','LY   ', &
       'LZ   ','OA   ','OD   ','OE   ','OH   ','OH0  ','OJ0  ','OK   ', &
       'OM   ','ON   ','OX   ','OY   ','OZ   ','P2   ','P4   ','PA   ', &
       'PJ2  ','PJ7  ','PY   ','PY0F ','PT0S ','PY0T ','PZ   ','R1F  ', &
       'R1M  ','S0   ','S2   ','S5   ','S7   ','S9   ','SM   ','SP   ', &
       'ST   ','SU   ','SV   ','SVA  ','SV5  ','SV9  ','T2   ','T30  ', &
       'T31  ','T32  ','T33  ','T5   ','T7   ','T8   ','T9   ','TA   ', &
               'TF   ','TG   ','TI   ','TI9  ','TJ   ','TK   ','TL   ', &
       'TN   ','TR   ','TT   ','TU   ','TY   ','TZ   ','UA   ','UA2  ', &
       'UA9  ','UK   ','UN   ','UR   ','V2   ','V3   ','V4   ','V5   ', &
       'V6   ','V7   ','V8   ','VE   ','VK   ','VK0H ','VK0M ','VK9C ', &
       'VK9L ','VK9M ','VK9N ','VK9W ','VK9X ','VP2E ','VP2M ','VP2V ', &
       'VP5  ','VP6  ','VP6D ','VP8  ','VP8G ','VP8H ','VP8O ','VP8S ', &
       'VP9  ','VQ9  ','VR   ','VU   ','VU4  ','VU7  ','XE   ','XF4  ', &
       'XT   ','XU   ','XW   ','XX9  ','XZ   ','YA   ','YB   ','YI   ', &
       'YJ   ','YK   ','YL   ','YN   ','YO   ','YS   ','YU   ','YV   ', &
       'YV0  ','Z2   ','Z3   ','ZA   ','ZB   ','ZC4  ','ZD7  ','ZD8  ', &
       'ZD9  ','ZF   ','ZK1N ','ZK1S ','ZK2  ','ZK3  ','ZL   ','ZL7  ', &
       'ZL8  ','ZL9  ','ZP   ','ZS   ','ZS8  ','KC4  ','E5   '/
