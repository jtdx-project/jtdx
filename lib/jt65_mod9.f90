! last time modified by Igor UA3DJY on 20200217

module jt65_mod9 
! stores callsign DB in memory for false decodes checking
  
  parameter (MAXC=200000)
! supporting 7-char Australian callsigns
  character*7 call0c(12000),calld(12000),callef(9000),callgh(7500),calli(7600),callj(8000),callk(23000),calllm(6000), &
              calln(9500),callo(7000),callpq(6000),callr(6000),callst(7500),calluv(10500),callw(15000),callxz(5300)
  character callsign*12
  character*180 line
  integer ncall0c,ncalld,ncallef,ncallgh,ncalli,ncallj,ncallk,ncalllm,ncalln,ncallo,ncallpq,ncallr,ncallst,ncalluv, &
          ncallw,ncallxz
  logical ldbvalid
  data ldbvalid/.true./
  data ncall0c,ncalld,ncallef,ncallgh,ncalli,ncallj,ncallk,ncalllm,ncalln,ncallo,ncallpq,ncallr,ncallst,ncalluv,ncallw, &
       ncallxz/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/

end module jt65_mod9