module ft8_mod1

  parameter (NPS=180000,NPS1=184320,NFR=151680) !NFRAME=1920*79
  real*4 dd8(nps),dd8m(nps)
  complex cw(nps1),cfilt1(nps1),cfilt2(nps1),cfilt3(nps1),cfilt4(nps1),cfilt5(nps1),cfilt6(nps1),cfilt7(nps1),   &
          cfilt8(nps1),cfilt9(nps1),cfilt10(nps1),cfilt11(nps1),cfilt12(nps1),cref1(nfr),cref2(nfr),cref3(nfr),  &
          cref4(nfr),cref5(nfr),cref6(nfr),cref7(nfr),cref8(nfr),cref9(nfr),cref10(nfr),cref11(nfr),cref12(nfr), &
          csync(0:6,32),csynce(0:18,32),csyncsd(0:18,32),csyncsdcq(0:57,32),csynccq(0:8,32)
  character*37 allmessages(200),msgsd76(76),msg(56),msgroot
  character lasthcall*12,mycall12_0*12,mycall12_00*12,hiscall12_0*12,hisgrid4*4
  character(len=12) :: mycall,hiscall,mybcall,hisbcall
  real allfreq(200),windowc1(0:100),windowx(0:200),scqnr(64),smycnr(64),pivalue,facx,twopi,facc1,dt,sumxdt,avexdt
  integer itone76(76,79),idtone76(76,58),itone56(56,79),idtone56(56,58),idtone25(25,58),allsnrs(200),apsym(58), &
          idtonemyc(58),mcq(29),mrrr(19),m73(19),mrr73(19),naptypes(0:5,12),icos7(0:6),graymap(0:7),nappasses(0:5), &
          nmsg,ndecodes,nlasttx,mycalllen1,msgrootlen,nFT8decd,nfawide,nfbwide,nhaptypes(0:5,14),apsymsp(66), &
          apsymdxns1(58),apsymdxns2(58),ndxnsaptypes(0:5,14),apcqsym(77),apsymdxnsrr73(77),apsymdxns73(77), &
          nft8cycles,nft8swlcycles,ncandall
  logical one(0:511,0:8),lqsomsgdcd
  logical(1) lapmyc,lagcc,lagccbail,lhound,lenabledxcsearch,lwidedxcsearch,lmultinst,lskiptx1
  data     mcq/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0/
  data    mrrr/0,1,1,1,1,1,1,0,1,0,0,1,0,0,1,0,0,0,1/
  data     m73/0,1,1,1,1,1,1,0,1,0,0,1,0,1,0,0,0,0,1/
  data   mrr73/0,1,1,1,1,1,1,0,0,1,1,1,0,1,0,1,0,0,1/
  data   icos7/3,1,4,0,6,5,2/
  data graymap/0,1,3,2,5,6,4,7/
  data mycall12_0/'dummy'/
  data mycall12_00/'dummy'/
  data hiscall12_0/'dummy'/
  data naptypes(0,1:12)/0,0,0,2,2,2,1,1,1,31,36,35/ ! Tx6 CQ
  data naptypes(1,1:12)/3,3,3,2,2,2,1,1,1,31,36,35/ ! Tx1
  data naptypes(2,1:12)/3,3,3,2,2,2,1,1,1,31,36,35/ ! Tx2
  data naptypes(3,1:12)/3,3,3,4,5,6,0,0,0,31,36,35/ ! Tx3
  data naptypes(4,1:12)/3,3,3,4,5,6,0,0,0,31,36,35/ ! Tx4
  data naptypes(5,1:12)/3,3,3,2,2,2,1,1,1,31,36,35/ ! Tx5
  data nhaptypes(0,1:14)/0,0,0,0,0,0,0,0,0,0,0,0,31,36/ ! Tx6 CQ, possible in idle mode
  data nhaptypes(1,1:14)/21,21,21,22,22,22,0,0,0,0,0,0,31,36/ ! Tx1 Grid !!! to add iaptype 5,6
  data nhaptypes(2,1:14)/0,0,0,0,0,0,0,0,0,0,0,0,31,36/ ! Tx2 none
  data nhaptypes(3,1:14)/21,21,21,22,22,22,23,23,23,24,24,24,31,36/ ! Tx3 RRreport
  data nhaptypes(4,1:14)/0,0,0,0,0,0,0,0,0,0,0,0,31,36/ ! Tx4 none
  data nhaptypes(5,1:14)/0,0,0,0,0,0,0,0,0,0,0,0,31,36/ ! Tx5 none
  data ndxnsaptypes(0,1:14)/1,1,1,31,31,0,36,36,0,0,31,36,35,0/       ! Tx6 CQ
  data ndxnsaptypes(1,1:14)/11,11,11,1,1,1,31,36,0,0,31,36,35,0/      ! Tx1 Grid
  data ndxnsaptypes(2,1:14)/11,11,11,1,1,1,31,36,0,0,31,36,35,0/      ! Tx2 Report
  data ndxnsaptypes(3,1:14)/11,11,11,13,13,13,14,14,14,12,31,36,35,1/ ! Tx3 RRreport
  data ndxnsaptypes(4,1:14)/11,11,11,13,13,13,14,14,14,12,31,36,35,1/ ! Tx4 RRR,RR73
  data ndxnsaptypes(5,1:14)/14,14,14,13,13,13,1,1,1,12,31,36,35,0/    ! Tx5 73
  data avexdt/0.0/
  
  type odd_struct
    real freq
    real dt
    logical lstate
    character*37 msg
  end type odd_struct
  type(odd_struct) odd(130)

  type even_struct
    real freq
    real dt
    logical lstate
    character*37 msg
  end type even_struct
  type(even_struct) even(130)

  type oddcopy_struct
    real freq
    real dt
    logical lstate
    character*37 msg
  end type oddcopy_struct
  type(oddcopy_struct) oddcopy(130)

  type evencopy_struct
    real freq
    real dt
    logical lstate
    character*37 msg
  end type evencopy_struct
  type(evencopy_struct) evencopy(130)
  
  type lastrxmsg_struct
    real xdt
    logical lstate
    character*37 lastmsg
  end type lastrxmsg_struct
  type (lastrxmsg_struct) lastrxmsg(1)
  data lastrxmsg%lstate/.false./

  type callsigndt_struct
    real dt
    character*12 call2
  end type callsigndt_struct
  type (callsigndt_struct) calldt(200)

  type incall_struct
    real xdt
    character*37 msg
  end type incall_struct
  type (incall_struct) incall(20)

end module ft8_mod1