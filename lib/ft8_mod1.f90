module ft8_mod1

  parameter (NPS=180000,NFR=151680,NFILT1=4000,NFILT2=3400,numcqsig=20,numdeccq=40,nummycsig=5,numdecmyc=25,nmaxthreads=24) !NFRAME=1920*79
  real*4 dd8(nps)
  real endcorr(NFILT1/2+1),endcorrswl(NFILT2/2+1)
  complex cw(nps),csync(0:6,32),csynce(0:18,32),csyncsd(0:18,32),csyncsdcq(0:57,32),csynccq(0:7,32),ctwkw(11,32), &
          ctwkn(11,32),ctwk256(256)
  character*37 allmessages(200),msgsd76(76),msg(56),msgroot,msgincall(174)
  character lasthcall*12,mycall12_0*12,mycall12_00*12,hiscall12_0*12,hisgrid4*4
  character(len=12) :: mycall,hiscall,mybcall,hisbcall
  real allfreq(200),windowc1(0:54),windowx(0:200),pivalue,facx,twopi,facc1,dt,sumxdtt(24),avexdt, &
       xdtincall(174)
  integer itone76(76,79),idtone76(76,58),itone56(56,79),idtone56(56,58),idtone25(25,58),allsnrs(200),apsym(58),     &
          idtonemyc(58),mcq(29),mrrr(19),m73(19),mrr73(19),naptypes(0:5,27),icos7(0:6),graymap(0:7),nappasses(0:5), &
          nmsg,ndecodes,nlasttx,mycalllen1,msgrootlen,nfawide,nfbwide,nhaptypes(0:5,27),apsymsp(66),                &
          apsymdxns1(58),apsymdxnsrrr(77),ndxnsaptypes(0:5,27),apcqsym(77),apsymdxnsrr73(77),apsymdxns73(77),       &
          nft8cycles,nft8swlcycles,ncandallthr(24),maskincallthr(25),nincallthr(24),idtonecqdxcns(58),              &
          apsymmyns1(29),apsymmyns2(58),apsymmynsrr73(77),apsymmyns73(77),nmycnsaptypes(0:5,27),apsymdxstd(58),     &
          apsymdxnsr73(77),apsymdxns732(77),apsymmynsrrr(77),idtonedxcns73(58),idtonefox73(58),idtonespec(58),nintcount
  integer*1 gen(91,174)
  logical one(0:511,0:8),lqsomsgdcd,first_osd
  logical(1) lapmyc,lagcc,lagccbail,lhound,lenabledxcsearch,lwidedxcsearch,lmultinst,lskiptx1,ltxing
  data maskincallthr/0,30,45,55,65,75,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160,165,170,175/
  data     mcq/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0/
  data    mrrr/0,1,1,1,1,1,1,0,1,0,0,1,0,0,1,0,0,0,1/
  data     m73/0,1,1,1,1,1,1,0,1,0,0,1,0,1,0,0,0,0,1/
  data   mrr73/0,1,1,1,1,1,1,0,0,1,1,1,0,1,0,1,0,0,1/
  data   icos7/3,1,4,0,6,5,2/
  data graymap/0,1,3,2,5,6,4,7/
  data mycall12_0/'dummy'/
  data mycall12_00/'dummy'/
  data hiscall12_0/'dummy'/
! Hound OFF, MyCall is standard, DXCall is standard or empty
  data naptypes(0,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,1,1,1,31,31,31,36,36,36,35,35,35/ ! Tx6 CQ
  data naptypes(1,1:27)/3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/ ! Tx1 Grid
  data naptypes(2,1:27)/3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/ ! Tx2 Report
  data naptypes(3,1:27)/3,3,3,6,6,6,5,5,5,4,4,4,0,0,0,0,0,0,31,31,31,36,36,36,35,35,35/ ! Tx3 RRreport
  data naptypes(4,1:27)/3,3,3,6,6,6,5,5,5,4,4,4,2,2,2,0,0,0,31,31,31,36,36,36,35,35,35/ ! Tx4 RRR,RR73
  data naptypes(5,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,1,1,1,31,31,31,36,36,36,35,35,35/ ! Tx5 73
! Hound OFF, MyCall is non-standard, DXCall is standard or empty
  data nmycnsaptypes(0,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx6 CQ
  data nmycnsaptypes(1,1:27)/41,41,41,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx1 DXcall MyCall
  data nmycnsaptypes(2,1:27)/41,41,41,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx2 Report
  data nmycnsaptypes(3,1:27)/41,41,41,44,44,44,43,43,43,42,42,42,0,0,0,0,0,0,31,31,31,36,36,36,35,35,35/    ! Tx3 RRreport
  data nmycnsaptypes(4,1:27)/41,41,41,44,44,44,43,43,43,42,42,42,40,40,40,0,0,0,31,31,31,36,36,36,35,35,35/ ! Tx4 RRR,RR73
  data nmycnsaptypes(5,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx5 73
! Hound mode
  data nhaptypes(0,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,111,111,111/       ! Tx6 CQ, possible in idle mode if DXCall is empty
  data nhaptypes(1,1:27)/21,21,21,22,22,22,0,0,0,0,0,0,0,0,0,31,31,31,0,0,0,36,36,36,0,0,0/ ! Tx1 Grid idle mode or transmitting
  data nhaptypes(2,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/             ! Tx2 none
  data nhaptypes(3,1:27)/21,21,21,22,22,22,23,23,23,24,24,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/ ! Tx3 RRreport QSO in progress or QSO is finished
  data nhaptypes(4,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/             ! Tx4 none
  data nhaptypes(5,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/             ! Tx5 none
!non-standard DXCall
  data ndxnsaptypes(0,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx6 CQ
  data ndxnsaptypes(1,1:27)/11,11,11,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/          ! Tx1 Grid
  data ndxnsaptypes(2,1:27)/11,11,11,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/          ! Tx2 Report
  data ndxnsaptypes(3,1:27)/11,11,11,14,14,14,13,13,13,12,12,12,0,0,0,0,0,0,31,31,31,36,36,36,35,35,35/ ! Tx3 RRreport
  data ndxnsaptypes(4,1:27)/11,11,11,14,14,14,13,13,13,12,12,12,2,2,2,0,0,0,31,31,31,36,36,36,35,35,35/ ! Tx4 RRR,RR73
  data ndxnsaptypes(5,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx5 73
  data nintcount/0/
  data avexdt/0.0/
  data first_osd/.true./
  
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

  type callsigndtodd_struct
    real dt
    character*12 call2
  end type callsigndtodd_struct
  type (callsigndtodd_struct) calldtodd(150)

  type callsigndteven_struct
    real dt
    character*12 call2
  end type callsigndteven_struct
  type (callsigndteven_struct) calldteven(150)

  type incall_struct
    real xdt
    character*37 msg
  end type incall_struct
  type (incall_struct) incall(30)

  type evencq_struct
    real freq
    real xdt
    complex cs(0:7,79)
  end type evencq_struct
  type(evencq_struct) evencq(numcqsig,nmaxthreads) ! 24 threads

  type oddcq_struct
    real freq
    real xdt
    complex cs(0:7,79)
  end type oddcq_struct
  type(oddcq_struct) oddcq(numcqsig,nmaxthreads)

  type evenmyc_struct
    real freq
    real xdt
    complex cs(0:7,79)
  end type evenmyc_struct
  type(evenmyc_struct) evenmyc(nummycsig,nmaxthreads)

  type oddmyc_struct
    real freq
    real xdt
    complex cs(0:7,79)
  end type oddmyc_struct
  type(oddmyc_struct) oddmyc(nummycsig,nmaxthreads)

  type evenqso_struct
    real freq
    real xdt
    complex cs(0:7,79)
  end type evenqso_struct
  type(evenqso_struct) evenqso(1,nmaxthreads)

  type oddqso_struct
    real freq
    real xdt
    complex cs(0:7,79)
  end type oddqso_struct
  type(oddqso_struct) oddqso(1,nmaxthreads)

end module ft8_mod1