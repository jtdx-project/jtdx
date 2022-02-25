subroutine ft8b(newdat1,nQSOProgress,nfqso,nftx,napwid,lsubtract,npos,freqsub,tmpcqdec,tmpmyc,          &
                nagainfil,iaptype,f1,xdt,nbadcrc,lft8sdec,msg37,msg37_2,xsnr,swl,stophint,              &
                nthr,lFreeText,ipass,lft8subpass,lspecial,lcqcand,ncqsignal,nmycsignal,npass,           &
                i3bit,lhidehash,lft8s,lmycallstd,lhiscallstd,levenint,loddint,lft8sd,i3,n3,nft8rxfsens, &
                ncount,msgsrcvd,lrepliedother,lhashmsg,lqsothread,lft8lowth,lhighsens,lsubtracted,      &
                tmpcqsig,tmpmycsig,tmpqsosig,lnohiscall,lnomycall,lnohisgrid)
!  use timer_module, only: timer
  use packjt77, only : unpack77
  use ft8_mod1, only : allmessages,ndecodes,apsym,mcq,m73,mrr73,mrrr,icos7,naptypes,nhaptypes,one,graymap,                     &
                       oddcopy,evencopy,lastrxmsg,lasthcall,nlasttx,calldteven,calldtodd,lqsomsgdcd,mycalllen1,                &
                       msgroot,msgrootlen,allfreq,idtone25,lapmyc,idtonemyc,mycall,hiscall,lhound,apsymsp,                     &
                       ndxnsaptypes,apsymdxns1,apsymdxnsrrr,lenabledxcsearch,lwidedxcsearch,apcqsym,apsymdxnsrr73,apsymdxns73, &
                       mybcall,hisbcall,lskiptx1,nft8cycles,nft8swlcycles,ctwkw,ctwkn,nincallthr,msgincall,xdtincall,          &
                       maskincallthr,ctwk256,numcqsig,numdeccq,evencq,oddcq,nummycsig,numdecmyc,evenmyc,oddmyc,idtone56,       &
                       idtonecqdxcns,evenqso,oddqso,nmycnsaptypes,apsymmyns1,apsymmyns2,apsymmynsrr73,apsymmyns73,apsymdxstd,  &
                       apsymdxnsr73,apsymdxns732,ltxing,apsymmynsrrr,idtonedxcns73,idtonefox73,idtonespec

  include 'ft8_params.f90'
  character c77*77,msg37*37,msg37_2*37,msgd*37,msgbase37*37,call_a*12,call_b*12,callsign*12,grid*12
  character*37 msgsrcvd(130)
  complex cd0(-800:4000),cd1(-800:4000),cd2(-800:4000),cd3(-800:4000),ctwk(32),csymb(32),cs(0:7,79),csymbr(32),csr(0:7,79), &
          csig(32),csig0(151680),z1,csymb256(256),cstmp2(0:7,79),csold(0:7,79),cscs(0:7,79)
  real a(5),s8(0:7,79),s82(0:7,79),s2(0:511),sp(0:7),s81(0:7),snrsync(21),syncw(7),sumkw(7),scoreratiow(7),freqsub(200), &
       s256(0:8),s2563(0:26),syncavpart(3)
  real bmeta(174),bmetb(174),bmetc(174),bmetd(174)
  real llra(174),llrb(174),llrc(174),llrd(174),llrz(174)
  integer*1 message77(77),apmask(174),cw(174),nsmax(8)
  integer itone(79),ip(1),ka(1),nqsoend(3)
  integer, intent(in) :: nQSOProgress,nfqso,nftx,napwid,nthr,ipass,nft8rxfsens
  logical newdat1,lsubtract,lFreeText,nagainfil,lspecial,unpk77_success
  logical(1), intent(in) :: swl,stophint,lft8subpass,lhidehash,lmycallstd,lhiscallstd,lqsothread,lft8lowth, &
                            lhighsens,lcqcand,levenint,loddint,lnohiscall,lnomycall,lnohisgrid
  logical(1) falsedec,lastsync,ldupemsg,lft8s,lft8sdec,lft8sd,lsdone,ldupeft8sd,lrepliedother,lhashmsg,                &
             lvirtual2,lvirtual3,lsd,lcq,ldeepsync,lcallsstd,lfound,lsubptxfreq,lreverse,lchkcall,lgvalid,             &
             lwrongcall,lsubtracted,lcqsignal,loutapwid,lfoundcq,lmycsignal,lfoundmyc,lqsosig,ldxcsig,lcqdxcsig,       &
             lcqdxcnssig,lqsocandave,lcall1hash,lqsosigtype3,lqso73,lqsorr73,lqsorrr,lfoxspecrpt,lfoxstdr73,lapcqonly, &
             lcall2hash,lskipnotap

  type tmpcqdec_struct
    real freq
    real xdt
  end type tmpcqdec_struct
  type(tmpcqdec_struct) tmpcqdec(numdeccq)

  type tmpcqsig_struct
    real freq
    real xdt
    complex cs(0:7,79)
  end type tmpcqsig_struct
  type(tmpcqsig_struct) tmpcqsig(numcqsig) ! 20 sigs 24 threads

  type tmpmyc_struct
    real freq
    real xdt
  end type tmpmyc_struct
  type(tmpmyc_struct) tmpmyc(numdecmyc)

  type tmpmycsig_struct
    real freq
    real xdt
    complex cs(0:7,79)
  end type tmpmycsig_struct
  type(tmpmycsig_struct) tmpmycsig(nummycsig) ! 5 sigs

  type tmpqsosig_struct
    real freq
    real xdt
    complex cs(0:7,79)
  end type tmpqsosig_struct
  type(tmpqsosig_struct) tmpqsosig(1)

  max_iterations=30; nharderrors=-1; nbadcrc=1; delfbest=0.; ibest=0; dfqso=500.; rrxdt=0.5
  fs2=200.; dt2=0.005 ! fs2=12000.0/NDOWN; dt2=1.0/fs2
  lcall1hash=.false.; lcall2hash=.false.; lskipnotap=.false.
  ldeepsync=.false.; if(lft8lowth .or. lft8subpass .or. swl) ldeepsync=.true.
  lcallsstd=.true.; if(.not.lmycallstd .or. .not.lhiscallstd) lcallsstd=.false.

  xdt0=xdt; f10=f1
! apply last freq f1 and last DT criteria here  
  nqso=1
  if(lqsothread .and. .not. lft8sdec .and. .not.lqsomsgdcd .and. .not.stophint .and. nlasttx.ge.1 &
    .and. nlasttx.le.4 .and. abs(f10-nfqso).lt.2.51) then
    if(lastrxmsg(1)%lstate .and. abs(lastrxmsg(1)%xdt-xdt).lt.0.18) then; nqso=2
      elseif(.not.lastrxmsg(1)%lstate) then; nqso=2
    endif
  endif

  lvirtual2=.false.; lvirtual3=.false.; maxlasttx=4
  if(lqsothread .and. .not. lft8sdec .and. .not.lqsomsgdcd) then
    if(len_trim(hiscall).gt.2) then
      if(xdt.gt.4.9 .or. xdt.lt.-4.9) then
        if(lastrxmsg(1)%lstate .and. trim(lastrxmsg(1)%lastmsg).eq.trim(msgroot)//' RRR') maxlasttx=5
      endif
      if(xdt.gt.4.9) then
        if(.not.stophint .and. nlasttx.ge.1 .and. nlasttx.le.maxlasttx .and. abs(f10-nfqso).lt.0.1) then
          if(lastrxmsg(1)%lstate) then; xdt0=lastrxmsg(1)%xdt; nqso=2; lvirtual2=.true.; endif
          if(.not.lastrxmsg(1)%lstate) then
            if(levenint) then
              do i=1,150
                if(trim(calldteven(i)%call2).eq.trim(hiscall)) then
                  xdt0=calldteven(i)%dt; nqso=3; lvirtual2=.true.; exit
                endif
              enddo
            else if(loddint) then 
              do i=1,150
                if(trim(calldtodd(i)%call2).eq.trim(hiscall)) then
                  xdt0=calldtodd(i)%dt; nqso=3; lvirtual2=.true.; exit
                endif
              enddo
            endif
          endif
        endif
      elseif (xdt.lt.-4.9) then
        if(.not.stophint .and. nlasttx.ge.1 .and. nlasttx.le.maxlasttx .and. abs(f10-nfqso).lt.0.1) then
          if(lastrxmsg(1)%lstate) then; xdt0=lastrxmsg(1)%xdt; nqso=3; lvirtual3=.true.; endif
          if(.not.lastrxmsg(1)%lstate) then
            if(levenint) then
              do i=1,150
                if(trim(calldteven(i)%call2).eq.trim(hiscall)) then
                  xdt0=calldteven(i)%dt; nqso=3; lvirtual3=.true.; exit
                endif
              enddo
            else if(loddint) then
              do i=1,150
                if(trim(calldtodd(i)%call2).eq.trim(hiscall)) then
                  xdt0=calldtodd(i)%dt; nqso=3; lvirtual3=.true.; exit
                endif
              enddo
            endif
          endif
        endif
      endif
    endif
  endif

  !call timer('ft8_down',0)
  call ft8_downsample(newdat1,f1,nqso,cd0,cd2,cd3,lhighsens,lsubtracted,npos,freqsub)   !Mix f1 to baseband and downsample
  !call timer('ft8_down',1)

  lsd=.false.; isd=1; lcq=.false.
  if(levenint) then
    do i=1,130
      if(.not.evencopy(i)%lstate) cycle
      if(abs(evencopy(i)%freq-f1).lt.3.0 .and. abs(evencopy(i)%dt-xdt).lt.0.19) then
        msgd=evencopy(i)%msg; lsd=.true.; isd=i
        if(msgd(1:3).eq.'CQ ' .or. msgd(1:3).eq.'DE ' .or. msgd(1:4).eq.'QRZ ') lcq=.true.
      endif
    enddo
  elseif(loddint) then
    do i=1,130
      if(.not.oddcopy(i)%lstate) cycle
      if(abs(oddcopy(i)%freq-f1).lt.3.0 .and. abs(oddcopy(i)%dt-xdt).lt.0.19) then
        msgd=oddcopy(i)%msg; lsd=.true.; isd=i
        if(msgd(1:3).eq.'CQ ' .or. msgd(1:3).eq.'DE ' .or. msgd(1:4).eq.'QRZ ') lcq=.true.
      endif
    enddo
  endif

  if(lsd .and. nqso.eq.1) nqso=4

!nlasttx  last TX message
!  0       Tx was halted
!  1      AA1AA BB1BB PL35
!  2      AA1AA BB1BB -15
!  3      AA1AA BB1BB R-15
!  4      AA1AA BB1BB RRR/RR73
!  5      AA1AA BB1BB 73
!  6      CQ BB1BB PL35

  if(nqso.eq.4) cd1=cd0
  do iqso=1,nqso
    lapcqonly=.false.
    if(iqso.gt.1 .and. iqso.lt.4 .and. nqso.eq.4) cycle
    if(xdt0.lt.-4.9 .or. xdt0.gt.4.9) cycle
    if(lvirtual2 .and. iqso.ne.2) cycle; if(lvirtual3 .and. iqso.lt.2) cycle
    if((lvirtual2 .or. lvirtual3) .and. nft8rxfsens.lt.3 .and. iqso.eq.3) cycle
    lastsync=.false.; lsdone=.false.
    if(.not.lvirtual2 .and. .not.lvirtual3 .and. iqso.eq.2) then; cd0=cd2
    elseif(lvirtual2 .and. iqso.eq.2) then; cd0=cd2
    elseif(lvirtual3 .and. iqso.eq.2) then; cd0=cd3
    endif
    if(iqso.eq.4) then; call tonesd(msgd,lcq); if(.not.ldeepsync) go to 32; cd0=cd1; endif
    if(iqso.eq.3) go to 16
    i0=nint((xdt0+0.5)*fs2)                   !Initial guess for start of signal
    smax=0.0; ctwk=cmplx(1.0,0.0)
    do idt=i0-8,i0+8                         !Search over +/- one quarter symbol
       call sync8d(cd0,idt,ctwk,0,sync,ipass,lastsync,iqso,lcq,lcallsstd,lcqcand)
       if(sync.gt.smax) then
          smax=sync
          ibest=idt
       endif
    enddo
    xdt2=ibest*dt2                           !Improved estimate for DT

! Now peak up in frequency
    i0=nint(xdt2*fs2)
    smax=0.0; kstep=1
    do ifr=-5,5
      if(iqso.eq.1 .or. iqso.eq.4) then; ctwk=ctwkw(kstep,:); delf=ifr*0.5 ! Search over +/- 2.5 Hz
      else; ctwk=ctwkn(kstep,:); delf=ifr*0.25 ! Search over +/- 1.25 Hz
      endif
      call sync8d(cd0,i0,ctwk,1,sync,ipass,lastsync,iqso,lcq,lcallsstd,lcqcand)
      if(sync.gt.smax) then; smax=sync; delfbest=delf; endif
      kstep=kstep+1
    enddo
    a=0.0
    a(1)=-delfbest
    call twkfreq1(cd0,-800,3199,4000,fs2,a,cd0)
    xdt=xdt2
    f1=f10+delfbest                           !Improved estimate of DF
    dfqso=abs(nfqso-f1)
!write (*,"(F5.2,1x,I1,1x,F6.1,1x,a3)") xdt,ipass,f1,'out'
    lastsync=.true.
    call sync8d(cd0,i0,ctwk,2,sync,ipass,lastsync,iqso,lcq,lcallsstd,lcqcand)

16  continue
    if(iqso.eq.3) ibest=ibest+1

    syncav=3.; syncavpart=3.
    snrsync=0.
    do k=1,79
      i1=ibest+(k-1)*32
      csymb=cd0(i1:i1+31)
      if((k.ge.1 .and. k.le.7) .or. (k.ge.37 .and. k.le.43) .or. (k.ge.73 .and. k.le.79)) then
        call four2a(csymb,32,1,-1,1)
        s81(0:7)=abs(csymb(1:8))
        if(k.ge.1 .and. k.le.7) then
          synclev=s81(icos7(k-1)); snoiselev=(sum(s81)-synclev)/7.0
          if(snoiselev.gt.1.E-16) snrsync(k)=synclev/snoiselev
        else if(k.ge.37 .and. k.le.43) then
          synclev=s81(icos7(k-37)); snoiselev=(sum(s81)-synclev)/7.0
          if(snoiselev.gt.1.E-16) snrsync(k-29)=synclev/snoiselev
        else if(k.ge.73 .and. k.le.79) then
          synclev=s81(icos7(k-73)); snoiselev=(sum(s81)-synclev)/7.0
          if(snoiselev.gt.1.E-16) snrsync(k-58)=synclev/snoiselev
        endif
      else
        cycle
      endif
    enddo
    syncav=sum(snrsync)/21.
    syncavpart(1)=sum(snrsync(1:7))/7.; syncavpart(2)=sum(snrsync(8:14))/7.; syncavpart(3)=sum(snrsync(15:21))/7.
    syncavemax=maxval(syncavpart,1)

!    plev=0.0
!    do k=1000,1009; abscd=abs(cd0(k)); if(abscd.gt.plev) plev=abscd; enddo
!    plev=plev/61.0
!    do k=0,3199; xx=plev*gran(); yy=plev*gran(); cd0(k)=cd0(k) + complex(xx,yy); enddo

    lreverse=.false.
    if(.not.swl) then
      if(nft8cycles.lt.2) then 
        if(ipass.eq.2) lreverse=.true.
      else
        if(ipass.eq.5 .or. ipass.eq.7) lreverse=.true.
      endif
    else ! swl
      if(nft8swlcycles.lt.2) then 
        if(ipass.eq.2) lreverse=.true.
      else
        if(ipass.eq.5 .or. ipass.eq.7) lreverse=.true.
      endif
    endif

    do k=1,79
      i1=ibest+(k-1)*32
      csymb=cd0(i1:i1+31)
      if(syncav.lt.2.5) then
        csymb(1)=csymb(1)*1.9; csymb(32)=csymb(32)*1.9
        scr=SQRT(abs(csymb(1)))/SQRT(abs(csymb(32)))
        if(scr.gt.1.0) then; csymb(32)=csymb(32)*scr; else; if(scr.gt.1.E-16) csymb(1)=csymb(1)/scr; endif
      endif
      do i=1,32; csymbr(i)=cmplx(real(csymb(33-i)),-aimag(csymb(33-i))); enddo
      if(lreverse) then
        call four2a(csymb,32,1,-1,1)
        cscs(0:7,k)=csymb(1:8)/1e3
        csymb=csymbr
      endif
      call four2a(csymb,32,1,-1,1)
      cs(0:7,k)=csymb(1:8)/1e3
      if(lreverse) then; csr=cs
      else; call four2a(csymbr,32,1,-1,1); csr(0:7,k)=csymbr(1:8)/1e3
      endif
      s8(0:7,k)=abs(csymb(1:8))
    enddo

    sp=0.
    do k=0,7; sp(k)=sum(s8(k,1:7))+sum(s8(k,18:79)); enddo
    ka=minloc(sp)-1; k=ka(1); if(k.lt.0) go to 128
    do kb=0,7
      if(kb.eq.k) cycle; spr=sp(kb)/sp(k)
      if(spr.gt.1.5) then
        s8(kb,:)=s8(kb,:)/spr; sprsqr=SQRT(spr)
        cs(kb,:)=cs(kb,:)/sprsqr; csr(kb,:)=csr(kb,:)/sprsqr; cscs(kb,:)=cscs(kb,:)/sprsqr
      endif
    enddo
128 continue

    if(iqso.gt.1 .and. iqso.lt.4) then; s82=SQRT(s8); go to 8; endif
    if(iqso.eq.4) go to 32

    nsyncscorew=0; scoreratiowa=0.; rrxdt=xdt-0.5
    if(rrxdt.ge.-0.5 .and. rrxdt.le.2.13) then
      do k=1,7; syncw(icos7(k-1)+1)=s8(icos7(k-1),k)+s8(icos7(k-1),k+36)+s8(icos7(k-1),k+72); enddo
      do k=1,7; sumkw(k)=(sum(s8(k-1,:))-syncw(k))/25.333; enddo ! (79-3)/3
    else if(rrxdt.lt.-0.5) then
      do k=1,7; syncw(icos7(k-1)+1)=s8(icos7(k-1),k+36)+s8(icos7(k-1),k+72); enddo
      do k=1,7; sumkw(k)=(sum(s8(k-1,26:79))-syncw(k))/26.; enddo ! (54-2)/2
    else if(rrxdt.gt.2.13) then
      do k=1,7; syncw(icos7(k-1)+1)=s8(icos7(k-1),k)+s8(icos7(k-1),k+36); enddo
      do k=1,7; sumkw(k)=(sum(s8(k-1,1:54))-syncw(k))/26.; enddo ! (54-2)/2
    endif
    do k=1,7; if(syncw(k).gt.sumkw(k)) nsyncscorew=nsyncscorew+1; scoreratiow(k)=syncw(k)/sumkw(k); enddo
    scoreratiowa=sum(scoreratiow)/7.

! sync quality check
    is1=0; is2=0; is3=0; nsyncscore=0; nsyncscore1=0; nsyncscore2=0; nsyncscore3=0
    scoreratio=0.; scoreratio1=0.; scoreratio2=0.; scoreratio3=0.; nsync2=0
    do k=1,7
      s81(:)=s8(:,k); ip=maxloc(s81)
      if(icos7(k-1).eq.(ip(1)-1)) then
        is1=is1+1
      else
        s81(ip(1)-1)=0.; ip=maxloc(s81)
        if(icos7(k-1).eq.(ip(1)-1)) nsync2=nsync2+1
      endif
      s81=s8(:,k+36); ip=maxloc(s81)
      if(icos7(k-1).eq.(ip(1)-1)) then
        is2=is2+1
      else
        s81(ip(1)-1)=0.; ip=maxloc(s81)
        if(icos7(k-1).eq.(ip(1)-1)) nsync2=nsync2+1
      endif
      s81=s8(:,k+72); ip=maxloc(s81)
      if(icos7(k-1).eq.(ip(1)-1)) then
        is3=is3+1
      else
        s81(ip(1)-1)=0.; ip=maxloc(s81)
        if(icos7(k-1).eq.(ip(1)-1)) nsync2=nsync2+1
      endif
      if(rrxdt.ge.-0.5) then
        synck=s8(icos7(k-1),k); sumk=(sum(s8(:,k))-synck)/7.0
        if(synck.gt.sumk) then; nsyncscore1=nsyncscore1+1; scoreratio1=scoreratio1+synck/sumk; endif
      endif
      synck=s8(icos7(k-1),k+36); sumk=(sum(s8(:,k+36))-synck)/7.0
      if(synck.gt.sumk) then; nsyncscore2=nsyncscore2+1; scoreratio2=scoreratio2+synck/sumk; endif
      if(rrxdt.le.2.13) then
        synck=s8(icos7(k-1),k+72); sumk=(sum(s8(:,k+72))-synck)/7.0
        if(synck.gt.sumk) then; nsyncscore3=nsyncscore3+1; scoreratio3=scoreratio3+synck/sumk; endif
      endif
    enddo
    nsyncscore=nsyncscore1+nsyncscore2+nsyncscore3; scoreratio=scoreratio1+scoreratio2+scoreratio3
! hard sync sum - max is 21
    nsync=is1+is2+is3
! bail out
    rscq=0.
    if(lcqcand) then
      do k11=8,16
        ip=maxloc(s8(:,k11))
        if(k11.lt.16) then; if(ip(1).eq.1) rscq=rscq+1.; else; if(ip(1).eq.2) rscq=rscq+1.; endif
      enddo
      ip=maxloc(s8(:,17)); if(ip(1).eq.1 .or. ip(1).eq.2) rscq=rscq+0.5
      ip=maxloc(s8(:,27)); if(ip(1).eq.1 .or. ip(1).eq.2) rscq=rscq+0.5
      ip=maxloc(s8(:,33)); if(ip(1).eq.3 .or. ip(1).eq.4) rscq=rscq+0.5
    endif
    if(lcqcand .and. nsync.eq.4) then
      if(nsync+nsync2.lt.12) then
        if(rscq.lt.6.6) then; nbadcrc=1; return; endif
      endif
    lapcqonly=.true.
    else if(lcqcand .and. nsync.eq.5) then
      if(nsync+nsync2.lt.12) then
        if(rscq.lt.6.1) then; nbadcrc=1; return; endif
      endif
    lapcqonly=.true.
    else if(lcqcand .and. nsync.eq.6) then
      if(nsync+nsync2.lt.11) then
        if(rscq.lt.5.6) then; nbadcrc=1; return; endif
      endif
    lapcqonly=.true.
    else
      if(nsync.lt.7) then; nbadcrc=1; return; endif
    endif

    lskipnotap=.false.
    if(.not.lapcqonly .and. nsync.lt.11) then
      nsmax=0
      do k=1,7
        s81=s8(:,k)
        include 'syncdist.f90'
        s81=s8(:,k+36)
        include 'syncdist.f90'
        s81=s8(:,k+72)
        include 'syncdist.f90'
      enddo
      if(sum(nsmax(7:8)).gt.sum(nsmax(2:3)) .or. sum(nsmax(5:6)).gt.sum(nsmax(2:3))) lskipnotap=.true.
    endif

!    if(lcqcand .and. nsync.lt.7 .and. nsync.gt.1) then
!      if(nsync+nsync2.lt.9) then
!        rscq=0.
!        do k11=8,16
!          ip=maxloc(s8(:,k11))
!          if(k11.lt.16) then; if(ip(1).eq.1) rscq=rscq+1.; else; if(ip(1).eq.2) rscq=rscq+1.; endif
!        enddo
!        ip=maxloc(s8(:,17)); if(ip(1).eq.1 .or. ip(1).eq.2) rscq=rscq+0.5
!        ip=maxloc(s8(:,27)); if(ip(1).eq.1 .or. ip(1).eq.2) rscq=rscq+0.5
!        ip=maxloc(s8(:,33)); if(ip(1).eq.3 .or. ip(1).eq.4) rscq=rscq+0.5
!        if(rscq.lt.4.9) then; nbadcrc=1; return; endif
!      endif
!    else
!      if(nsync.lt.7) then; nbadcrc=1; return; endif
!    endif
    if(nsyncscore.gt.0) then; scoreratio=scoreratio/nsyncscore; else; scoreratio=0.; endif 
    if(nsyncscore1.gt.0) then; scoreratio1=scoreratio1/nsyncscore1; else; scoreratio1=0.; endif 
    if(nsyncscore3.gt.0) then; scoreratio3=scoreratio3/nsyncscore3; else; scoreratio3=0.; endif
    if(dfqso.ge.2.0 .or. (dfqso.lt.2.0 .and. stophint)) then
      if(rrxdt.ge.-0.5 .and. rrxdt.le.2.13) then
        if(nsyncscore.lt.8 .or. (nsyncscore.lt.10 .and. scoreratio.lt.5.5) .or. (nsyncscore.lt.11 .and. &
           scoreratio.lt.3.63)) then
          nbadcrc=1; return ! 377 out of 20709
        else if(nsyncscore.eq.11 .and. scoreratio.lt.5.37) then
          if(nsyncscore1.lt.5 .and. nsyncscore3.lt.5 .and. scoreratio1.lt.4.2 .and. scoreratio3.lt.4.2) then
            nbadcrc=1; return ! 261
          endif
        else if(nsyncscore.eq.12 .and. scoreratio.lt.4.6) then
          if(nsyncscore1.lt.5 .and. nsyncscore3.lt.5 .and. scoreratio1.lt.4.0 .and. scoreratio3.lt.4.0) then
            nbadcrc=1; return ! 222
          endif
        else if(nsyncscore.eq.13 .and. scoreratio.lt.4.4) then
          if(nsyncscore1.lt.5 .and. nsyncscore2.lt.6 .and. nsyncscore3.lt.5 .and. scoreratio1.lt.4.4 .and. &
             scoreratio3.lt.4.4) then
            nbadcrc=1; return ! 98
          endif
        else if(nsyncscorew.lt.3) then
          if((nsyncscore1.gt.5 .and. scoreratio1.gt.13.8) .or. (nsyncscore2.gt.5 .and. scoreratio2.gt.13.8) .or. &
             (nsyncscore3.gt.5 .and. scoreratio3.gt.13.8)) go to 32
          nbadcrc=1; return ! 75
        else if(nsyncscorew.eq.3) then
          if(scoreratio1.gt.15. .or. scoreratio2.gt.15. .or. scoreratio3.gt.15.) go to 32
          nbadcrc=1; return ! 125
        else if(nsyncscorew.eq.4) then
          if(nsyncscore1.eq.7 .or. nsyncscore2.eq.7 .or. nsyncscore3.eq.7 .or. scoreratio1.gt.10. .or. &
             scoreratio2.gt.10. .or. scoreratio3.gt.10.) go to 32
          nbadcrc=1; return ! 94
        else if(nsyncscorew.eq.5) then
          if(nsyncscore.gt.17 .or. nsyncscore1.eq.7 .or. nsyncscore2.eq.7 .or. nsyncscore3.eq.7 .or. scoreratio1.gt.10. .or. &
             scoreratio2.gt.10. .or. scoreratio3.gt.10.) go to 32
            nbadcrc=1; return ! 131
        endif
      else if(rrxdt.lt.-0.5) then
        if(nsyncscore.lt.6 .or. (nsyncscore.gt.5 .and. nsyncscore.lt.8 .and. nsyncscorew.lt.6 .and. &
           scoreratio2.lt.5.5 .and. scoreratio3.lt.5.5)) then
          nbadcrc=1; return ! 46
        else if(nsyncscore.eq.8) then
          if(nsyncscore2.lt.6 .and. nsyncscore3.lt.6 .and. scoreratio2.lt.6.6 .and. scoreratio3.lt.6.6) then
            nbadcrc=1; return ! 20 
          endif
        else if(nsyncscore.eq.9 .and. scoreratio.lt.6.0) then
          if(nsyncscore2.lt.6 .and. nsyncscore3.lt.6 .and. scoreratio2.lt.6.6 .and. scoreratio3.lt.6.5) then
            nbadcrc=1; return ! 5 
          endif
        else if(nsyncscorew.lt.3) then
          if((nsyncscore2.gt.5 .and. scoreratio2.gt.13.8) .or. (nsyncscore3.gt.5 .and. scoreratio3.gt.13.8)) go to 32
          nbadcrc=1; return ! 22
        else if(nsyncscorew.eq.3) then
          if(scoreratio2.gt.15. .or. nsyncscore3.gt.15) go to 32
          nbadcrc=1; return ! 31
        else if(nsyncscorew.eq.4) then
          if(nsyncscore2.eq.7 .or. nsyncscore3.eq.7 .or. scoreratio2.gt.10. .or. nsyncscore3.gt.10) go to 32
          nbadcrc=1; return ! 34
        else if(nsyncscorew.eq.5) then
          if(nsyncscore.gt.11 .or. nsyncscore2.eq.7 .or. nsyncscore3.eq.7 .or. scoreratio2.gt.10. .or. &
             scoreratio3.gt.10.) go to 32
            nbadcrc=1; return ! 35
        endif
      else if(rrxdt.gt.2.13) then
        if(nsyncscore.lt.6 .or. (nsyncscore.gt.5 .and. nsyncscore.lt.8 .and. nsyncscorew.lt.6 .and. &
           scoreratio1.lt.5.5 .and. scoreratio2.lt.5.5)) then
          nbadcrc=1; return ! 4
        else if(nsyncscore.eq.8) then
          if(nsyncscore1.lt.6 .and. nsyncscore2.lt.6 .and. scoreratio1.lt.6.6 .and. scoreratio2.lt.6.6) then
            nbadcrc=1; return ! 8
          endif
        else if(nsyncscore.eq.9 .and. scoreratio.lt.6.0) then
          if(nsyncscore1.lt.6 .and. nsyncscore2.lt.6 .and. scoreratio2.lt.6.6 .and. scoreratio1.lt.6.5) then
            nbadcrc=1; return ! 2
          endif
        else if(nsyncscorew.lt.3) then
          if((nsyncscore1.gt.5 .and. scoreratio1.gt.13.8) .or. (nsyncscore2.gt.5 .and. scoreratio2.gt.13.8)) go to 32
          nbadcrc=1; return ! 12
        else if(nsyncscorew.eq.3) then
          if(scoreratio1.gt.15. .or. scoreratio2.gt.15.) go to 32
          nbadcrc=1; return ! 32
        else if(nsyncscorew.eq.4) then
          if(nsyncscore1.eq.7 .or. nsyncscore2.eq.7 .or. scoreratio1.gt.10. .or. nsyncscore2.gt.10) go to 32
          nbadcrc=1; return ! 103
        else if(nsyncscorew.eq.5) then
          if(nsyncscore.gt.11 .or. nsyncscore1.eq.7 .or. nsyncscore2.eq.7 .or. scoreratio1.gt.10. .or. &
             scoreratio2.gt.10.) go to 32
            nbadcrc=1; return ! 0
        endif
      endif
    endif

32  if(lsd) then
      if(iqso.eq.4 .and. .not.ldeepsync) go to 64
      call ft8sd1(s8,itone,msgd,msg37,lft8sd,lcq)
      if(lft8sd) then
        if(levenint) then; evencopy(isd)%lstate=.false.
        elseif(loddint) then; oddcopy(isd)%lstate=.false.
        endif
        i3=1; n3=1; iaptype=0; nbadcrc=0; lsd=.false.; go to 2
      endif
64    if(iqso.eq.4) then
        if(.not.lcq) then
          call ft8mf1(s8,itone,msgd,msg37,lft8sd)
          if(lft8sd) then
            if(levenint) then; evencopy(isd)%lstate=.false.
            elseif(loddint) then; oddcopy(isd)%lstate=.false.
            endif
            i3=1; n3=1; iaptype=0; nbadcrc=0; lsd=.false.; go to 2
          endif
        else
          call ft8mfcq(s8,itone,msgd,msg37,lft8sd)
          if(lft8sd) then
            if(levenint) then; evencopy(isd)%lstate=.false.
            elseif(loddint) then; oddcopy(isd)%lstate=.false.
            endif
            i3=1; n3=1; iaptype=0; nbadcrc=0; lsd=.false.; go to 2
          endif
        endif
      endif
    endif
    if(iqso.eq.4) then; nbadcrc=1; go to 2; endif

    synclev=0.0; snoiselev=1.0
    do k=1,7
      synclev=synclev+s8(icos7(k-1),k+36)
    enddo
    snoiselev=(sum(s8(0:7,37:43))- synclev)/7.0
    if(snoiselev.lt.0.1) snoiselev=1.0 ! safe division
    srr=synclev/snoiselev
!  SNR   srr range  average srr
! -18 1.8...4.0  2.9
! -19 1.7...3.6  2.65
! -20 1.6...3.3  2.43
! -21 1.6...3.0  2.22
! -22 1.55...2.8 2.19
! -23 1.4...2.6  2.03
! -24            1.94

8   if(iqso.gt.1 .and. iqso.lt.4) then
      if(.not.lqsomsgdcd .and. .not.(.not.lmycallstd .and. .not.lhiscallstd)) then
        if(.not.lft8sdec .and. dfqso.lt.2.0) then
          if(lvirtual2 .or. lvirtual3) srr=0.0
          call ft8s(s82,srr,itone,msg37,lft8s,nft8rxfsens,stophint)
          if(lft8s) then
            if(index(msg37,'<').gt.0) then; lhashmsg=.true.; call delbraces(msg37); endif
            nbadcrc=0; lft8sdec=.true.; lsdone=.true.; go to 2 ! i3=16 n3=16, any affect?
          endif
        endif
      endif
      lsdone=.true.; nbadcrc=1; cycle
    endif

    i1=ibest+224 ! 7*32
    csymb256=cd0(i1:i1+255)*ctwk256
    call four2a(csymb256,256,1,-1,1)
    s256(0:8)=abs(csymb256(1:9))
    rscq=0.; nmic=0
    do k11=8,16
      ip=maxloc(s8(:,k11))
      if(ip(1).eq.idtonemyc(k11-7)+1) nmic=nmic+1
      if(k11.lt.16) then
        if(ip(1).eq.1) rscq=rscq+1.
      else
        if(ip(1).eq.2) rscq=rscq+1.
      endif
    enddo
    ip=maxloc(s8(:,17)); if(ip(1).eq.1 .or. ip(1).eq.2) rscq=rscq+0.5
    ip=maxloc(s8(:,27)); if(ip(1).eq.1 .or. ip(1).eq.2) rscq=rscq+0.5
    ip=maxloc(s8(:,33)); if(ip(1).eq.3 .or. ip(1).eq.4) rscq=rscq+0.5

    lqsosig=.false.; lqsosigtype3=.false.; lqso73=.false.; lqsorr73=.false.; lqsorrr=.false. ! 73/rr73/rrr for std/nonstd callsigns
    if(.not.lqsomsgdcd .and. (dfqso.lt.napwid .or. abs(nftx-f1).lt.napwid) .and. lapmyc .and. len_trim(hiscall).gt.2) then
      nqsot=0
      do i=1,19
        ip=maxloc(s8(:,i+7))
        if(ip(1).eq.idtone56(1,i)+1) nqsot=nqsot+1
      enddo
      if(nqsot.gt.6) lqsosig=.true. ! decoding depth only
      do i=20,22
        ip=maxloc(s8(:,i+7))
        if(ip(1).eq.idtone56(1,i)+1) nqsot=nqsot+1
      enddo
      if(nqsot.gt.3) lqsosigtype3=.true.
      nqsoend=0 ! array 73,rr73,rrr
      if(dfqso.lt.napwid .and. (nQSOProgress.eq.3 .or. nQSOProgress.eq.4)) then ! QSO RX freq only
        do i=24,58
          if(i.lt.30) then; ip=maxloc(s8(:,i+7)); else; ip=maxloc(s8(:,i+14)); endif
          if(ip(1).eq.idtone56(56,i)+1) nqsoend(1)=nqsoend(1)+1
          if(ip(1).eq.idtone56(55,i)+1) nqsoend(2)=nqsoend(2)+1
          if(ip(1).eq.idtone56(54,i)+1) nqsoend(3)=nqsoend(3)+1
        enddo
        ip=maxloc(nqsoend)
        if(nqsoend(ip(1)).gt.6) then
          if(ip(1).eq.1) then; lqso73=.true.
          else if(ip(1).eq.2) then; lqsorr73=.true.
          else if(ip(1).eq.3) then; lqsorrr=.true.
          endif
        endif
      endif
    endif

    lcqsignal=.false.
    ip(1)=maxloc(s256,1)
    if(ip(1).eq.5 .or. rscq.gt.3.1) lcqsignal=.true.
    if(.not.lcqsignal .and. ip(1).eq.4 .or. ip(1).eq.6) then
      s2563(0:8)=s256(0:8); s2563(9:26)=abs(csymb256(9:26))
      ip(1)=maxloc(s2563,1)
      if(ip(1).eq.4 .or. ip(1).eq.6) lcqsignal=.true.
    endif
    lmycsignal=.false.; if(lapmyc .and. nmic.gt.2) lmycsignal=.true.

    ldxcsig=.false.; lcqdxcsig=.false.; lcqdxcnssig=.false.; ndxt=0
    if(lhiscallstd) then
      do k11=17,26
        ip=maxloc(s8(:,k11))
        if(ip(1).eq.idtone56(1,k11-7)+1) ndxt=ndxt+1
      enddo
      if(ndxt.gt.3) ldxcsig=.true.
      if(lcqsignal .and. ldxcsig) lcqdxcsig=.true.
    endif
! nonstd DXCall search in idle mode
    if(.not.lhiscallstd .and. len_trim(hiscall).gt.2) then ! nonstandard DXCall
      ncqdxcnst=0
      do i=1,4
        ip=maxloc(s8(:,i+7))
        if(ip(1).eq.idtonecqdxcns(i)+1) ncqdxcnst=ncqdxcnst+1
      enddo
      ndxt=0
      do i=5,23
        ip=maxloc(s8(:,i+7))
        if(ip(1).eq.idtonedxcns73(i)+1) ndxt=ndxt+1
        if(ip(1).eq.idtonecqdxcns(i)+1) ncqdxcnst=ncqdxcnst+1
      enddo
      ldxcsig=.false.
      if(dfqso.lt.napwid) then
        if(ndxt.gt.4) ldxcsig=.true. ! relaxed threshold for RXF napwid
        if(ncqdxcnst.gt.5) lcqdxcnssig=.true.
      else
        if(ndxt.gt.5) ldxcsig=.true.
        if(ncqdxcnst.gt.6) lcqdxcnssig=.true.
      endif
    endif

    lfoxstdr73=.false.; lfoxspecrpt=.false.
    nfoxspecrpt=0 ! checking fo special message with report to mybcall
    if(lhound) then
      nfoxstdbase=0 ! checking for report signal and base of RR73 signal
      nfoxspecr73=0 ! checking fo special message with RR73 to mybcall
      nfoxr73=0 ! checking for RR73 signal with standard message
      fdelta=abs(f1-nfqso); fdeltam=modulo(fdelta,60.)
      if(fdelta.lt.245.0 .and. fdeltam.lt.3.0 .and. (nQSOProgress.eq.1 .or. nQSOProgress.eq.3)) then ! calculate on possible Fox frequencies only
        do i=1,18
          ip=maxloc(s8(:,i+7))
          if(ip(1).eq.idtonefox73(i)+1) nfoxstdbase=nfoxstdbase+1
          if(i.gt.10 .and. ip(1).eq.idtonespec(i)+1) nfoxspecrpt=nfoxspecrpt+1
        enddo
        do i=20,22 ! hash10
          ip=maxloc(s8(:,i+7))
          if(ip(1).eq.idtonespec(i)+1) then; nfoxspecrpt=nfoxspecrpt+1; nfoxspecr73=nfoxspecr73+1; endif
        enddo
        ip=maxloc(s8(:,25+7)) ! i3,n3
        if(ip(1).eq.idtonespec(25)+1) then; nfoxspecrpt=nfoxspecrpt+1; nfoxspecr73=nfoxspecr73+1; endif
        if(nfoxstdbase.eq.0) then
          rspecstdrpt=(nfoxspecrpt*18.)/(1.2)
        else
          rspecstdrpt=(nfoxspecrpt*18.)/(12.*nfoxstdbase)
        endif
        if(rspecstdrpt.gt.1.) lfoxspecrpt=.true.
        if(nQSOProgress.eq.3) then
          do i=24,58
            if(i.lt.30) then; ip=maxloc(s8(:,i+7)); else; ip=maxloc(s8(:,i+14)); endif
            if(ip(1).eq.idtonefox73(i)+1) nfoxr73=nfoxr73+1
          enddo
          if(nfoxspecr73.eq.0) then
            rstdr73=(nfoxr73*4.)/(3.5)
          else
            rstdr73=(nfoxr73*4.)/(35.*nfoxspecr73)
          endif
          if(rstdr73.gt.1.) lfoxstdr73=.true.
        endif
      endif
    endif

    lsubptxfreq=.false.
    if(lapmyc .and. abs(f1-nftx).lt.2.0 .and. .not.lhound .and. .not.lft8sdec .and. .not.lqsomsgdcd .and. &
      ((.not.lskiptx1 .and. nlasttx.eq.1) .or. (lskiptx1 .and. nlasttx.eq.2))) lsubptxfreq=.true.

    nweak=1
    if(lft8subpass .or. swl .or. dfqso.lt.2.0 .or. lsubptxfreq) nweak=2
    nsubpasses=nweak
    if(lcqsignal) then
      nsubpasses=3
      if(levenint) then
        do ik=1,numcqsig
          if(evencq(ik,nthr)%freq.gt.5001.) exit
          if(abs(evencq(ik,nthr)%freq-f1).lt.2.0 .and. abs(evencq(ik,nthr)%xdt-xdt).lt.0.05) then
            nsubpasses=5; csold=evencq(ik,nthr)%cs
          endif
        enddo
      else if (loddint) then
        do ik=1,numcqsig
          if(oddcq(ik,nthr)%freq.gt.5001.) exit
          if(abs(oddcq(ik,nthr)%freq-f1).lt.2.0 .and. abs(oddcq(ik,nthr)%xdt-xdt).lt.0.05) then
            nsubpasses=5; csold=oddcq(ik,nthr)%cs
          endif
        enddo
      endif
    endif
    if(lmycsignal .and. lmycallstd) then
      nsubpasses=6
      if(levenint) then
        do ik=1,nummycsig
          if(evenmyc(ik,nthr)%freq.gt.5001.) exit
          if(abs(evenmyc(ik,nthr)%freq-f1).lt.2.0 .and. abs(evenmyc(ik,nthr)%xdt-xdt).lt.0.05) then
            nsubpasses=8; csold=evenmyc(ik,nthr)%cs
          endif
        enddo
      else if (loddint) then
        do ik=1,nummycsig
          if(oddmyc(ik,nthr)%freq.gt.5001.) exit
          if(abs(oddmyc(ik,nthr)%freq-f1).lt.2.0 .and. abs(oddmyc(ik,nthr)%xdt-xdt).lt.0.05) then
            nsubpasses=8; csold=oddmyc(ik,nthr)%cs
          endif
        enddo
      endif
    endif
    lqsocandave=.false.
    if(lapmyc .and. ndxt.gt.2 .and. nmic.gt.2 .and. .not.lqsomsgdcd .and. lmycallstd .and. lhiscallstd .and. &
       dfqso.lt.napwid/2.0) then
      lqsocandave=.true.
      nsubpasses=9
      if(levenint) then
          if(abs(evenqso(1,nthr)%freq-f1).lt.2.0 .and. abs(evenqso(1,nthr)%xdt-xdt).lt.0.05) then
            nsubpasses=11; csold=evenqso(1,nthr)%cs
          endif
      else if (loddint) then
          if(abs(oddqso(1,nthr)%freq-f1).lt.2.0 .and. abs(oddqso(1,nthr)%xdt-xdt).lt.0.05) then
            nsubpasses=11; csold=oddqso(1,nthr)%cs
          endif
      endif
    endif

    do isubp1=1,nsubpasses
      if(nweak.eq.1 .and. isubp1.eq.2) cycle
      if(isubp1.gt.2 .and. isubp1.lt.6 .and. lmycsignal) cycle ! skip if it is lmycsignal, can be both lcq and lmy
      syncavemax=3.
      if(isubp1.eq.2) cs=csr
      if(ipass.eq.npass-1 .and. (lcqsignal .or. lmycsignal) .and. &
        ((nweak.eq.1 .and. isubp1.eq.1) .or. (nweak.eq.2 .and. isubp1.eq.2))) cstmp2=cs
      if(ipass.eq.npass .and. lapmyc .and. ndxt.gt.2 .and. nmic.gt.2 .and. &
        ((nweak.eq.1 .and. isubp1.eq.1) .or. (nweak.eq.2 .and. isubp1.eq.2))) cstmp2=cs
      do nsym=1,3
        nt=2**(3*nsym)-1
        do ihalf=1,2
          do k=1,29,nsym
            if(ihalf.eq.1) then; ks=k+7
            else; ks=k+43
            endif
            ks1=ks+1; ks2=ks+2
            do i=0,nt
              i1=i/64
              i2=iand(i,63)/8
              i33=iand(i,7)
              if(isubp1.lt.3) then
                if(nsym.eq.1) then
                  s2(i)=abs(cs(graymap(i33),ks))
                elseif(nsym.eq.2) then
                  s2(i)=abs(cs(graymap(i2),ks)+cs(graymap(i33),ks1))
                else
                  s2(i)=abs(cs(graymap(i1),ks)+cs(graymap(i2),ks1)+cs(graymap(i33),ks2))
                endif
              else if(isubp1.eq.3 .or. isubp1.eq.6 .or. isubp1.eq.9) then
                if(nsym.eq.1) then
                  s2(i)=abs(cscs(graymap(i33),ks))**2+abs(csr(graymap(i33),ks))**2
                elseif(nsym.eq.2) then
                  s2(i)=abs(cscs(graymap(i2),ks)+cscs(graymap(i33),ks1))**2+abs(csr(graymap(i2),ks)+csr(graymap(i33),ks1))**2
                else
                  s2(i)=abs(cscs(graymap(i1),ks)+cscs(graymap(i2),ks1)+cscs(graymap(i33),ks2))**2 + &
                    abs(csr(graymap(i1),ks)+csr(graymap(i2),ks1)+csr(graymap(i33),ks2))**2
                endif
              else if(isubp1.eq.4 .or. isubp1.eq.7 .or. isubp1.eq.10) then
                if(nsym.eq.1) then
                  s2(i)=abs(cs(graymap(i33),ks))**2+abs(csold(graymap(i33),ks))**2
                elseif(nsym.eq.2) then
                  s2(i)=abs(cs(graymap(i2),ks)+cs(graymap(i33),ks1))**2+abs(csold(graymap(i2),ks)+csold(graymap(i33),ks1))**2
                else
                  s2(i)=abs(cs(graymap(i1),ks)+cs(graymap(i2),ks1)+cs(graymap(i33),ks2))**2 + &
                    abs(csold(graymap(i1),ks)+csold(graymap(i2),ks1)+csold(graymap(i33),ks2))**2
                endif
              else if(isubp1.eq.5 .or. isubp1.eq.8 .or. isubp1.eq.11) then
                if(nsym.eq.1) then
                  s2(i)=abs(cs(graymap(i33),ks))+abs(csold(graymap(i33),ks))
                elseif(nsym.eq.2) then
                  s2(i)=abs(cs(graymap(i2),ks)+cs(graymap(i33),ks1))+abs(csold(graymap(i2),ks)+csold(graymap(i33),ks1))
                else
                  s2(i)=abs(cs(graymap(i1),ks)+cs(graymap(i2),ks1)+cs(graymap(i33),ks2)) + &
                    abs(csold(graymap(i1),ks)+csold(graymap(i2),ks1)+csold(graymap(i33),ks2))
                endif
              endif
              if(isubp1.eq.1 .and. srr.lt.2.5) then !  srr.lt.2.5 -19dB SNR threshold
                if(srr.gt.2.3) then 
                  s2(i)=s2(i)**2
                else
                  ss1=s2(i)
                  if(ss1.lt.5.77) then; s2(i)=1+8.*ss1**2-0.12*ss1**4; else; s2(i)=(ss1+5.82)**2; endif
                endif
              endif
              if(isubp1.gt.1 .and. srr.lt.2.5) s2(i)=(0.5*s2(i))**3 ! -19dB SNR threshold
            enddo
            i32=1+(k-1)*3+(ihalf-1)*87
            if(nsym.eq.1) ibmax=2; if(nsym.eq.2) ibmax=5; if(nsym.eq.3) ibmax=8
            do ib=0,ibmax
              bm=maxval(s2(0:nt),one(0:nt,ibmax-ib)) - maxval(s2(0:nt),.not.one(0:nt,ibmax-ib))
              if(i32+ib .gt.174) cycle
              if(nsym.eq.1) then
                bmeta(i32+ib)=bm
                den=max(maxval(s2(0:nt),one(0:nt,ibmax-ib)),maxval(s2(0:nt),.not.one(0:nt,ibmax-ib)))
                if(den.gt.0.0) then; cm=bm/den; else; cm=0.0; endif
                bmetd(i32+ib)=cm
              elseif(nsym.eq.2) then
                bmetb(i32+ib)=bm
              elseif(nsym.eq.3) then
                bmetc(i32+ib)=bm
              endif
            enddo
          enddo ! k
        enddo ! ihalf
      enddo ! nsym

!tests
!call indexx(bmetc(1:174),174,indx)
!src=abs(bmetc(indx(1))/bmetc(indx(174)))
!print *,src
      call normalizebmet(bmeta,174);call normalizebmet(bmetb,174);call normalizebmet(bmetc,174);call normalizebmet(bmetd,174)
      scalefac=2.83; llra=scalefac*bmeta; llrb=scalefac*bmetb; llrc=scalefac*bmetc; llrd=scalefac*bmetd
      apmag=maxval(abs(llra))*1.01

! isubp2 #
!------------------------------
!   1        regular decoding, nsym=1 
!   2        regular decoding, nsym=2 
!   3        regular decoding, nsym=3 
!   4        regular decoding, llrd
!   5..18    ap passes

! iaptype Hound OFF, MyCall is standard, DXCall is standard or empty
!------------------------
!   0        cycle
!   1        CQ     ???    ???           (29+3=32 ap bits)
!   2        MyCall ???    ???           (29+3=32 ap bits)
!   3        MyCall DxCall ???           (58+3=61 ap bits)
!   4        MyCall DxCall RRR           (77 ap bits)
!   5        MyCall DxCall 73            (77 ap bits)
!   6        MyCall DxCall RR73          (77 ap bits)

! naptypes(nQSOProgress, extra decoding pass)
!  data naptypes(0,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,1,1,1,31,31,31,36,36,36,35,35,35/ ! Tx6 CQ
!  data naptypes(1,1:27)/3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/ ! Tx1 Grid
!  data naptypes(2,1:27)/3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/ ! Tx2 Report
!  data naptypes(3,1:27)/3,3,3,6,6,6,5,5,5,4,4,4,0,0,0,0,0,0,31,31,31,36,36,36,35,35,35/ ! Tx3 RRreport
!  data naptypes(4,1:27)/3,3,3,6,6,6,5,5,5,4,4,4,2,2,2,0,0,0,31,31,31,36,36,36,35,35,35/ ! Tx4 RRR,RR73
!  data naptypes(5,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,1,1,1,31,31,31,36,36,36,35,35,35/ ! Tx5 73

! iaptype standard DxCall tracking, also valid in Hound mode
!------------------------
!   31        CQ  DxCall Grid(???)     (77 ap bits)
!   35        ??? DxCall 73            (29+19 ap bits)
!   36        ??? DxCall RR73          (29+19 ap bits)

! iaptype Hound off, MyCall is nonstandard, DXCall is standard or empty
!------------------------
!   0         cycle
!   1         CQ     ???    ???        (29+3=32 ap bits)
!   40       <MyCall> ???  ???         (29+3=32 ap bits) incoming call
!   41       <MyCall> DxCall ???       (58 ap bits) REPORT/RREPORT
!   42        MyCall <DxCall> RRR      (77 ap bits)
!   43        MyCall <DxCall> 73       (77 ap bits)
!   44        MyCall <DxCall> RR73     (77 ap bits)
!   31        CQ  DxCall Grid(???)     (77 ap bits) standard DxCall tracking
!   35        ??? DxCall 73            (29+19 ap bits) standard DxCall tracking
!   36        ??? DxCall RR73          (29+19 ap bits) standard DxCall tracking

! nmycnsaptypes(nQSOProgress, extra decoding pass)
!  data nmycnsaptypes(0,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx6 CQ
!  data nmycnsaptypes(1,1:27)/41,41,41,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx1 DXcall MyCall
!  data nmycnsaptypes(2,1:27)/41,41,41,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx2 Report
!  data nmycnsaptypes(3,1:27)/41,41,41,44,44,44,43,43,43,42,42,42,0,0,0,0,0,0,31,31,31,36,36,36,35,35,35/    ! Tx3 RRreport
!  data nmycnsaptypes(4,1:27)/41,41,41,44,44,44,43,43,43,42,42,42,40,40,40,0,0,0,31,31,31,36,36,36,35,35,35/ ! Tx4 RRR,RR73
!  data nmycnsaptypes(5,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx5 73

! iaptype Hound off, MyCall is standard, DXCall is not empty and is nonstandard
!------------------------
!   0         cycle
!   1         CQ     ???    ???            (29+3=32 ap bits)
!   2         MyCall ???    ???            (29+3=32 ap bits)
!   11        MyCall <DxCall> ???          (58 ap bits) REPORT/RREPORT
!   12       <MyCall> DxCall RRR           (77 ap bits)
!   13       <MyCall> DxCall 73            (77 ap bits)
!   14       <MyCall> DxCall RR73          (77 ap bits)
!   31        CQ  DxCall                   (77 ap bits) ! full compound or just nonstandard callsign
!   35        ??? DxCall 73                (64 ap bits) ! full compound or just nonstandard callsign
!   36        ??? DxCall RR73              (64 ap bits) ! full compound or just nonstandard callsign

! ndxnsaptypes(nQSOProgress, extra decoding pass)
!  data ndxnsaptypes(0,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx6 CQ
!  data ndxnsaptypes(1,1:27)/11,11,11,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/          ! Tx1 Grid
!  data ndxnsaptypes(2,1:27)/11,11,11,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,31,31,31,36,36,36,35,35,35/          ! Tx2 Report
!  data ndxnsaptypes(3,1:27)/11,11,11,14,14,14,13,13,13,12,12,12,0,0,0,0,0,0,31,31,31,36,36,36,35,35,35/ ! Tx3 RRreport
!  data ndxnsaptypes(4,1:27)/11,11,11,14,14,14,13,13,13,12,12,12,2,2,2,0,0,0,31,31,31,36,36,36,35,35,35/ ! Tx4 RRR,RR73
!  data ndxnsaptypes(5,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,1,1,1,31,31,31,36,36,36,35,35,35/             ! Tx5 73

! iaptype Hound mode
!------------------------
!    0        cycle
!    1        CQ     ???    ???            (29+3=32 ap bits) standard callsign
!   21        MyBaseCall DxBaseCall ???    (58+3=61 ap bits) Report
!   22        ??? RR73; MyCall <???> ???   (28+6=34 ap bits)
!   23        MyBaseCall DxBaseCall RR73   (77 ap bits)
!   24        MyCall RR73; ??? <???> ???   (28+6=34 ap bits)
!   31        CQ  DxCall (DXGrid)          (77 ap bits) ! standard/full compound or just nonstandard callsign
!   36        ??? DxCall RR73              (29+19/64 ap bits) ! standard/ full compound or just nonstandard callsign
!  111        CQ     ???                   (3 last i3 bits) type 4 message with nonstandard callsign

! nhaptypes(nQSOProgress, extra decoding pass)
!  data nhaptypes(0,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,111,111,111/       ! Tx6 CQ, possible in idle mode if DXCall is empty
!  data nhaptypes(1,1:27)/21,21,21,22,22,22,0,0,0,0,0,0,0,0,0,31,31,31,0,0,0,36,36,36,0,0,0/ ! Tx1 Grid idle mode or transmitting
!  data nhaptypes(2,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/             ! Tx2 none
!  data nhaptypes(3,1:27)/21,21,21,22,22,22,23,23,23,24,24,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/ ! Tx3 RRreport QSO in progress or QSO is finished
!  data nhaptypes(4,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/             ! Tx4 none
!  data nhaptypes(5,1:27)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/             ! Tx5 none

      loutapwid=.false.; loutapwid=abs(f1-nfqso).gt.napwid .and. abs(f1-nftx).gt.napwid
      scqnr=2.; smycnr=2.

      do isubp2=1,31
        if(isubp2.lt.5) then
          if(lapcqonly .or. lskipnotap) cycle
          if(ltxing) then ! enabled Tx and transmitted message including CQ at last interval
            if(abs(f1-nfqso).lt.3.0) then ! +- 3Hz sync8 QSOfreq candidate list
              if(syncavemax.lt.1.8) cycle
            else
              if(syncavemax.lt.1.9) cycle
            endif
          else
            if(syncavemax.lt.1.8) cycle
          endif
        endif
        if(.not.swl .and. isubp2.eq.4) cycle
        if(isubp1.gt.2 .and. isubp2.lt.5) cycle ! skip regular decoding for extra subpasses
        if(lqsocandave) then
          if(isubp1.gt.2 .and. isubp1.lt.9) cycle ! skip other extra subpasses if QSO signal, highest priority
          if(lqsomsgdcd) cycle
        else if(lmycsignal .and. lmycallstd) then
          if(isubp1.gt.2 .and. isubp1.lt.6) cycle ! skip CQ signal extra subpasses if MyCall signal
        endif

        if(isubp2.lt.5) then
          apmask=0; iaptype=0
          if(isubp2.eq.1) then
            if(.not.swl .and. ipass.eq.1) then; llrz=llrd; else; llrz=llra; endif
            if(isubp1.gt.1 .and. ipass.gt.1) llrz=llrd
          else if(isubp2.eq.2) then; llrz=llrb; if(isubp1.gt.1) llrz=llra
          else if(isubp2.eq.3) then; llrz=llrc
          else if(isubp2.eq.4) then; llrz=llrd
          endif
        else
          if(.not.lhound) then
            if(lmycallstd .and. (lhiscallstd .or. lnohiscall)) then
              iaptype=naptypes(nQSOProgress,isubp2-4); if(iaptype.eq.0) cycle
              if(lqsomsgdcd .and. iaptype.gt.2 .and. iaptype.lt.31) cycle ! QSO message already decoded
              if(iaptype.eq.2) then
                if(.not.lapmyc .or. lapcqonly) cycle ! skip AP for 'mycall ???? ????' in 2..3 minutes after last TX
                if(nQSOProgress.ne.0 .and. nmic.lt.2) cycle ! reduce CPU load at QSO
              endif
              if(.not.stophint .and. iaptype.gt.30) cycle ! no DXCall searching at QSO, reduce Lag
              if(stophint .and. iaptype.gt.2 .and. iaptype.lt.31) cycle
              if(lft8sdec .and. iaptype.gt.2) cycle ! QSO message already decoded
              if(iaptype.gt.2 .and. lnohiscall) cycle ! no DXCall
              if(iaptype.gt.2 .and. iaptype.lt.31 .and. loutapwid) cycle
              if(iaptype.eq.3 .and. .not.lqsosigtype3) cycle ! not QSO signal
              if(iaptype.eq.4 .and. .not.lqsorrr) cycle ! not RRR signal
              if(iaptype.eq.5 .and. .not.lqso73) cycle ! not 73 signal
              if(iaptype.eq.6 .and. .not.lqsorr73) cycle ! not RR73 signal
              if(iaptype.gt.30 .and. .not.lenabledxcsearch) cycle ! in QSO or TXing CQ or last logged is DX Call: searching disabled
              if(iaptype.gt.30 .and. .not.lwidedxcsearch .and. loutapwid) cycle ! only RX freq DX Call searching
              if(iaptype.eq.31 .and. .not.lcqdxcsig) cycle ! not CQ signal from std DXCall
              if(iaptype.eq.31 .and. .not.lhiscallstd .and. lapcqonly) cycle ! skip weak CQ signals with std call
              if(iaptype.gt.31 .and. lapcqonly) cycle ! skip weak CQ signals
              if(iaptype.eq.35 .and. .not.lqso73) cycle ! DXCall searching, not 73 signal
              if(iaptype.eq.36 .and. .not.lqsorr73) cycle ! DXCall searching, not RR73 signal
              if(lqsocandave .and. isubp1.gt.8 .and. (iaptype.lt.3 .or. iaptype.gt.6)) cycle ! QSO signal
              if(.not.lqsocandave .and. lmycsignal .and. isubp1.gt.5 .and. isubp1.lt.9 .and. iaptype.ne.2) cycle ! skip other AP if lmycsignal extra subpasses)

              if(iaptype.eq.1) then
                if(.not.swl .and. isubp2.eq.20) then
                  scqlev=0.; do i4=1,9; scqlev=scqlev+s8(idtone25(2,i4),i4+7); enddo
                  snoiselev=(sum(s8(0:7,8:16))-scqlev)/7.0; scqnr=scqlev/snoiselev
                  if(scqnr.lt.1.0 .and. .not.lcqsignal) cycle
                endif
                if(isubp2.eq.21) then
                  if(.not.swl .and. (lft8lowth .or. lft8subpass)) then
                    if(scqnr.lt.1.2 .and. .not.lcqsignal) cycle
                  endif
                  if(.not.swl .and. .not. lft8subpass .and. .not.lft8lowth) then
                    if(scqnr.lt.1.3 .and. .not.lcqsignal) cycle
                  endif
                endif
              endif

              if(iaptype.eq.2) then
                if(.not.swl .and. isubp2.eq.17) then
                  smyclev=0.; do i4=1,9; smyclev=smyclev+s8(idtonemyc(i4),i4+7); enddo
                  snoiselev=(sum(s8(0:7,8:16))-smyclev)/7.0; smycnr=smyclev/snoiselev
                  if(smycnr.lt.1.0 .and. .not.lmycsignal) cycle
                endif
                if(isubp2.eq.18) then
                  if(.not.swl .and. (lft8lowth .or. lft8subpass)) then
                    if(smycnr.lt.1.2 .and. .not.lmycsignal) cycle
                  endif
                endif
              endif

              if(iaptype.eq.3) then
                if(isubp2.eq.5) then
                  smyclev=0.; do i4=1,9; smyclev=smyclev+s8(idtonemyc(i4),i4+7); enddo
                  snoiselev=(sum(s8(0:7,8:16))-smyclev)/7.0; smycnr=smyclev/snoiselev
                  if(smycnr.lt.1.0) cycle
                else if(isubp2.eq.6) then
                  if(smycnr.lt.1.2) cycle
                endif
              endif

              apmask=0
              if(iaptype.eq.1) then ! CQ
                if(isubp2.eq.20) then; llrz=llrc
                else if(isubp2.eq.21) then; llrz=llrb
                else if(isubp2.eq.22) then; llrz=llra
                endif
                apmask(1:29)=1; llrz(1:29)=apmag*mcq(1:29); apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
              else if(iaptype.eq.2) then ! MyCall ??? ???
                if(isubp2.eq.17) then; llrz=llrc
                else if(isubp2.eq.18) then; llrz=llrb
                else if(isubp2.eq.19) then; llrz=llra
                endif
                apmask(1:29)=1; llrz(1:29)=apmag*apsym(1:29); apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
              else if(iaptype.eq.3) then ! MyCall DxCall ???
                if(isubp2.eq.5) then; llrz=llrc
                else if(isubp2.eq.6) then; llrz=llrb
                else if(isubp2.eq.7) then; llrz=llra
                endif
                apmask(1:58)=1; llrz(1:58)=apmag*apsym; apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
              else if(iaptype.gt.3 .and. iaptype.lt.7) then
                if(isubp2.eq.8 .or. isubp2.eq.11 .or. isubp2.eq.14) then; llrz=llrc
                else if(isubp2.eq.9 .or. isubp2.eq.12 .or. isubp2.eq.15) then; llrz=llrb
                else if(isubp2.eq.10 .or. isubp2.eq.13 .or. isubp2.eq.16) then; llrz=llrb
                endif
                apmask(1:77)=1; llrz(1:58)=apmag*apsym ! mycall hiscall 73|RR73
                if(iaptype.eq.5) then; llrz(59:77)=apmag*m73
                else if(iaptype.eq.6) then; llrz(59:77)=apmag*mrr73
                else if(iaptype.eq.4) then; llrz(59:77)=apmag*mrrr
                endif
              else if(iaptype.eq.31) then ! CQ DxCall Grid(???)
                if(isubp2.eq.23) then; llrz=llrc
                else if(isubp2.eq.24) then; llrz=llrb
                else if(isubp2.eq.25) then; llrz=llra
                endif
                if(lnohisgrid) then
                  apmask(1:58)=1; llrz(1:58)=apmag*apcqsym(1:58); apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
                else
                  apmask(1:77)=1; llrz(1:77)=apmag*apcqsym
                endif
              else if(iaptype.eq.35) then ! ??? DxCall 73, type 1 std DXCall
                if(isubp2.eq.29) then; llrz=llrc
                else if(isubp2.eq.30) then; llrz=llrb
                else if(isubp2.eq.31) then; llrz=llra
                endif
                apmask(30:77)=1; llrz(30:58)=apmag*apsym(30:58); llrz(59:77)=apmag*m73
              else if(iaptype.eq.36) then ! ??? DxCall RR73 type 1 std DXCall
                if(isubp2.eq.26) then; llrz=llrc
                else if(isubp2.eq.27) then; llrz=llrb
                else if(isubp2.eq.28) then; llrz=llra
                endif
                apmask(30:77)=1; llrz(30:58)=apmag*apsym(30:58); llrz(59:77)=apmag*mrr73
              endif

            else if(lmycallstd .and. .not.lhiscallstd .and. len_trim(hiscall).gt.2) then
              iaptype=ndxnsaptypes(nQSOProgress,isubp2-4); if(iaptype.eq.0) cycle
              if(iaptype.eq.2 .and. lapcqonly) cycle ! skip weak CQ signals
              if(.not.stophint .and. iaptype.gt.30) cycle ! no DXCall searching at QSO, reduce Lag
              if((lqsomsgdcd .or. .not.lapmyc) .and. iaptype.gt.1 .and. iaptype.lt.15) cycle ! skip AP for mycall in 2..3 minutes after last TX
              if(iaptype.eq.12 .and. .not.lqsorrr) cycle ! not RRR signal
              if(iaptype.eq.13 .and. .not.lqso73) cycle ! not 73 signal
              if(iaptype.eq.14 .and. .not.lqsorr73) cycle ! not RR73 signal
              if(iaptype.gt.30 .and. .not.lenabledxcsearch) cycle ! in QSO or TXing CQ or last logged is DX Call: searching disabled
              if(iaptype.gt.30 .and. .not.lwidedxcsearch .and. loutapwid) cycle ! only RX freq DX Call searching
              if(iaptype.gt.30 .and. lapcqonly) cycle ! skip weak CQ signals
              if(iaptype.eq.31 .and. .not.lcqdxcnssig) cycle ! it is not CQ signal of non-standard DXCall
              if(iaptype.eq.35 .and. .not.lqso73) cycle ! DXCall searching, not 73 signal
              if(iaptype.eq.36 .and. .not.lqsorr73) cycle ! DXCall searching, not RR73 signal
              if(lqsocandave .and. isubp1.gt.8 .and. (iaptype.lt.11 .or. iaptype.gt.14)) cycle ! QSO signal
              if(iaptype.gt.2 .and. iaptype.lt.15 .and. loutapwid) cycle

              if(iaptype.eq.1) then
                if(.not.swl .and. isubp2.eq.20) then
                  scqlev=0.; do i4=1,9; scqlev=scqlev+s8(idtone25(2,i4),i4+7); enddo
                  snoiselev=(sum(s8(0:7,8:16))-scqlev)/7.0; scqnr=scqlev/snoiselev
                  if(scqnr.lt.1.0 .and. .not.lcqsignal) cycle
                endif
                if(isubp2.eq.21) then
                  if(.not.swl .and. (lft8lowth .or. lft8subpass)) then
                    if(scqnr.lt.1.2 .and. .not.lcqsignal) cycle
                  endif
                  if(.not.swl .and. .not. lft8subpass .and. .not.lft8lowth) then
                    if(scqnr.lt.1.3 .and. .not.lcqsignal) cycle
                  endif
                endif
              endif

              if(iaptype.eq.2) then
                if(.not.swl .and. isubp2.eq.17) then
                  smyclev=0.; do i4=1,9; smyclev=smyclev+s8(idtonemyc(i4),i4+7); enddo
                  snoiselev=(sum(s8(0:7,8:16))-smyclev)/7.0; smycnr=smyclev/snoiselev
                  if(smycnr.lt.1.0 .and. .not.lmycsignal) cycle
                endif
                if(isubp2.eq.18) then
                  if(.not.swl .and. (lft8lowth .or. lft8subpass)) then
                    if(smycnr.lt.1.2 .and. .not.lmycsignal) cycle
                  endif
                endif
              endif

              apmask=0
              if(iaptype.eq.1) then ! CQ ??? ???
                if(isubp2.eq.20) then; llrz=llrc
                else if(isubp2.eq.21) then; llrz=llrb
                else if(isubp2.eq.22) then; llrz=llra
                endif
                apmask(1:29)=1; llrz(1:29)=apmag*mcq(1:29); apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
              else if(iaptype.eq.2) then ! MyCall ??? ???
                if(isubp2.eq.17) then; llrz=llrc
                else if(isubp2.eq.18) then; llrz=llrb
                else if(isubp2.eq.19) then; llrz=llra
                endif
                apmask(1:29)=1; llrz(1:29)=apmag*apsym(1:29); apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
              else if(iaptype.eq.11) then ! MyCall <DxCall> ???  ! report rreport type 1 msg
                if(isubp2.eq.5) then; llrz=llrc
                else if(isubp2.eq.6) then; llrz=llrb
                else if(isubp2.eq.7) then; llrz=llra
                endif
                apmask(1:58)=1; llrz(1:58)=apmag*apsymdxns1; apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
              else if(iaptype.eq.12) then  ! type 4 <MyCall> DxCall RRR
                if(isubp2.eq.14) then; llrz=llrc
                else if(isubp2.eq.15) then; llrz=llrb
                else if(isubp2.eq.16) then; llrz=llra
                endif
                apmask(1:77)=1; llrz(1:77)=apmag*apsymdxnsrrr
              else if(iaptype.eq.13) then  ! type 4 <MyCall> DxCall 73
                if(isubp2.eq.11) then; llrz=llrc
                else if(isubp2.eq.12) then; llrz=llrb
                else if(isubp2.eq.13) then; llrz=llra
                endif
                apmask(1:77)=1; llrz(1:77)=apmag*apsymdxns732
              else if(iaptype.eq.14) then  ! type 4 <MyCall> DxCall RR73
                if(isubp2.eq.8) then; llrz=llrc
                else if(isubp2.eq.9) then; llrz=llrb
                else if(isubp2.eq.10) then; llrz=llra
                endif
                apmask(1:77)=1; llrz(1:77)=apmag*apsymdxnsr73
              else if(iaptype.eq.31) then ! CQ  DxCall ! full compound or nonstandard
                if(isubp2.eq.23) then; llrz=llrc
                else if(isubp2.eq.24) then; llrz=llrb
                else if(isubp2.eq.25) then; llrz=llra
                endif
                apmask(1:77)=1; llrz(1:77)=apmag*apcqsym
              else if(iaptype.eq.35) then ! ??? DxCall 73 ! full compound or nonstandard
                if(isubp2.eq.29) then; llrz=llrc
                else if(isubp2.eq.30) then; llrz=llrb
                else if(isubp2.eq.31) then; llrz=llra
                endif
                apmask(14:77)=1; llrz(14:77)=apmag*apsymdxns73(14:77)
              else if(iaptype.eq.36) then ! ??? DxCall RR73 ! full compound or nonstandard
                if(isubp2.eq.26) then; llrz=llrc
                else if(isubp2.eq.27) then; llrz=llrb
                else if(isubp2.eq.28) then; llrz=llra
                endif
                apmask(14:77)=1; llrz(14:77)=apmag*apsymdxnsrr73(14:77)
              endif

            else if(.not.lmycallstd .and. .not.lhiscallstd .and. len_trim(hiscall).gt.2) then ! empty mycall or compound/nonstandard both calls
              iaptype=ndxnsaptypes(nQSOProgress,isubp2-4); if(iaptype.eq.0) cycle
              if(iaptype.gt.1 .and. iaptype.lt.31) cycle
              if(.not.stophint .and. iaptype.gt.1) cycle ! on air, QSO is not possible
              if(iaptype.gt.30 .and. lapcqonly) cycle ! skip weak CQ signals with std call
              if(iaptype.eq.31 .and. .not.lcqdxcnssig) cycle ! it is not CQ signal of non-standard DXCall
              if(iaptype.gt.34 .and. .not.ldxcsig) cycle ! not DXCall signal

              if(iaptype.eq.1) then
                if(.not.swl .and. isubp2.eq.20) then
                  scqlev=0.; do i4=1,9; scqlev=scqlev+s8(idtone25(2,i4),i4+7); enddo
                  snoiselev=(sum(s8(0:7,8:16))-scqlev)/7.0; scqnr=scqlev/snoiselev
                  if(scqnr.lt.1.0 .and. .not.lcqsignal) cycle
                endif
                if(isubp2.eq.21) then
                  if(.not.swl .and. (lft8lowth .or. lft8subpass)) then
                    if(scqnr.lt.1.2 .and. .not.lcqsignal) cycle
                  endif
                  if(.not.swl .and. .not. lft8subpass .and. .not.lft8lowth) then
                    if(scqnr.lt.1.3 .and. .not.lcqsignal) cycle
                  endif
                endif
              endif

              apmask=0
              if(iaptype.eq.1) then ! CQ ??? ???
                if(isubp2.eq.20) then; llrz=llrc
                else if(isubp2.eq.21) then; llrz=llrb
                else if(isubp2.eq.22) then; llrz=llra
                endif
                apmask(1:29)=1; llrz(1:29)=apmag*mcq(1:29); apmask(75:77)=1; llrz(75:76)=apmag*(-1)
                llrz(77)=apmag*(+1)
! nonstandard DXCall search masks below are for monitoring purpose in idle mode, QSO is not possible
! wideband searching being used by default
              else if(iaptype.eq.31) then ! CQ  DxCall ! full compound or nonstandard
                if(isubp2.eq.23) then; llrz=llrc
                else if(isubp2.eq.24) then; llrz=llrb
                else if(isubp2.eq.25) then; llrz=llra
                endif
                apmask(1:77)=1; llrz(1:77)=apmag*apcqsym
              else if(iaptype.eq.35) then ! ??? DxCall 73 ! full compound or nonstandard
                if(isubp2.eq.29) then; llrz=llrc
                else if(isubp2.eq.30) then; llrz=llrb
                else if(isubp2.eq.31) then; llrz=llra
                endif
                apmask(14:77)=1; llrz(14:77)=apmag*apsymdxns73(14:77)
              else if(iaptype.eq.36) then ! ??? DxCall RR73 ! full compound or nonstandard
                if(isubp2.eq.26) then; llrz=llrc
                else if(isubp2.eq.27) then; llrz=llrb
                else if(isubp2.eq.28) then; llrz=llra
                endif
                apmask(14:77)=1; llrz(14:77)=apmag*apsymdxnsrr73(14:77)
              endif

            else if(.not.lmycallstd .and. (lhiscallstd .or. lnohiscall)) then
              iaptype=nmycnsaptypes(nQSOProgress,isubp2-4); if(iaptype.eq.0) cycle
              if(isubp1.eq.2 .and. nweak.eq.1) cycle
              if(isubp1.gt.5) cycle ! so far CQ averaging only
              if(iaptype.eq.40 .and. lapcqonly) cycle ! skip weak CQ signals
              if(iaptype.gt.40 .and. iaptype.lt.45 .and. lqsomsgdcd) cycle ! already decoded
              if(iaptype.eq.42 .and. .not.lqsorrr) cycle ! not RRR signal
              if(iaptype.eq.43 .and. .not.lqso73) cycle ! not 73 signal
              if(iaptype.eq.44 .and. .not.lqsorr73) cycle ! not RR73 signal
              if(iaptype.gt.39 .and. .not.lapmyc) cycle
              if(lnomycall .and. iaptype.gt.39 .and. iaptype.lt.45) cycle ! skip QSO signals if mycall is empty
              if(lnohiscall .and. iaptype.ne.1 .and. iaptype.ne.40) cycle ! skip DXCall masks if empty
              if(iaptype.gt.30 .and. iaptype.lt.40 .and. .not.stophint) cycle ! in QSO, reduce number of CPU cycles
              if(iaptype.eq.31 .and. .not.lcqdxcsig) cycle ! not CQ signal from std DXCall
              if(iaptype.gt.34 .and. iaptype.lt.37 .and. (.not.ldxcsig .or. lapcqonly)) cycle ! not DXCall signal, skip weak CQ signals
              if(iaptype.gt.30 .and. iaptype.lt.40 .and. .not.lwidedxcsearch .and. loutapwid) cycle ! if wideband DX search disabled

              apmask=0
              if(lcqsignal .and. iaptype.eq.1) then ! CQ ??? ???
                if(isubp2.eq.20) then; llrz=llrc
                else if(isubp2.eq.21) then; llrz=llrb
                else if(isubp2.eq.22) then; llrz=llra
                endif
                apmask(1:29)=1; llrz(1:29)=apmag*mcq(1:29); apmask(75:77)=1; llrz(75:76)=apmag*(-1)
                llrz(77)=apmag*(+1)
              else if(iaptype.eq.31) then ! CQ DxCall Grid(???) //std DX call type1 with or without grid
                if(isubp2.eq.23) then; llrz=llrc
                else if(isubp2.eq.24) then; llrz=llrb
                else if(isubp2.eq.25) then; llrz=llra
                endif
                if(lnohisgrid) then
                  apmask(1:58)=1; llrz(1:58)=apmag*apcqsym(1:58); apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
                else
                  apmask(1:77)=1; llrz(1:77)=apmag*apcqsym
                endif
              else if(iaptype.eq.35) then ! ??? DxCall 73 //std DX call
                if(isubp2.eq.29) then; llrz=llrc
                else if(isubp2.eq.30) then; llrz=llrb
                else if(isubp2.eq.31) then; llrz=llra
                endif
                apmask(30:77)=1; llrz(30:58)=apmag*apsymdxstd(30:58); llrz(59:77)=apmag*m73
              else if(iaptype.eq.36) then ! ??? DxCall RR73 //std DX call
                if(isubp2.eq.26) then; llrz=llrc
                else if(isubp2.eq.27) then; llrz=llrb
                else if(isubp2.eq.28) then; llrz=llra
                endif
                apmask(30:77)=1; llrz(30:58)=apmag*apsymdxstd(30:58); llrz(59:77)=apmag*mrr73
              else if(iaptype.eq.40) then ! <MyCall>,???,??? type 1
                if(isubp2.eq.17) then; llrz=llrc
                else if(isubp2.eq.18) then; llrz=llrb
                else if(isubp2.eq.19) then; llrz=llra
                endif
                apmask(1:29)=1; llrz(1:29)=apmag*apsymmyns1(1:29); apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
              else if(iaptype.eq.41) then ! <MyCall>,DXCall,??? type 1
                if(isubp2.eq.5) then; llrz=llrc
                else if(isubp2.eq.6) then; llrz=llrb
                else if(isubp2.eq.7) then; llrz=llra
                endif
                apmask(1:58)=1; llrz(1:58)=apmag*apsymmyns2; apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
              else if(iaptype.eq.42) then ! MyCall,<DXCall>,RRR type 4
                if(isubp2.eq.14) then; llrz=llrc
                else if(isubp2.eq.15) then; llrz=llrb
                else if(isubp2.eq.16) then; llrz=llra
                endif
                apmask(1:77)=1; llrz(1:77)=apmag*apsymmynsrrr
              else if(iaptype.eq.43) then ! MyCall,<DXCall>,73 type 4
                if(isubp2.eq.11) then; llrz=llrc
                else if(isubp2.eq.12) then; llrz=llrb
                else if(isubp2.eq.13) then; llrz=llra
                endif
                apmask(1:77)=1; llrz(1:77)=apmag*apsymmyns73
              else if(iaptype.eq.44) then ! MyCall,<DXCall>,RR73 type 4
                if(isubp2.eq.8) then; llrz=llrc
                else if(isubp2.eq.9) then; llrz=llrb
                else if(isubp2.eq.10) then; llrz=llra
                endif
                apmask(1:77)=1; llrz(1:77)=apmag*apsymmynsrr73
              endif
            else; cycle ! fallback
            endif

          else if(lhound) then
            iaptype=nhaptypes(nQSOProgress,isubp2-4); if(iaptype.eq.0) cycle
            if(lnomycall .and. iaptype.gt.1 .and. iaptype.lt.31) cycle ! skip AP if mycall is missed in config
            if(lhiscallstd .and. iaptype.eq.31 .and. .not.lcqsignal) cycle ! not CQ signal, skip CQ DXCall DXgrid mask
            if(lqsomsgdcd .and. iaptype.gt.0 .and. iaptype.lt.25) cycle ! QSO message already decoded
            if(.not.stophint .and. (iaptype.eq.31 .or. iaptype.eq.36)) cycle ! CQ and RR73 decoding not needed if TX is on
            if(nQSOProgress.eq.1) then
              if(lfoxspecrpt) then
                if(iaptype.eq.21) cycle ! it is not standard message report signal
                if((iaptype.eq.31 .or. iaptype.eq.36) .and. nfoxspecrpt.gt.3) cycle ! probably special message signal
              else
                if(iaptype.eq.22) cycle ! it is not special message report signal
                if((iaptype.eq.31 .or. iaptype.eq.36) .and. nmic.gt.3) cycle ! probably standard message signal with mybcall
              endif
            endif
            if(nQSOProgress.eq.3) then
              if(lfoxspecrpt) then
                if(iaptype.eq.21) cycle ! it is not standard message report signal
              else
                if(iaptype.eq.22) cycle ! it is not special message report signal
              endif
              if(lfoxstdr73) then
                if(iaptype.eq.24) cycle ! it is not special message RR73 signal
              else
                if(iaptype.eq.23) cycle ! it is not standard message RR73 signal
              endif
            endif
! can be staying in queue, need to process possible incoming report iaptype 21/22 even if TX is switched off
            if(.not.lapmyc .and. (iaptype.eq.23 .or. iaptype.eq.24)) cycle ! skip AP for mycall RR73 in 2..3 minutes after last TX
            fdelta=abs(f1-nfqso); fdeltam=modulo(fdelta,60.) ! dupe string to prevent compiler warning
            if(nQSOProgress.gt.0 .and. iaptype.lt.31 .and. (fdelta.gt.245.0 .or. fdeltam.gt.3.0)) cycle ! AP shall be applied to Fox frequencies only
            if((iaptype.eq.31 .or. iaptype.eq.36) .and. .not.lwidedxcsearch .and. (fdelta.gt.245.0 .or. fdeltam.gt.3.0)) cycle ! only Fox frequencies DX Call searching
            if(iaptype.eq.31 .and. .not.lhiscallstd .and. lapcqonly) cycle ! skip weak CQ signals with std call
            if(iaptype.eq.36 .and. lwidedxcsearch .and. lapcqonly) cycle ! skip weak CQ signals
            if(iaptype.eq.111 .and. lapcqonly) cycle ! skip weak CQ signals with std call

            apmask=0
            if(iaptype.eq.1) then ! CQ ??? ??? type 1 msg
              if(isubp2.eq.20) then; llrz=llrc
              else if(isubp2.eq.21) then; llrz=llrb
              else if(isubp2.eq.22) then; llrz=llra
              endif
              apmask(1:29)=1; llrz(1:29)=apmag*mcq(1:29); apmask(75:77)=1; llrz(75:76)=apmag*(-1)
              llrz(77)=apmag*(+1)
            else if(iaptype.eq.21) then ! MyBaseCall DxBaseCall ???  ! report
              if(isubp2.eq.5) then; llrz=llrc
              else if(isubp2.eq.6) then; llrz=llrb
              else if(isubp2.eq.7) then; llrz=llra
              endif
              apmask(1:58)=1; llrz(1:58)=apmag*apsym; apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
            else if(iaptype.eq.22) then ! ??? RR73; MyCall <???> ??? ! report
              if(isubp2.eq.8) then; llrz=llrc
              else if(isubp2.eq.9) then; llrz=llrb
              else if(isubp2.eq.10) then; llrz=llra
              endif
              apmask(29:66)=1; llrz(29:66)=apmag*apsymsp(29:66); apmask(72:77)=1; llrz(72:73)=apmag*(-1)
              llrz(74)=apmag*(+1); llrz(75:77)=apmag*(-1)
            else if(iaptype.eq.23) then ! MyBaseCall DxBaseCall RR73
              if(isubp2.eq.11) then; llrz=llrc
              else if(isubp2.eq.12) then; llrz=llrb
              else if(isubp2.eq.13) then; llrz=llra
              endif
              apmask(1:77)=1; llrz(1:58)=apmag*apsym; llrz(59:77)=apmag*mrr73
            else if(iaptype.eq.24) then ! MyCall RR73; ??? <???> ???
              if(isubp2.eq.14) then; llrz=llrc
              else if(isubp2.eq.15) then; llrz=llrb
              else if(isubp2.eq.16) then; llrz=llra
              endif
              apmask(1:28)=1; apmask(57:66)=1; llrz(1:28)=apmag*apsymsp(1:28); llrz(57:66)=apmag*apsymsp(57:66)
              apmask(72:77)=1; llrz(72:73)=apmag*(-1); llrz(74)=apmag*(+1); llrz(75:77)=apmag*(-1)
            else if(iaptype.eq.31) then ! CQ  DxCall Grid(???)
              if(isubp2.eq.23) then; llrz=llrc
              else if(isubp2.eq.24) then; llrz=llrb
              else if(isubp2.eq.25) then; llrz=llra
              endif
              if(lhiscallstd) then
                if(lnohisgrid) then
                  apmask(1:58)=1; llrz(1:58)=apmag*apcqsym(1:58); apmask(75:77)=1; llrz(75:76)=apmag*(-1); llrz(77)=apmag*(+1)
                else
                  apmask(1:77)=1; llrz(1:77)=apmag*apcqsym
                endif
              else
                apmask(1:77)=1; llrz(1:77)=apmag*apcqsym
              endif
            else if(iaptype.eq.36) then
              if(isubp2.eq.26) then; llrz=llrc
              else if(isubp2.eq.27) then; llrz=llrb
              else if(isubp2.eq.28) then; llrz=llra
              endif
              if(lhiscallstd .or. (.not.lhiscallstd .and. len_trim(hiscall).gt.2 .and. index(hiscall,"/").gt.0)) then
                apmask(30:77)=1; llrz(30:58)=apmag*apsym(30:58); llrz(59:77)=apmag*mrr73 ! ??? DxBaseCall RR73
              else
                cycle ! (noncompound .and. nonstandard) Fox callsign being not supported by Fox WSJT-X
              endif
            else if(iaptype.eq.111) then ! CQ ??? type 4 msg (iflip,nrpt,icq,i3 001100)
              if(isubp2.eq.29) then; llrz=llrc
              else if(isubp2.eq.30) then; llrz=llrb
              else if(isubp2.eq.31) then; llrz=llra
              endif
              apmask(72:77)=1; llrz(72:73)=apmag*(-1); llrz(74:75)=apmag*(1); llrz(76:77)=apmag*(-1)
            endif
          else; cycle ! fallback
          endif
        endif

        cw=0
!        call timer('bpd174_91 ',0)
        call bpdecode174_91(llrz,apmask,max_iterations,message77,cw,nharderrors,  &
             niterations)
!        call timer('bpd174_91 ',1)
        dmin=0.0
        if(nharderrors.lt.0) then
          ndeep=3
          if(lqsosig .or. lmycsignal) then
            if((dfqso.lt.napwid .or. (abs(nftx-f1).lt.napwid .and. lapmyc)) .and. .not.nagainfil) ndeep=4
            if(lapmyc .and. lqsomsgdcd .or. iaptype.eq.0) ndeep=3 ! deep is not needed, reduce number of CPU cycles
            if(.not.stophint .and. len_trim(hiscall).gt.2) ndeep=3 ! unload CPU, let ft8s pick up QSO message
          endif
          if(ldxcsig .and. stophint .and. dfqso.lt.napwid) ndeep=4 ! DXCall search inside RX napwid
          if(lhound) ndeep=3 ! we have no enough CPU resources to decode all Fox slots with ndeep=4
!          if(nagainfil .or. swl) ndeep=5 ! 30 against 26 -23dB, more than 15sec to decode and many false decodes
!          if(swl) ndeep=4 ! 29 decodes -23dB, 7..12sec to decode
          if(nagainfil) ndeep=5
!print *,omp_get_nested(),OMP_get_num_threads()
!          call timer('osd174_91 ',0)
          call osd174_91(llrz,apmask,ndeep,message77,cw,nharderrors,dmin,nthr)
!          call timer('osd174_91 ',1)
        endif
        nbadcrc=1; msg37=''
        if(count(cw.eq.0).eq.174) cycle           !Reject the all-zero codeword
        if(nharderrors.lt.0 .or. nharderrors+dmin.ge.60.0 .or. (isubp2.gt.2 .and. nharderrors.gt.39)) then ! chk isubp2 value
          if(nweak.eq.2 .and. isubp1.eq.2) then
            if(ipass.eq.npass-1) then
              if(lcqsignal) then
                if(isubp2.eq.22) then ! last pass, std and nonstd mycall
                  lfoundcq=.false.
                  do ik=1,numdeccq
                    if(tmpcqdec(ik)%freq.gt.5001.0) exit
                    if(abs(tmpcqdec(ik)%freq-f1).lt.5.0 .and. abs(tmpcqdec(ik)%xdt-xdt).lt.0.05) then ! max signal delay
                      lfoundcq=.true.; exit
                    endif
                  enddo
                  if(.not.lfoundcq .and. ncqsignal.lt.numcqsig) then
                    ncqsignal=ncqsignal+1
                    tmpcqsig(ncqsignal)%freq=f1; tmpcqsig(ncqsignal)%xdt=xdt
                    tmpcqsig(ncqsignal)%cs=cstmp2
                  endif
                endif
              endif
              if(lmycsignal .and. isubp2.eq.19) then ! last pass
                lfoundmyc=.false.
                do ik=1,numdecmyc
                  if(tmpmyc(ik)%freq.gt.5001.0) exit
                  if(abs(tmpmyc(ik)%freq-f1).lt.5.0 .and. abs(tmpmyc(ik)%xdt-xdt).lt.0.05) then ! max signal delay
                    lfoundmyc=.true.; exit
                  endif
                enddo
                if(.not.lfoundmyc .and. nmycsignal.lt.nummycsig) then
                  nmycsignal=nmycsignal+1
                  tmpmycsig(nmycsignal)%freq=f1; tmpmycsig(nmycsignal)%xdt=xdt
                  tmpmycsig(nmycsignal)%cs=cstmp2
                endif
              endif
            endif
            if(ipass.eq.npass) then
              if(lqsocandave .and. (iaptype.eq.3 .or. iaptype.eq.6)) then
                tmpqsosig(1)%freq=f1; tmpqsosig(1)%xdt=xdt; tmpqsosig(1)%cs=cstmp2
              endif
            endif
          endif

          if(isubp1.gt.1) cycle

          if(lqsothread .and. (.not.lhound .and. iaptype.ge.3 .or. lhound .and. (iaptype.eq.21 .or. iaptype.eq.23)) &
             .and. .not.lsdone) then
            if(.not.lqsomsgdcd .and. .not.(.not.lmycallstd .and. .not.lhiscallstd)) then
              if(.not.lft8sdec .and. .not.stophint .and. dfqso.lt.2.0) then
                call ft8s(s8,srr,itone,msg37,lft8s,nft8rxfsens,stophint)
                if(lft8s) then
                  if(index(msg37,'<').gt.0) then; lhashmsg=.true.; call delbraces(msg37); endif
                  nbadcrc=0; lft8sdec=.true.
                endif
              endif
            endif
            lsdone=.true.
          endif
          if(nbadcrc.eq.0) then; i3=1; n3=1; exit; endif

          if(lsd .and. isubp2.eq.3 .and. nbadcrc.eq.1 .and. srr.lt.7.0) then ! low DR setups shall not try FT8SD for strong signals
            call ft8sd(s8,srr,itone,msgd,msg37,lft8sd,lcq)
            if(lft8sd) then
              if(levenint) then; evencopy(isd)%lstate=.false.
              elseif(loddint) then; oddcopy(isd)%lstate=.false.
              endif
              i3=1; n3=1; iaptype=0; nbadcrc=0; lsd=.false.; exit
            endif
          endif

          if(nweak.eq.1 .and. isubp1.eq.1) then
            if(ipass.eq.npass-1) then
              if(lcqsignal) then
                if(isubp2.eq.22) then ! last pass, std and nonstd mycall
                  lfoundcq=.false.
                  do ik=1,numdeccq
                    if(tmpcqdec(ik)%freq.gt.5001.0) exit
                    if(abs(tmpcqdec(ik)%freq-f1).lt.5.0 .and. abs(tmpcqdec(ik)%xdt-xdt).lt.0.05) then ! max signal delay
                      lfoundcq=.true.; exit
                    endif
                  enddo
                  if(.not.lfoundcq .and. ncqsignal.lt.numcqsig) then
                    ncqsignal=ncqsignal+1
                    tmpcqsig(ncqsignal)%freq=f1; tmpcqsig(ncqsignal)%xdt=xdt
                    tmpcqsig(ncqsignal)%cs=cstmp2
                  endif
                endif
              endif
              if(lmycsignal .and. isubp2.eq.19) then ! last pass
                lfoundmyc=.false.
                do ik=1,numdecmyc
                  if(tmpmyc(ik)%freq.gt.5001.0) exit
                  if(abs(tmpmyc(ik)%freq-f1).lt.5.0 .and. abs(tmpmyc(ik)%xdt-xdt).lt.0.05) then ! max signal delay
                    lfoundmyc=.true.; exit
                  endif
                enddo
                if(.not.lfoundmyc .and. nmycsignal.lt.nummycsig) then
                  nmycsignal=nmycsignal+1
                  tmpmycsig(nmycsignal)%freq=f1; tmpmycsig(nmycsignal)%xdt=xdt
                  tmpmycsig(nmycsignal)%cs=cstmp2
                endif
              endif
            endif
            if(ipass.eq.npass) then
              if(lqsocandave .and. (iaptype.eq.3 .or. iaptype.eq.6)) then
                tmpqsosig(1)%freq=f1; tmpqsosig(1)%xdt=xdt; tmpqsosig(1)%cs=cstmp2
              endif
            endif
          endif

          if(nbadcrc.eq.1) cycle
        endif

        write(c77,'(77i1)') message77
        read(c77(72:74),'(b3)') n3
        read(c77(75:77),'(b3)') i3
        if(i3.gt.4 .or. (i3.eq.0 .and. n3.gt.5)) cycle
!print *,i3,n3,iaptype
        call unpack77(c77,1,msg37,unpk77_success,nthr)
        if(.not.unpk77_success) then
          if(lqsothread .and. (.not.lhound .and. iaptype.ge.3 .or. lhound .and. &
             (iaptype.eq.21 .or. iaptype.eq.23)) .and. .not.lsdone) then
            if(.not.lqsomsgdcd .and. .not.(.not.lmycallstd .and. .not.lhiscallstd)) then
              if(.not.lft8sdec .and. .not.stophint .and. dfqso.lt.2.0) then
                call ft8s(s8,srr,itone,msg37,lft8s,nft8rxfsens,stophint)
                if(lft8s) then
                  if(index(msg37,'<').gt.0) then; lhashmsg=.true.; call delbraces(msg37); endif
                  nbadcrc=0; lft8sdec=.true.
                endif
              endif
            endif
            lsdone=.true.
            if(nbadcrc.eq.0) then; i3=1; n3=1; exit; endif
          endif
          cycle
        endif
        if(iaptype.eq.1 .and. msg37(1:10).eq.'CQ DE AA00') then; nbadcrc=1; cycle; endif
        lcall1hash=.false.; if(msg37(1:1).eq.'<') lcall1hash=.true.
        nbadcrc=0  ! If we get this far: valid codeword, valid (i3,n3), nonquirky message.
        call get_tones_from_77bits(message77,itone)

! 0.1  K1ABC RR73; W9XYZ <KH1/KH7Z> -11   28 28 10 5       71   DXpedition Mode
        i3bit=0; if(i3.eq.0 .and. n3.eq.1) i3bit=1
!        iFreeText=message77(57)
! 0.0  Free text
        if(i3.eq.0 .and. n3.eq.0) then; lFreeText=.true.; else; lFreeText=.false.; endif
! delete braces
        lcall2hash=.false.
        if(.not.lFreeText .and. i3bit.ne.1 .and. index(msg37,'<').gt.0) then
          if(index(msg37,'<').gt.4) lcall2hash=.true.
          if(index(msg37,'<').gt.0) then; lhashmsg=.true.; call delbraces(msg37); endif
        endif
!print *,msg37
        if(nbadcrc.eq.0) exit
      enddo ! ap passes
      if(nbadcrc.eq.0) exit
    enddo ! weak sigs
    if(nbadcrc.eq.0) exit
  enddo !nqso sync

2 if(nbadcrc.eq.0) then
! check for the false FT8S decode
    if(lft8s .and. lrepliedother) then; msg37=''; nbadcrc=1; return; endif
    msg37_2=''
    if(i3bit.eq.1 .and. .not.lft8s .and. .not.lft8sd) then
      call msgparser(msg37,msg37_2); lspecial=.true.
!protection against a false FT8S decode in Hound mode 
      if(lhound .and. dfqso.lt.2.0) lqsomsgdcd=.true.; !$OMP FLUSH (lqsomsgdcd)
    endif
    qual=1.0-(nharderrors+dmin)/60.0
    xsnr=0.001; xnoi=1.E-8
    xsnrtmp=0.001
    do i=1,79
      xsig=s8(itone(i),i)**2
      xnoi=(sum(s8(0:7,i)**2) - xsig)/7.0
      if(xnoi.lt.0.01) xnoi=0.01 ! safe division and better accuracy
      if(xnoi.lt.xsig) then; xsnr=xsig/xnoi; else; xsnr=1.01; endif
      xsnrtmp=xsnrtmp+xsnr
    enddo
!print *,xsig,xnoi
    xsnr=xsnrtmp/79.0-1.0
    xsnr=10.0*log10(xsnr)-26.5
    if(xsnr.gt.7.0) xsnr=xsnr+(xsnr-7.0)/2.0
    if(xsnr.gt.30.0) then; xsnr=xsnr-1.0; if(xsnr.gt.40.0) xsnr=xsnr-1.0; if(xsnr.gt.49.0) xsnr=49.0; endif 
    xsnrs=xsnr
    if(xsnr .lt. -17.0) then
      if(xsnr.lt.-22.5 .and. xsnr.gt.-23.5) xsnr=-22.5 ! safe division and better accuracy
      xsnr=xsnr-(1.0+1.4/(23.0+xsnr))**2+1.2
    endif
    if(iaptype.eq.0) then; if(xsnr.lt.-23.0) xsnr=-23.0; else; if(xsnr.lt.-24.0) xsnr=-24.0; endif
    if(lft8s .or. lft8sd) then
      if(xsnr.lt.-22.0) xsnr=xsnrs-1.0 ! correction offset
      if(xsnr.lt.-26.0) xsnr=-26.0;
! -26  0.1 1477 ~ AC1MX AC1MX R-17          ^
      if(len_trim(mycall).gt.3 .and. index(msg37,' '//trim(mycall)//' ').gt.1) then; msg37=''; return; endif 
      go to 4 ! bypass checking to false decode
    endif
!print *,qual,msg37
    rxdt=xdt-0.5
    if(iaptype.gt.34 .and. iaptype.lt.40) then ! DX Call searching false iaptype 35,36: 'CS7CYU/R FO5QB 73', 'T57KWP/R FO5QB RR73'
      ispc1=index(msg37,' ')
      if(ispc1.gt.5) then
        if(msg37((ispc1-2):(ispc1-1)).eq.'/R') then ! have to cascade it to prevent getting out of index range
          call_a=''; call_a=msg37(1:(ispc1-3))
          lfound=.false.
          call searchcalls(call_a,"            ",lfound)
          if(.not.lfound) then; nbadcrc=1; msg37=''; return; endif
        endif
      endif
    else if(qual.lt.0.39 .or. xsnr.lt.-20.5 .or. rxdt.lt.-0.5 .or. rxdt.gt.1.9 .or. (iaptype.gt.0 .and. iaptype.lt.4) .or. &
            iaptype.eq.11 .or. iaptype.eq.21 .or. iaptype.eq.40 .or. iaptype.eq.41) then
      if(iaptype.gt.3 .and. iaptype.lt.7) go to 4 ! skip, nothing to check
      if(iaptype.ne.2 .and. iaptype.ne.3 .and. iaptype.ne.11 .and. iaptype.ne.21 &
            .and. iaptype.ne.40 .and. iaptype.ne.41) then
        if((mybcall.ne."            " .and. index(msg37,mybcall).gt.0) .or. &
           (hisbcall.ne."            " .and. index(msg37,hisbcall).gt.0)) go to 256
      endif
      if(i3bit.eq.1) then; call chkspecial8(msg37,msg37_2,nbadcrc)
      else; call chkfalse8(msg37,i3,n3,nbadcrc,iaptype,lcall1hash)
      endif
      if(nbadcrc.eq.1) then; msg37=''; return; endif
    endif
    if(iaptype.eq.2 .or. iaptype.eq.3 .or. iaptype.eq.11 .or. iaptype.eq.21 .or. iaptype.eq.40 .or. iaptype.eq.41) go to 4 ! already checked

! decoded without AP masks:
! CQ PLIB U40OKH FE61
! CQ 000 RO8JSV GJ38
256 if(iaptype.eq.0 .and. i3.eq.1 .and. msg37(1:3).eq.'CQ ') then
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
      if((ispc2-ispc1).eq.4 .or. (ispc2-ispc1).eq.5) then
        ispc3=index(msg37((ispc2+1):),' ')+ispc2; ispc4=index(msg37((ispc3+1):),' ')+ispc3
        if((ispc3-ispc2).gt.3 .and. (ispc4-ispc3).eq.5) then
          grid=''
          if(msg37(ispc3+1:ispc3+1).gt.'@' .and. msg37(ispc3+1:ispc3+1).lt.'S' .and. &
             msg37(ispc3+2:ispc3+2).gt.'@' .and. msg37(ispc3+2:ispc3+2).lt.'S' .and. &
             msg37(ispc3+3:ispc3+3).lt.':' .and. msg37(ispc3+4:ispc3+4).lt.':') grid=msg37(ispc3+1:ispc4-1)
          if(grid.ne.'') then
            callsign=''; callsign=msg37(ispc2+1:ispc3-1)
            call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
            if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
          endif
        endif
      endif
    endif

! i3=1,2 false decodes with ' R '
! 3B4NDC/R C40AUZ/R R IR83  i3=1
! MS8QQS UX3QBS/P R NG63  i3=2
! <...> P32WRF R LR56
! P32WRF <...> R LR56
    if((i3.eq.1 .or. i3.eq.2) .and. index(msg37,' R ').gt.0) then
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1; ispc3=index(msg37((ispc2+1):),' ')+ispc2
      if(ispc3-ispc2.eq.2) then
        if(msg37(ispc2:ispc3).eq.' R ') then
          call_b=''
          if((i3.eq.1 .and. msg37(ispc2-2:ispc2-1).eq.'/R') .or. (i3.eq.2 .and. msg37(ispc2-2:ispc2-1).eq.'/P')) then
            call_b=msg37(ispc1+1:ispc2-3)
          else
            call_b=msg37(ispc1+1:ispc2-1)
          endif
          if(lcall2hash) then ! valid call_b can be found in hash table, check call_a then
            call_a=''
            if((i3.eq.1 .and. msg37(ispc1-2:ispc1-1).eq.'/R') .or. (i3.eq.2 .and. msg37(ispc1-2:ispc1-1).eq.'/R')) then
              call_a=msg37(1:ispc1-3)
            else
              call_a=msg37(1:ispc1-1)
            endif
            falsedec=.false.; call chkflscall('CQ          ',call_a,falsedec)
            if(falsedec) then; nbadcrc=1; msg37=''; return; endif
          else if(len_trim(call_b).gt.2) then
            ispc4=index(msg37((ispc3+1):),' ')+ispc3; grid=''
            if(ispc4-ispc3.eq.5 .and. msg37(ispc3+1:ispc3+1).gt.'@' .and. msg37(ispc3+1:ispc3+1).lt.'S' .and. &
               msg37(ispc3+2:ispc3+2).gt.'@' .and. msg37(ispc3+2:ispc3+2).lt.'S' .and. &
               msg37(ispc3+3:ispc3+3).lt.':' .and. msg37(ispc3+4:ispc3+4).lt.':') grid=msg37(ispc3+1:ispc4-1)
            if(grid.ne.'') then
              call chkgrid(call_b,grid,lchkcall,lgvalid,lwrongcall)
              if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
            endif
          endif
        endif
      endif
    endif

! 0.3   WA9XYZ KA1ABC R 16A EMA            28 28 1 4 3 7    71   ARRL Field Day
! 0.4   WA9XYZ KA1ABC R 32A EMA            28 28 1 4 3 7    71   ARRL Field Day
! -23  3.1  197 ~ Z67BGE H67HJI 22G EMA i3=0 n3=4
!  -3  2.2 FY4IML UV7BEA R 24F NNJ   i3=0 n3=4
    if(i3.eq.0 .and. n3.gt.2 .and. n3.lt.5) then
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
      if(ispc1.gt.3 .and. ispc2.gt.7) then
        call_a=''; call_b=''
        if(msg37(1:ispc1-1).eq.'/R' .or. msg37(1:ispc1-1).eq.'/P') then; call_a=msg37(1:ispc1-3)
        else; call_a=msg37(1:ispc1-1)
        endif
        if(msg37(ispc1+1:ispc2-1).eq.'/R' .or. msg37(ispc1+1:ispc2-1).eq.'/P') then; call_b=msg37(ispc1+1:ispc2-3)
        else; call_b=msg37(ispc1+1:ispc2-1)
        endif
! operators outside USA and Canada shall transmit ' DX '
        if(call_b(1:1).ne.'A' .and. call_b(1:1).ne.'K' .and. call_b(1:1).ne.'N' .and. &
          call_b(1:1).ne.'W' .and. call_b(1:1).ne.'V' .and. call_b(1:1).ne.'C' .and. call_b(1:1).ne.'X') then
            if(index(msg37,' DX ').lt.1) then; nbadcrc=1; msg37=''; return; endif
        endif
! USA AA..AL
        if(call_b(1:1).eq.'A' .and. (call_b(2:2).lt.'A' .or. call_b(2:2).gt.'L')) then
            if(index(msg37,' DX ').lt.1) then; nbadcrc=1; msg37=''; return; endif
        endif
! Canada CF..CK,CY,CZ,VA..VG,VO,VX,VY,XJ..XO
        if(call_b(1:1).eq.'C' .and. (call_b(2:2).lt.'F' .or. call_b(2:2).gt.'K')) then
          if(call_b(2:2).ne.'Y' .and. call_b(2:2).ne.'Z') then
            if(index(msg37,' DX ').lt.1) then; nbadcrc=1; msg37=''; return; endif
          endif
        endif
        if(call_b(1:1).eq.'V' .and. (call_b(2:2).lt.'A' .or. call_b(2:2).gt.'G')) then
          if(call_b(2:2).ne.'O' .and. call_b(2:2).ne.'X' .and. call_b(2:2).ne.'Y') then
            if(index(msg37,' DX ').lt.1) then; nbadcrc=1; msg37=''; return; endif
          endif
        endif
        if(call_b(1:1).eq.'X' .and. (call_b(2:2).lt.'J' .or. call_b(2:2).gt.'O')) then
          if(index(msg37,' DX ').lt.1) then; nbadcrc=1; msg37=''; return; endif
        endif
        if(xsnr.lt.-19.0 .or. rxdt.lt.-0.5 .or. rxdt.gt.1.0) then
          falsedec=.false.; call chkflscall(call_a,call_b,falsedec)
          if(falsedec) then; nbadcrc=1; msg37=''; return; endif
        endif
      endif
    endif

! EU VHF contest exchange i3=5
! <PA3XYZ> <G4ABC/P> R 590003 IO91NP      h12 h22 r1 s3 S11 g25
! packing is not implemented in WSJT-X 2.5.4?
! old implementation with i3=0 n3=2
! JL6GSC/P R 571553 CJ76MV i3=5
! G59XTB R 521562 RA82SJ
!    if(i3.eq.0 .and. n3.eq.2 .and. (xsnr.lt.-19.0 .or. rxdt.lt.-0.5 .or. rxdt.gt.1.0)) then
!      ispc1=index(msg37,' ')
!      if(ispc1.gt.3) then
!        call_b=''
!        if(msg37(ispc1-2:ispc1-1).eq.'/P') then; call_b=msg37(1:ispc1-3)
!        else; call_b=msg37(1:ispc1-1)
!        endif
!        falsedec=.false.; call chkflscall('CQ          ',call_b,falsedec)
!        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
!      endif
!    endif

! FT8OPG/R Z27HRN/R OH12 check all standard messages with contest callsigns
! /P has i3.eq.2 .and. n3.eq.0
    if(iaptype.eq.0 .and. i3.eq.1 .and. n3.eq.0 .and. index(msg37,'/R ').gt.3) then
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
      if(ispc1.gt.3 .and. ispc2.gt.6) then
        call_a=''; call_b=''
        if(msg37(ispc1-2:ispc1-1).eq.'/R') then; call_a=msg37(1:ispc1-3); else; call_a=msg37(1:ispc1-1); endif
        if(msg37(ispc2-2:ispc2-1).eq.'/R') then; call_b=msg37(ispc1+1:ispc2-3); else; call_b=msg37(ispc1+1:ispc2-1); endif
        falsedec=.false.; call chkflscall(call_a,call_b,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif
    endif

! -23 -0.5 2533 ~ <...> W LKNQZG2K4 RR73  invalid message, iaptype=0 this type of message is not allowed for transmission with RR73   
! -23 -1.2 1335 ~ <...> Z7VENB8R G9 RRR   non AP decode, iaptype=0 invalid message, this type of message is not allowed 
! for transmission with RRR   
    if(msg37(1:2).eq.'<.') then
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1; ispc3=index(msg37((ispc2+1):),' ')+ispc2
      ispc4=index(msg37((ispc3+1):),' ')+ispc3
      if((ispc4-ispc3.eq.4 .and. msg37(ispc3+1:ispc4-1).eq.'RRR') .or. &
         (ispc4-ispc3.eq.5 .and. msg37(ispc3+1:ispc4-1).eq.'RR73') .or. &
         (ispc4-ispc3.eq.3 .and. msg37(ispc3+1:ispc4-1).eq.'73')) then; nbadcrc=1; msg37=''; return; endif
! -19 0.0 2256 ~ <...> 9T4DQZ RP53  non AP decode, iaptype=0 i3=1 n3=1  SAME AS CQ MSG
! <...> 9T4DQZ -15(R-15) message has i3=1 n3=4
      if(i3.eq.1 .and. n3.eq.1 .and. (xsnr.lt.-18.0 .or. rxdt.lt.-0.5 .or. rxdt.gt.1.0)) then
        callsign='            '; callsign=msg37(ispc1+1:ispc2-1); grid=msg37(ispc2+1:ispc3-1)
        include 'callsign_q.f90'
        call chkgrid(callsign,grid,lchkcall,lgvalid,lwrongcall)
        if(lwrongcall) then; nbadcrc=1; msg37=''; return; endif
        if(lchkcall .or. .not.lgvalid) then
          falsedec=.false.
          call chkflscall('CQ          ',callsign,falsedec)
          if(falsedec) then; nbadcrc=1; msg37=''; return; endif
        endif
      endif
    endif

! -22  0.3 1000 ~ 9Y4DWY <...> BF70  iaptype=0 i3=1 n3=2  invalid message in FT8 protocol, can be transmitted manually
    if(i3.eq.1) then
      ispc1=index(msg37,' ')
      if(msg37(ispc1+1:ispc1+2).eq.'<.') then
        ispc2=index(msg37((ispc1+1):),' ')+ispc1
        ispc3=index(msg37((ispc2+1):),' ')+ispc2
        if(ispc3-ispc2.eq.5 .and. msg37(ispc2+1:ispc2+1).gt.'@' .and. msg37(ispc2+1:ispc2+1).lt.'S' .and. &
           msg37(ispc2+2:ispc2+2).gt.'@' .and. msg37(ispc2+2:ispc2+2).lt.'S' .and. &
           msg37(ispc2+3:ispc2+3).lt.':' .and. msg37(ispc2+4:ispc2+4).lt.':') then ! there is a valid grid in message
          call_b=''; call_b=msg37(1:ispc1-1)
          falsedec=.false.; call chkflscall('CQ          ',call_b,falsedec)
          if(falsedec) then; nbadcrc=1; msg37=''; return; endif
        endif
      endif
    endif

! i3=3 parse ARRL RTTY contest message
! i3 n3                                      Bits               Total  Message type
! 3     TU; W9XYZ K1ABC R 579 MA             1 28 28 1 3 13       74   ARRL RTTY contest
! 3     TU; W9XYZ G8ABC R 559 0013           1 28 28 1 3 13       74   ARRL RTTY (DX)
! TU; D47IAQ <...> 559 032' does protocol support the message?
    if(i3.eq.3 .and. msg37(1:3).eq.'TU;') then
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1 
      ispc3=index(msg37((ispc2+1):),' ')+ispc2
      call_a=''; call_b=''; call_a=msg37(ispc1+1:ispc2-1); call_b=msg37(ispc2+1:ispc3-1)
! check for false, too tough, need to rework if contest is supported
      falsedec=.false.
      call chkflscall(call_a,call_b,falsedec)
      if(falsedec) then; nbadcrc=1; msg37=''; return; endif
! parse
      lspecial=.true.
      msg37_2=msg37(5:37)//'    '
      msg37=''; msg37='DE '//trim(call_b)//' TU'
    endif

! DX Call searching false decodes, search for 1st callsign in ALLCALL7
! 6W6VIV EY8MM 73
! 6Y9KOZ EY8MM RR73
    if(iaptype.gt.34 .and. iaptype.lt.40 .and. (xsnr.lt.-21.0 .or. rxdt.lt.-0.5 .or. rxdt.gt.1.0)) then
      ispc1=index(msg37,' ')
      if(ispc1.gt.1) then
        call_b=''; call_b=msg37(1:ispc1-1)
        falsedec=.false.; call chkflscall('CQ          ',call_b,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif
    endif

4   ldupemsg=.false.
    if(ndecodes.gt.0) then
      do i=1,ndecodes; if(allmessages(i).eq.msg37 .and. abs(allfreq(i)-f1).lt.45.0) ldupemsg=.true.; enddo
    endif

    if(.not.ldupemsg .and. dfqso.lt.2.0 .and. ((i3.eq.1 .and. .not.lft8s) .or. lft8s)) then
      if(msg37(1:msgrootlen+1).eq.trim(msgroot)//' ') then
        lasthcall=hiscall; lastrxmsg(1)%lastmsg=msg37; lastrxmsg(1)%xdt=xdt-0.5; lastrxmsg(1)%lstate=.true.
        lqsomsgdcd=.true.
!$OMP FLUSH (lqsomsgdcd)
      else if(.not.lft8s .and. mycalllen1.gt.2) then
        if(msg37(1:mycalllen1).ne.trim(mycall)//' ' .and. index(msg37,' '//trim(hiscall)//' ').gt.0) then
          lrepliedother=.true.
        endif
      endif
    endif
    if(len_trim(hiscall).gt.3 .and. .not.lqsomsgdcd) then
      if(msg37(1:msgrootlen+1).eq.trim(msgroot)//' ') then
      lqsomsgdcd=.true.
!$OMP FLUSH (lqsomsgdcd)
      endif
    endif
    if(.not.stophint .and. .not.ldupemsg .and. dfqso.lt.2.0 .and. nlasttx.gt.0 .and. &
       nlasttx.lt.6 .and. msg37(1:3).eq.'CQ ') then
      nlength=len_trim(hiscall)
      if(nlength.gt.2) then
        if(msg37(4:4+nlength).eq.trim(hiscall)//' ' .or. msg37(7:7+nlength).eq.trim(hiscall)//' ') lrepliedother=.true.
      endif
    endif

    if(mycalllen1.gt.2) then
      if(.not.ldupemsg .and. msg37(1:mycalllen1).eq.trim(mycall)//' ') then
        nincallthr(nthr)=nincallthr(nthr)+1
        nindex=maskincallthr(nthr)+nincallthr(nthr)
        if(nindex.lt.maskincallthr(nthr+1)) then
          msgincall(nindex)=msg37
          xdtincall(nindex)=xdt-0.5
        else
          nincallthr(nthr)=nincallthr(nthr)-1
        endif
      endif
    endif

    ldupeft8sd=.false.
    if(ncount.gt.0 .and. lft8sd) then
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
      msgbase37=''; msgbase37=msg37(1:ispc2-1)
      do i=1,ncount
        if(trim(msgbase37).eq.trim(msgsrcvd(i))) then; ldupeft8sd=.true.; exit; endif
      enddo
    endif
    if(ldupeft8sd) then; msg37=''; nbadcrc=1; return; endif

    if(.not.ldupemsg .and. i3.eq.1 .and. .not.lft8sd .and. .not.lft8s .and. msg37(1:3).ne.'CQ ') then
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
      ncount=ncount+1
      msgsrcvd(ncount)=msg37(1:ispc2-1)
    endif

! -23  0.0 1606 ~ <...> 3U1TBM/R CC65 i3=4 n3=0
! -18  0.3 1609 ~ CQ 6U6MBL/R IJ90 i3=1  in some noisy setups false decode with any SNR value is possible
    if((i3.eq.4 .and. n3.eq.0) .or. (i3.eq.1 .and. msg37(1:3).eq."CQ ")) then
      if(index(msg37,'/R ').gt.9) then
        ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
        if(ispc2.gt.11) then
          ispc3=index(msg37((ispc2+1):),' ')+ispc2
          if(msg37((ispc2-2):(ispc2-1)).eq.'/R') then
            if((ispc3-ispc2).eq.5) then
              grid=msg37(ispc2+1:ispc3-1)
! grid can not be txed, invalid message:
              if(i3.eq.4 .and. len_trim(grid).eq.4) then; nbadcrc=1; msg37=''; return; endif
              call_b=''; call_b=msg37((ispc1+1):(ispc2-3))
              call chkgrid(call_b,grid,lchkcall,lgvalid,lwrongcall)
              if(lwrongcall .or. .not.lgvalid) then; nbadcrc=1; msg37=''; return; endif
            endif
          endif
        endif
      endif
    endif

!print *,'iaptype',iaptype
!print *,i3,msg37

! protocol violations
! 713STG 869TK NO05  i3=2 n3=5, false decode, as per protocol type2 shall be /P message
    if(i3.eq.2 .and. index(msg37,'/P ').lt.1) then; msg37=''; nbadcrc=1; return; endif
! -18  0.5  584 ~ UA3ALE <...> PR07         *  AP decode with grid
    if(iaptype.eq.2) then
      nhash=index(msg37,"<...>")
      if(nhash.gt.4 .and. nhash.lt.13 .and. msg37(nhash+6:nhash+6).gt.'@' .and. msg37(nhash+7:nhash+7).gt.'@') then
        msg37=''; nbadcrc=1; return
      endif
    endif

!    if(lsubtract .and. .not.ldupemsg) then
    if(lsubtract) then
      noff=10; sync0=0.; syncp=0.; syncm=0.; k=1
      call gen_ft8wave(itone,79,1920,2.0,12000.0,0.0,csig0,xjunk,1,151680)
      do i=0,78
        do j=1,32
          csig(j)=csig0(k)
          k=k+60
        enddo
        i21=i0+i*32; z1=0.; z1=sum(cd0(i21:i21+31)*conjg(csig)); sync0 = sync0 + real(z1)**2 + aimag(z1)**2
        i21=i0+i*32+noff; z1=0.; z1=sum(cd0(i21:i21+31)*conjg(csig)); syncp = syncp + real(z1)**2 + aimag(z1)**2
        i21=i21-noff*2; z1=0.; z1=sum(cd0(i21:i21+31)*conjg(csig)); syncm = syncm + real(z1)**2 + aimag(z1)**2
      enddo
      call peakup(syncm,sync0,syncp,dx)
      if(abs(dx).gt.1.0) then; scorr=0.; else; scorr=real(noff)*dx; endif
      xdt3=xdt+scorr*dt2
      call subtractft8(itone,f1,xdt3,swl)
      lsubtracted=.true. ! inside current thread
      if(npos.lt.200) then; npos=npos+1; freqsub(npos)=f1; endif
    endif

    if(lhidehash .and. index(msg37,'<...>').gt.6) then; nbadcrc=1; msg37=''; msg37_2=''; endif

!if(nbadcrc.eq.0 .and. iaptype.ge.2 .and. iaptype.le.3) then
!write (*,"(I1.1,1x,I1.1,1x,F5.3,38x,A1)") iaptype,iFreeText,qual,'d'
!flush(6)
!endif

  endif

  return
end subroutine ft8b

subroutine normalizebmet(bmet,n)
  real bmet(n)

!  bmetav=sum(bmet)/real(n)
  bmet2av=sum(bmet*bmet)/real(n)
!  var=bmet2av-bmetav*bmetav
!  if( var .gt. 0.0 ) then
!     bmetsig=sqrt(var)
!  else
     bmetsig=sqrt(bmet2av)
!  endif
  bmet=bmet/bmetsig
  return
end subroutine normalizebmet