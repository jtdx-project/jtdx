! last time modified by Igor UA3DJY on 20200302

subroutine ft8b(newdat,nQSOProgress,nfqso,nftx,ndepth,nft8filtdepth,lapon,napwid,lsubtract, &
                nagainfil,iaptype,f1,xdt,nbadcrc,lft8sdec,msg37,msg37_2,xsnr,swl,stophint,  &
                nthr,lFreeText,imainpass,filter,lft8subpass,lspecial,                       &
                i3bit,lhidehash,lft8s,lmycallstd,lhiscallstd,nsec,lft8sd,i3,n3,nft8rxfsens, &
                ncount,msgsrcvd,lrepliedother,lhashmsg,lqsothread,lft8lowth,lhighsens)
!  use timer_module, only: timer
  use packjt77 ! mycall13,dxcall13
  use ft8_mod1, only : allmessages,ndecodes,apsym,mcq,mrrr,m73,mrr73,icos7,naptypes,nhaptypes,one,twopi,graymap, &
                       oddcopy,evencopy,lastrxmsg,lasthcall,nlasttx,calldt,incall,lqsomsgdcd,mycalllen1,msgroot, &
                       msgrootlen,allfreq,idtone25,lapmyc,idtonemyc,scqnr,smycnr,mycall,hiscall,lhound,apsymsp, &
                       ndxnsaptypes,apsymdxns1,apsymdxns2,lenabledxcsearch,lwidedxcsearch,apcqsym,apsymdxnsrr73,apsymdxns73
  include 'ft8_params.f90'
  parameter (NP2=3199)
  character c77*77,msg37*37,msg37_2*37,msgd*37,msgbase37*37,call_a*12,call_b*12
  character*37 msgsrcvd(130)
  complex cd0(0:3199),cd1(0:3199),cd2(0:3199),cd3(0:3199),ctwk(32),csymb(32),cs(0:7,79),cs1(0:7)
  real a(5),s8(0:7,79),s82(0:7,79),s2(0:511),sp(0:7),s81(0:7),snrsync(21)
  real bmeta(174),bmetb(174),bmetc(174)
  real llra(174),llrb(174),llrc(174),llrd(174)
  integer*1 message77(77),apmask(174),cw(174)
  integer itone(79),ip(1),ka(1)
  integer, intent(in) :: nQSOProgress,nfqso,nftx,ndepth,nft8filtdepth,napwid,nthr,imainpass,nsec,nft8rxfsens
  logical newdat,lsubtract,lapon,lFreeText,nagainfil,lspecial,unpk77_success
  logical(1), intent(in) :: swl,stophint,filter,lft8subpass,lhidehash,lmycallstd,lhiscallstd,lqsothread,lft8lowth,lhighsens
  logical(1) falsedec,lastsync,ldupemsg,lft8s,lft8sdec,lft8sd,lsdone,ldupeft8sd,lrepliedother,lhashmsg, &
             lvirtual2,lvirtual3,lsd,lcq,ldeepsync,lcallsstd

  max_iterations=30; nharderrors=-1; nbadcrc=1; delfbest=0.; ibest=0; dfqso=500.
  fs2=200.; dt2=0.005 ! fs2=12000.0/NDOWN; dt2=1.0/fs2
  ldeepsync=.false.; if(lft8lowth .or. lft8subpass .or. swl) ldeepsync=.true.
  lcallsstd=.true.; if(.not.lmycallstd .or. .not.lhiscallstd) lcallsstd=.false.

  xdt0=xdt; f10=f1
! apply last freq f1 and last DT criteria here  
  nqso=1
  if(lqsothread .and. .not. lft8sdec .and. .not.lqsomsgdcd .and. .not.stophint .and. lapon .and. nlasttx.ge.1 &
    .and. nlasttx.le.4 .and. abs(f10-nfqso).lt.2.51) then
    if(lastrxmsg(1)%lstate .and. abs(lastrxmsg(1)%xdt-xdt).lt.0.18) then; nqso=2
      elseif(.not.lastrxmsg(1)%lstate) then; nqso=2
    endif
  endif

  lvirtual2=.false.; lvirtual3=.false.; maxlasttx=4
  if(lqsothread .and. .not. lft8sdec .and. .not.lqsomsgdcd) then
    if(xdt.gt.4.9 .or. xdt.lt.-4.9) then
      if(lastrxmsg(1)%lstate .and. trim(lastrxmsg(1)%lastmsg).eq.trim(msgroot)//' RRR') maxlasttx=5
    endif
    if(xdt.gt.4.9) then
      if(.not.stophint .and. lapon .and. nlasttx.ge.1 .and. nlasttx.le.maxlasttx .and. abs(f10-nfqso).lt.0.1) then
        if(lastrxmsg(1)%lstate) then; xdt0=lastrxmsg(1)%xdt; nqso=2; lvirtual2=.true.; endif
        if(.not.lastrxmsg(1)%lstate) then
          do i=1,200
            if(trim(calldt(i)%call2).eq.trim(hiscall)) then
              xdt0=calldt(i)%dt; nqso=3; lvirtual2=.true.; exit
            endif
          enddo
        endif
      endif
    elseif (xdt.lt.-4.9) then
      if(.not.stophint .and. lapon .and. nlasttx.ge.1 .and. nlasttx.le.maxlasttx .and. abs(f10-nfqso).lt.0.1) then
        if(lastrxmsg(1)%lstate) then; xdt0=lastrxmsg(1)%xdt; nqso=3; lvirtual3=.true.; endif
        if(.not.lastrxmsg(1)%lstate) then
          do i=1,200
            if(trim(calldt(i)%call2).eq.trim(hiscall)) then
              xdt0=calldt(i)%dt; nqso=3; lvirtual3=.true.; exit
            endif
          enddo
        endif
      endif
    endif
  endif

  !call timer('ft8_down',0)
  call ft8_downsample(newdat,f1,nqso,cd0,cd2,cd3,lhighsens)   !Mix f1 to baseband and downsample
  !call timer('ft8_down',1)

  lsd=.false.; isd=1; lcq=.false.
  if(lapon) then
    if(nsec.eq.0 .or. nsec.eq.30) then
      do i=1,130
        if(.not.evencopy(i)%lstate) cycle
        if(abs(evencopy(i)%freq-f1).lt.3.0 .and. abs(evencopy(i)%dt-xdt).lt.0.19) then
          msgd=evencopy(i)%msg; lsd=.true.; isd=i
          if(msgd(1:3).eq.'CQ ' .or. msgd(1:3).eq.'DE ' .or. msgd(1:4).eq.'QRZ ') lcq=.true.
        endif
      enddo
    elseif(nsec.eq.15 .or. nsec.eq.45) then
      do i=1,130
        if(.not.oddcopy(i)%lstate) cycle
        if(abs(oddcopy(i)%freq-f1).lt.3.0 .and. abs(oddcopy(i)%dt-xdt).lt.0.19) then
          msgd=oddcopy(i)%msg; lsd=.true.; isd=i
          if(msgd(1:3).eq.'CQ ' .or. msgd(1:3).eq.'DE ' .or. msgd(1:4).eq.'QRZ ') lcq=.true.
        endif
      enddo
    endif
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
       call sync8d(cd0,idt,ctwk,0,sync,imainpass,lastsync,iqso,lcq,lcallsstd)
       if(sync.gt.smax) then
          smax=sync
          ibest=idt
       endif
    enddo
    xdt2=ibest*dt2                           !Improved estimate for DT

! Now peak up in frequency
    i0=nint(xdt2*fs2)
    smax=0.0
    do ifr=-5,5                              !Search over +/- 2.5 Hz
      delf=ifr*0.5
      dphi=twopi*delf*dt2
      phi=0.0
      do i=1,32
        ctwk(i)=cmplx(cos(phi),sin(phi))
        phi=mod(phi+dphi,twopi)
      enddo
      call sync8d(cd0,i0,ctwk,1,sync,imainpass,lastsync,iqso,lcq,lcallsstd)
      if(sync.gt.smax) then; smax=sync; delfbest=delf; endif
    enddo
    a=0.0
    a(1)=-delfbest
    call twkfreq1(cd0,NP2,fs2,a,cd0)
    xdt=xdt2
    f1=f10+delfbest                           !Improved estimate of DF
    dfqso=abs(nfqso-f1)
!write (*,"(F5.2,1x,I1,1x,F6.1,1x,a3)") xdt,imainpass,f1,'out'
    lastsync=.true.
    call sync8d(cd0,i0,ctwk,2,sync,imainpass,lastsync,iqso,lcq,lcallsstd)

16  continue
    if(iqso.eq.3) ibest=ibest+1

    syncav=3.
    snrsync=0.
    do k=1,79
      i1=ibest+(k-1)*32
      csymb=cmplx(0.0,0.0)
      if(i1.ge.0 .and. i1+31 .le. NP2) csymb=cd0(i1:i1+31)
      if((k.ge.1 .and. k.le.7) .or. (k.ge.37 .and. k.le.43) .or. (k.ge.73 .and. k.le.79)) then
        call four2a(csymb,32,1,-1,1)
        cs1(0:7)=csymb(1:8)/1e3; s81(0:7)=abs(csymb(1:8))
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

!    plev=0.0
!    do k=1000,1009; abscd=abs(cd0(k)); if(abscd.gt.plev) plev=abscd; enddo
!    plev=plev/61.0
!    do k=0,3199; xx=plev*gran(); yy=plev*gran(); cd0(k)=cd0(k) + complex(xx,yy); enddo

    do k=1,79
      i1=ibest+(k-1)*32
      csymb=cmplx(0.0,0.0)
      if(i1.ge.0 .and. i1+31 .le. NP2) csymb=cd0(i1:i1+31)
      if(syncav.lt.2.5) then
        csymb(1)=csymb(1)*1.9; csymb(32)=csymb(32)*1.9
        scr=SQRT(abs(csymb(1)))/SQRT(abs(csymb(32)))
        if(scr.gt.1.0) then; csymb(32)=csymb(32)*scr; else; if(scr.gt.1.E-16) csymb(1)=csymb(1)/scr; endif
      endif
      call four2a(csymb,32,1,-1,1)
      cs(0:7,k)=csymb(1:8)/1e3
      s8(0:7,k)=abs(csymb(1:8))
    enddo

    sp=0.
    do k=0,7; sp(k)=sum(s8(k,1:7))+sum(s8(k,18:79)); enddo
    ka=minloc(sp)-1; k=ka(1); if(k.lt.0) go to 128
    do kb=0,7
      if(kb.eq.k) cycle; spr=sp(kb)/sp(k)
      if(spr.gt.1.5) then; s8(kb,:)=s8(kb,:)/spr; sprsqr=SQRT(spr); cs(kb,:)=cs(kb,:)/sprsqr; endif
    enddo
128 continue

    if(iqso.gt.1 .and. iqso.lt.4) then; s82=SQRT(s8); go to 8; endif
    if(iqso.eq.4) go to 32

! sync quality check
    is1=0; is2=0; is3=0; nsyncscore=0; scoreratio=0.
    do k=1,7
      ip=maxloc(s8(:,k))
      if(icos7(k-1).eq.(ip(1)-1)) is1=is1+1
      ip=maxloc(s8(:,k+36))
      if(icos7(k-1).eq.(ip(1)-1)) is2=is2+1
      ip=maxloc(s8(:,k+72))
      if(icos7(k-1).eq.(ip(1)-1)) is3=is3+1
      synck=s8(icos7(k-1),k); sumk=(sum(s8(:,k))-synck)/7.0
      if(synck.gt.sumk) then; nsyncscore=nsyncscore+1; scoreratio=scoreratio+synck/sumk; endif
      synck=s8(icos7(k-1),k+36); sumk=(sum(s8(:,k+36))-synck)/7.0
      if(synck.gt.sumk) then; nsyncscore=nsyncscore+1; scoreratio=scoreratio+synck/sumk; endif
      synck=s8(icos7(k-1),k+72); sumk=(sum(s8(:,k+72))-synck)/7.0
      if(synck.gt.sumk) then; nsyncscore=nsyncscore+1; scoreratio=scoreratio+synck/sumk; endif
    enddo
! hard sync sum - max is 21
    nsync=is1+is2+is3
! bail out
    if(nsync.le.6) then; nbadcrc=1; return; endif
    scoreratio=scoreratio/nsyncscore
    if(dfqso.ge.2.0 .or. (dfqso.lt.2.0 .and. stophint)) then
      if(nsyncscore.lt.8 .or. (nsyncscore.lt.12 .and. scoreratio.lt.2.3)) then; nbadcrc=1; return; endif
    endif

32  if(lsd) then
      if(iqso.eq.4 .and. .not.ldeepsync) go to 64
      call ft8sd1(s8,itone,msgd,msg37,lft8sd,lcq)
      if(lft8sd) then
        if(nsec.eq.0 .or. nsec.eq.30) then; evencopy(isd)%lstate=.false.
        elseif(nsec.eq.15 .or. nsec.eq.45) then; oddcopy(isd)%lstate=.false.
        endif
        i3=1; n3=1; iaptype=0; nbadcrc=0; lsd=.false.; go to 2
      endif
64    if(iqso.eq.4) then
        if(.not.lcq) then
          call ft8mf1(s8,itone,msgd,msg37,lft8sd)
          if(lft8sd) then
            if(nsec.eq.0 .or. nsec.eq.30) then; evencopy(isd)%lstate=.false.
            elseif(nsec.eq.15 .or. nsec.eq.45) then; oddcopy(isd)%lstate=.false.
            endif
            i3=1; n3=1; iaptype=0; nbadcrc=0; lsd=.false.; go to 2
          endif
        else
          call ft8mfcq(s8,itone,msgd,msg37,lft8sd)
          if(lft8sd) then
            if(nsec.eq.0 .or. nsec.eq.30) then; evencopy(isd)%lstate=.false.
            elseif(nsec.eq.15 .or. nsec.eq.45) then; oddcopy(isd)%lstate=.false.
            endif
            i3=1; n3=1; iaptype=0; nbadcrc=0; lsd=.false.; go to 2
          endif
        endif
      endif
    endif
    if(iqso.eq.4) then; nbadcrc=1; exit; endif

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
          if(lft8s) then; nbadcrc=0; lft8sdec=.true.; lsdone=.true.; exit; endif
        endif
      endif
      lsdone=.true.; nbadcrc=1; cycle
    endif

    nweak=1
    if((lft8subpass .or. swl) .and. srr.lt.2.5 .and. (imainpass.eq.1 .or. imainpass.eq.3 .or. &
       imainpass.eq.4 .or. imainpass.eq.6 .or. imainpass.eq.7 .or. imainpass.eq.9)) nweak=2
! a bit better efficiency on the overcrowded bands, with subpass 7935 without subpass 7948 decodes
!    if((lft8subpass) .and. srr.lt.2.5 .and. (imainpass.eq.1 .or. imainpass.eq.3)) nweak=2
    do k1=1,nweak
      do nsym=1,3
        nt=2**(3*nsym)
        do ihalf=1,2
          do k=1,29,nsym
            if(ihalf.eq.1) ks=k+7
            if(ihalf.eq.2) ks=k+43
            do i=0,nt-1
              i1=i/64
              i2=iand(i,63)/8
              i3=iand(i,7)
              if(nsym.eq.1) then
                s2(i)=abs(cs(graymap(i3),ks))
              elseif(nsym.eq.2) then
                s2(i)=abs(cs(graymap(i2),ks)+cs(graymap(i3),ks+1))
              elseif(nsym.eq.3) then
                s2(i)=abs(cs(graymap(i1),ks)+cs(graymap(i2),ks+1)+cs(graymap(i3),ks+2))
              else
                print*,"Error - nsym must be 1, 2, or 3."
              endif
            enddo
            if(k1.eq.1 .and. srr.lt.2.5) then !  srr.lt.2.5 -19dB SNR threshold
              if(srr.gt.2.3) then 
                s2=s2**2
              else
                do kk1=0,511
                  ss1=s2(kk1)
                  if(ss1.lt.5.77) then; s2(kk1)=1+8.*ss1**2-0.12*ss1**4; else; s2(kk1)=(ss1+5.82)**2; endif
                enddo
              endif
            endif
            if(k1.eq.2 .and. srr.lt.2.5) s2=(0.5*s2)**3 ! -19dB SNR threshold
            i32=1+(k-1)*3+(ihalf-1)*87
            if(nsym.eq.1) ibmax=2; if(nsym.eq.2) ibmax=5; if(nsym.eq.3) ibmax=8
            do ib=0,ibmax
              bm=maxval(s2(0:nt-1),one(0:nt-1,ibmax-ib)) - maxval(s2(0:nt-1),.not.one(0:nt-1,ibmax-ib))
              if(i32+ib .gt.174) cycle
              if(nsym.eq.1) then
                bmeta(i32+ib)=bm
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
      call normalizebmet(bmeta,174); call normalizebmet(bmetb,174); call normalizebmet(bmetc,174)
      scalefac=2.83; llra=scalefac*bmeta; llrb=scalefac*bmetb; llrc=scalefac*bmetc
      apmag=maxval(abs(llra))*1.01

! ipass #
!------------------------------
!   1        regular decoding, nsym=1 
!   2        regular decoding, nsym=2 
!   3        regular decoding, nsym=3 
!   4        ap pass 1
!   5        ap pass 2
!   6        ap pass 3
!   7        ap pass 4
!   8        ap pass 5
!   9        ap pass 6
!  10        ap pass 7
!  11        ap pass 8
!  12        ap pass 9

! iaptype Hound off
!------------------------
!   0        cycle
!   1        CQ     ???    ???           (29+3=32 ap bits)
!   2        MyCall ???    ???           (29+3=32 ap bits)
!   3        MyCall DxCall ???           (58+3=61 ap bits)
!   4        MyCall DxCall RRR           (77 ap bits)
!   5        MyCall DxCall 73            (77 ap bits)
!   6        MyCall DxCall RR73          (77 ap bits)

! naptypes(nQSOProgress, extra decoding pass)
!  data naptypes(0,1:12)/0,0,0,2,2,2,1,1,1,31,36,35/ ! Tx6 CQ
!  data naptypes(1,1:12)/3,3,3,2,2,2,1,1,1,31,36,35/ ! Tx1 Grid
!  data naptypes(2,1:12)/3,3,3,2,2,2,1,1,1,31,36,35/ ! Tx2 Report
!  data naptypes(3,1:12)/3,3,3,4,5,6,0,0,0,31,36,35/ ! Tx3 RRreport
!  data naptypes(4,1:12)/3,3,3,4,5,6,0,0,0,31,36,35/ ! Tx4 RRR,RR73
!  data naptypes(5,1:12)/3,3,3,2,2,2,1,1,1,31,36,35/ ! Tx5 73


! iaptype standard DxCall tracking, also valid in Hound mode
!------------------------
!   31        CQ  DxCall Grid(???)     (77 ap bits)
!   35        ??? DxCall 73            (29+19 ap bits)
!   36        ??? DxCall RR73          (29+19 ap bits)

! iaptype Hound off DXCall is not empty and is nonstandard
!------------------------
!   0         cycle
!   1         CQ     ???    ???            (29+3=32 ap bits)
!   11        MyCall <DxCall> ???          (58 ap bits) REPORT/RREPORT
!   12       <MyCall> DxCall RRR           (77 ap bits)
!   13       <MyCall> DxCall 73            (77 ap bits)
!   14       <MyCall> DxCall RR73          (77 ap bits)
!   31        CQ  DxCall                   (77 ap bits) ! full compound or just nonstandard callsign
!   35        ??? DxCall 73                (64 ap bits) ! full compound or just nonstandard callsign
!   36        ??? DxCall RR73              (64 ap bits) ! full compound or just nonstandard callsign

! ndxnsaptypes(nQSOProgress, extra decoding pass)
!  data ndxnsaptypes(0,1:14)/1,1,1,31,31,0,36,36,0,0,31,36,35,0/       ! Tx6 CQ
!  data ndxnsaptypes(1,1:14)/11,11,11,1,1,1,31,36,0,0,31,36,35,0/      ! Tx1 Grid
!  data ndxnsaptypes(2,1:14)/11,11,11,1,1,1,31,36,0,0,31,36,35,0/      ! Tx2 Report
!  data ndxnsaptypes(3,1:14)/11,11,11,13,13,13,14,14,14,12,31,36,35,1/ ! Tx3 RRreport
!  data ndxnsaptypes(4,1:14)/11,11,11,13,13,13,14,14,14,12,31,36,35,1/ ! Tx4 RRR,RR73
!  data ndxnsaptypes(5,1:14)/14,14,14,13,13,13,1,1,1,12,31,36,35,0/    ! Tx5 73


! iaptype Hound mode
!------------------------
!    0        cycle
!   21        BaseMyCall BaseDxCall ???    (58+3=61 ap bits) Report
!   22        ??? RR73; MyCall <???> ???   (28+6=34 ap bits)
!   23        BaseMyCall BaseDxCall RR73   (77 ap bits)
!   24        MyCall RR73; ??? <???> ???   (28+6=34 ap bits)
!   31        CQ  DxCall (DXGrid)          (77 ap bits) ! standard/full compound or just nonstandard callsign
!   36        ??? DxCall RR73              (29+19/64 ap bits) ! standard/ full compound or just nonstandard callsign

! nhaptypes(nQSOProgress, extra decoding pass)
!  data nhaptypes(0,1:14)/0,0,0,0,0,0,0,0,0,0,0,0,31,36/ ! Tx6 CQ, possible in idle mode
!  data nhaptypes(1,1:14)/21,21,21,22,22,22,0,0,0,0,0,0,31,36/ ! Tx1 Grid !!! to add iaptype 5,6
!  data nhaptypes(2,1:14)/0,0,0,0,0,0,0,0,0,0,0,0,31,36/ ! Tx2 none
!  data nhaptypes(3,1:14)/21,21,21,22,22,22,23,23,23,24,24,24,31,36/ ! Tx3 RRreport
!  data nhaptypes(4,1:14)/0,0,0,0,0,0,0,0,0,0,0,0,31,36/ ! Tx4 none
!  data nhaptypes(5,1:14)/0,0,0,0,0,0,0,0,0,0,0,0,31,36/ ! Tx5 none

      npasses=3
! iaptype 31,35,36 DX Call searching
      if(lapon) then
        if(lhound) then; npasses=17 ! nhaptypes
        else if(.not.lhound .and. (lhiscallstd .or. .not.lhiscallstd .and. len(trim(hiscall)).lt.3)) &
               then; npasses=15 ! naptypes
        else if(.not.lhound .and. lmycallstd .and. .not.lhiscallstd .and. len(trim(hiscall)).gt.2) then; npasses=17 ! ndxnsaptypes
        else if(.not.lhound .and. .not.lmycallstd) then; npasses=3
        endif
      else; npasses=3; endif ! drop AP masks for special messages if there is no TX activity

      do ipass=1,npasses
        if(ipass.lt.4) then
          apmask=0; iaptype=0
          if(ipass.eq.1) then; llrd=llra; else if(ipass.eq.2) then; llrd=llrb; else if(ipass.eq.3) then; llrd=llrc; endif
        else
          if(.not.lhound .and. (lhiscallstd .or. .not.lhiscallstd .and. len(trim(hiscall)).lt.3)) then
            iaptype=naptypes(nQSOProgress,ipass-3); if(iaptype.eq.0) cycle
            if(lqsomsgdcd .and. iaptype.ge.3 .and. iaptype.lt.31) cycle ! QSO message already decoded
            if(.not.lapmyc .and. iaptype.eq.2) cycle ! skip AP for 'mycall ???? ????' in 2..3 minutes after last TX
            if(stophint .and. iaptype.gt.2 .and. iaptype.lt.31) cycle
            if(lft8sdec .and. iaptype.ge.3 .and. iaptype.lt.31) cycle !already decoded

            if(iaptype.ge.3 .and. iaptype.lt.31 .and. (abs(f1-nfqso).gt.napwid .and. abs(f1-nftx).gt.napwid)) cycle
            if(iaptype.gt.30 .and. (.not.lenabledxcsearch .or. len(trim(hiscall)).lt.3)) cycle ! in QSO or TXing CQ or last logged is DX Call: searching disabled
            if(iaptype.gt.30 .and. .not.lwidedxcsearch .and. (abs(f1-nfqso).gt.napwid .and. abs(f1-nftx).gt.napwid)) cycle ! only RX freq DX Call searching
            if(iaptype.ge.2 .and. iaptype.lt.31 .and. apsym(1).gt.1) cycle  ! No, or nonstandard, mycall 
            if(iaptype.ge.3 .and. apsym(30).gt.1) cycle ! No, or nonstandard, dxcall

            llrd=llra; if(iaptype.gt.30) llrd=llrc

            if(iaptype.eq.1) then
              if(.not.swl .and. ipass.eq.10) then
                scqlev=0.; do i4=1,9; scqlev=scqlev+s8(idtone25(2,i4),i4+7); enddo
                snoiselev=(sum(s8(0:7,8:16))-scqlev)/7.0
                scqnr(nthr)=scqlev/snoiselev
                if(scqnr(nthr).lt.1.0) cycle
                llrd=llrc
              endif
              if(swl .and. ipass.eq.10) llrd=llrc
              if(ipass.eq.12) then
                if(.not.swl .and. (lft8lowth .or. lft8subpass)) then
                  if(scqnr(nthr).gt.1.2) then; llrd=llrb; else; cycle; endif
                endif
                if(swl) llrd=llrb
                if(.not.swl .and. .not. lft8subpass .and. .not.lft8lowth) then
                  if(scqnr(nthr).gt.1.3) then; llrd=llrb; else; cycle; endif
                endif
              endif
            endif

            if(iaptype.eq.2) then
              if(.not.swl .and. ipass.eq.7) then
                smyclev=0.; do i4=1,9; smyclev=smyclev+s8(idtonemyc(i4),i4+7); enddo
                snoiselev=(sum(s8(0:7,8:16))-smyclev)/7.0
                smycnr(nthr)=smyclev/snoiselev
                if(smycnr(nthr).lt.1.0) cycle
                llrd=llrc
              endif
              if(swl .and. ipass.eq.7) llrd=llrc
              if(ipass.eq.9) then
                if(.not.swl .and. (lft8lowth .or. lft8subpass)) then
                  if(smycnr(nthr).gt.1.2) then; llrd=llrb; else; cycle; endif
                endif
                if(swl) llrd=llrb
                if(.not.swl .and. .not. lft8subpass .and. .not.lft8lowth) cycle
              endif
            endif

            if(iaptype.eq.3) then
              if(ipass.eq.4) then
                smyclev=0.; do i4=1,9; smyclev=smyclev+s8(idtonemyc(i4),i4+7); enddo
                snoiselev=(sum(s8(0:7,8:16))-smyclev)/7.0
                smycnr(nthr)=smyclev/snoiselev
                if(smycnr(nthr).lt.1.0) cycle
                llrd=llrc
              else if(ipass.eq.6) then; if(smycnr(nthr).gt.1.2) then; llrd=llrb; else; cycle; endif
              endif
            endif

            apmask=0
            if(iaptype.eq.1) then ! CQ
              apmask(1:29)=1; llrd(1:29)=apmag*mcq(1:29); apmask(75:77)=1; llrd(75:76)=apmag*(-1); llrd(77)=apmag*(+1)
            else if(iaptype.eq.2) then ! MyCall,???,???
              apmask(1:29)=1; llrd(1:29)=apmag*apsym(1:29); apmask(75:77)=1; llrd(75:76)=apmag*(-1); llrd(77)=apmag*(+1)
            else if(iaptype.eq.3) then ! MyCall,DxCall,??? 
              apmask(1:58)=1; llrd(1:58)=apmag*apsym; apmask(75:77)=1; llrd(75:76)=apmag*(-1); llrd(77)=apmag*(+1)
            else if(iaptype.eq.4 .or. iaptype.eq.5 .or. iaptype.eq.6) then  
              apmask(1:77)=1; llrd(1:58)=apmag*apsym ! mycall, hiscall, RRR|73|RR73
              if(iaptype.eq.4) llrd(59:77)=apmag*mrrr; if(iaptype.eq.5) llrd(59:77)=apmag*m73
              if(iaptype.eq.6) llrd(59:77)=apmag*mrr73 
            else if(iaptype.eq.31) then ! CQ  DxCall Grid(???)
              apmask(1:77)=1; llrd(1:77)=apmag*apcqsym
            else if(iaptype.eq.35) then ! ??? DxCall 73
              apmask(30:77)=1; llrd(30:58)=apmag*apsym(30:58); llrd(59:77)=apmag*m73
            else if(iaptype.eq.36) then ! ??? DxCall RR73
              apmask(30:77)=1; llrd(30:58)=apmag*apsym(30:58); llrd(59:77)=apmag*mrr73
            endif

          else if(.not.lhound .and. lmycallstd .and. .not.lhiscallstd .and. len(trim(hiscall)).gt.2) then
            iaptype=ndxnsaptypes(nQSOProgress,ipass-3); if(iaptype.eq.0) cycle
            if(lqsomsgdcd .and. iaptype.gt.0 .and. iaptype.lt.15) cycle ! QSO message already decoded
            if(.not.lapmyc .and. iaptype.gt.1 .and. iaptype.lt.15) cycle ! skip AP for mycall in 2..3 minutes after last TX
            if(iaptype.gt.30 .and. .not.lenabledxcsearch) cycle ! in QSO or TXing CQ or last logged is DX Call: searching disabled
            if(iaptype.gt.30 .and. .not.lwidedxcsearch .and. (abs(f1-nfqso).gt.napwid .and. abs(f1-nftx).gt.napwid)) cycle ! only RX freq DX Call searching

!!!        if(stophint .and. iaptype.gt.0 .and. iaptype.lt.5) cycle !!! to check stophint functionality
!        if(lft8sdec .and. iaptype.gt.0 .and. iaptype.lt.5) cycle ! already decoded ! but may be false FT8S decode

            if(iaptype.gt.1 .and. iaptype.lt.15 .and. abs(f1-nfqso).gt.napwid .and. abs(f1-nftx).gt.napwid) cycle

            if(ipass.eq.4 .or. ipass.eq.7 .or. ipass.eq.10) then; llrd=llra
            else if(ipass.eq.5 .or. ipass.eq.8 .or. ipass.eq.11) then; llrd=llrb
            else if(ipass.eq.6 .or. ipass.eq.9 .or. ipass.gt.11) then; llrd=llrc
            endif

            apmask=0
            if(iaptype.eq.1) then ! CQ
              apmask(1:29)=1; llrd(1:29)=apmag*mcq(1:29); apmask(75:77)=1; llrd(75:76)=apmag*(-1); llrd(77)=apmag*(+1)
            else if(iaptype.eq.11) then ! MyCall <DxCall> ???  ! report rreport
              apmask(1:58)=1; llrd(1:58)=apmag*apsymdxns1; apmask(75:77)=1; llrd(75:76)=apmag*(-1); llrd(77)=apmag*(+1)
            else if(iaptype.eq.12 .or. iaptype.eq.13 .or. iaptype.eq.14) then  ! i3=4, to rework mrrr m73 mrr73
              apmask(1:77)=1; llrd(1:58)=apmag*apsymdxns2 ! <MyCall> DxCall RRR|73|RR73
              if(iaptype.eq.12) llrd(59:77)=apmag*mrrr; if(iaptype.eq.13) llrd(59:77)=apmag*m73 
              if(iaptype.eq.14) llrd(59:77)=apmag*mrr73 
! <WA9XYZ> PJ4/KA1ABC RR73             13 58 1 2            74   Nonstandard call
! <WA9XYZ> PJ4/KA1ABC 73               13 58 1 2            74   Nonstandard call
            else if(iaptype.eq.31) then ! CQ  DxCall ! full compound or nonstandard
              apmask(1:77)=1; llrd(1:77)=apmag*apcqsym
            else if(iaptype.eq.35) then ! ??? DxCall 73 ! full compound or nonstandard
              apmask(14:77)=1; llrd(14:77)=apmag*apsymdxns73(14:77)
            else if(iaptype.eq.36) then ! ??? DxCall RR73 ! full compound or nonstandard
              apmask(14:77)=1; llrd(14:77)=apmag*apsymdxnsrr73(14:77)
            endif

          else if(.not.lhound .and. .not.lmycallstd .and. .not.lhiscallstd .and. len(trim(hiscall)).gt.2) then ! empty calls or compound/nonstandard calls
            iaptype=ndxnsaptypes(nQSOProgress,ipass-3); if(iaptype.eq.0) cycle
            if(lqsomsgdcd .and. iaptype.gt.1 .and. iaptype.lt.15) cycle ! QSO message already decoded

            if(iaptype.gt.1) cycle; if(ipass.gt.3) llrd=llrc ! temporary solution, need to add AP masks here

            if(iaptype.eq.1) then ! CQ
              apmask=0; apmask(1:29)=1; llrd(1:29)=apmag*mcq(1:29); apmask(75:77)=1; llrd(75:76)=apmag*(-1)
              llrd(77)=apmag*(+1)
            endif

          else if(lhound) then
            iaptype=nhaptypes(nQSOProgress,ipass-3); if(iaptype.eq.0) cycle
            if(lqsomsgdcd .and. iaptype.gt.0 .and. iaptype.lt.25) cycle ! QSO message already decoded
            if(.not.lapmyc .and. iaptype.gt.0 .and. iaptype.lt.25) cycle ! skip AP for mycall in 2..3 minutes after last TX
            if(iaptype.gt.30 .and. .not.lenabledxcsearch) cycle ! in QSO or TXing CQ or last logged is DX Call: searching disabled
!          if(lft8sdec .and. iaptype.gt.0 .and. iaptype.lt.25) cycle ! already decoded ! but may be false FT8S decode

            if((iaptype.eq.21 .or. iaptype.eq.23) .and. apsym(30).gt.1) cycle ! No dxcall 
            fdelta=abs(f1-nfqso); fdeltam=modulo(fdelta,60.)
            if(lapon .and. nQSOProgress.gt.0 .and. iaptype.lt.31 .and. (fdelta.gt.245.0 .or. fdeltam.gt.3.0)) cycle ! AP shall be applied to Fox frequencies
            if(iaptype.gt.30 .and. .not.lwidedxcsearch .and. (fdelta.gt.245.0 .or. fdeltam.gt.3.0)) cycle ! only Fox frequencies DX Call searching

            if(ipass.eq.4 .or. ipass.eq.7 .or. ipass.eq.10 .or. ipass.eq.13) then; llrd=llra
            else if(ipass.eq.5 .or. ipass.eq.8 .or. ipass.eq.11 .or. ipass.eq.14) then; llrd=llrb
            else if(ipass.eq.6 .or. ipass.eq.9 .or. ipass.eq.12 .or. ipass.gt.14) then; llrd=llrc
            endif

            apmask=0
            if(iaptype.eq.21) then ! MyBaseCall DxBaseCall ???  ! report
              apmask(1:58)=1; llrd(1:58)=apmag*apsym; apmask(75:77)=1; llrd(75:76)=apmag*(-1); llrd(77)=apmag*(+1)
            else if(iaptype.eq.22) then ! ??? RR73; MyCall <???> ??? ! report
              apmask(29:66)=1; llrd(29:66)=apmag*apsymsp(29:66); apmask(72:77)=1; llrd(72:73)=apmag*(-1)
              llrd(74)=apmag*(+1); llrd(75:77)=apmag*(-1)
            else if(iaptype.eq.23) then ! MyBaseCall DxBaseCall RR73
              apmask(1:77)=1; llrd(1:58)=apmag*apsym; llrd(59:77)=apmag*mrr73
            else if(iaptype.eq.24) then ! MyCall RR73; ??? <???> ???
              apmask(1:28)=1; apmask(57:66)=1; llrd(1:28)=apmag*apsymsp(1:28); llrd(57:66)=apmag*apsymsp(57:66)
              apmask(72:77)=1; llrd(72:73)=apmag*(-1); llrd(74)=apmag*(+1); llrd(75:77)=apmag*(-1)
            else if(iaptype.eq.31) then ! CQ  DxCall Grid(???)
              apmask(1:77)=1; llrd(1:77)=apmag*apcqsym
            else if(iaptype.eq.36) then
              if(lhiscallstd .or. (.not.lhiscallstd .and. len(trim(hiscall)).gt.2 .and. index(hiscall,"/").gt.0)) then
                apmask(30:77)=1; llrd(30:58)=apmag*apsym(30:58); llrd(59:77)=apmag*mrr73 ! ??? DxBaseCall RR73
! (noncompound .and. nonstandard) Fox callsign being not supported by Fox WSJT-X
!            else if(.not.lhiscallstd .and. len(trim(hiscall)).gt.2) then
!              apmask=0; apmask(14:77)=1; llrd(14:77)=apmag*apsymdxnsrr73(14:77) ! ??? DxCall RR73 !  noncompound and nonstandard
              endif
            endif

          else; cycle ! fallback
          endif
        endif

        cw=0
!        call timer('bpd174_91 ',0)
        call bpdecode174_91(llrd,apmask,max_iterations,message77,cw,nharderrors,  &
             niterations)
!        call timer('bpd174_91 ',1)
        dmin=0.0
        if(((.not.filter .and. ndepth.eq.3) .or. (filter .and. nft8filtdepth.eq.3)) .and. nharderrors.lt.0) then
          ndeep=3
          if((dfqso.le.napwid .or. abs(nftx-f1).le.napwid) .and. .not.nagainfil) ndeep=4
!          if(nagainfil .or. swl) ndeep=5 ! 30 against 26 -23dB, more than 15sec to decode and many false decodes
!          if(swl) ndeep=4 ! 29 decodes -23dB, 7..12sec to decode
          if(nagainfil) ndeep=5
!print *,omp_get_nested(),OMP_get_num_threads()
!          call timer('osd174_91 ',0)
          call osd174_91(llrd,apmask,ndeep,message77,cw,nharderrors,dmin,nthr)
!          call timer('osd174_91 ',1)
        endif
        nbadcrc=1; msg37=''
        if(count(cw.eq.0).eq.174) cycle           !Reject the all-zero codeword
        if(nharderrors.lt.0 .or. nharderrors+dmin.ge.60.0 .or. (sync.lt.2.0 .and. nharderrors.gt.35) &
          .or. (ipass.gt.2 .and. nharderrors.gt.39)) then

          if(.not.lapon .or. k1.eq.2) cycle

          if(lapon) then
            if(lqsothread .and. (.not.lhound .and. iaptype.ge.3 .or. lhound .and. (iaptype.eq.21 .or. iaptype.eq.23)) &
               .and. .not.lsdone) then
              if(.not.lqsomsgdcd .and. .not.(.not.lmycallstd .and. .not.lhiscallstd)) then
                if(.not.lft8sdec .and. .not.stophint .and. dfqso.lt.2.0) then
                  call ft8s(s8,srr,itone,msg37,lft8s,nft8rxfsens,stophint)
                  if(lft8s) then; nbadcrc=0; lft8sdec=.true.; endif
                endif
              endif
              lsdone=.true.
            endif
            if(nbadcrc.eq.0) then; i3=1; n3=1; exit; endif

            if(lsd .and. ipass.eq.3 .and. nbadcrc.eq.1 .and. srr.lt.7.0) then ! low DR setups shall not try FT8SD for strong signals
              call ft8sd(s8,srr,itone,msgd,msg37,lft8sd,lcq)
              if(lft8sd) then
                if(nsec.eq.0 .or. nsec.eq.30) then; evencopy(isd)%lstate=.false.
                elseif(nsec.eq.15 .or. nsec.eq.45) then; oddcopy(isd)%lstate=.false.
                endif
                i3=1; n3=1; iaptype=0; nbadcrc=0; lsd=.false.; exit
              endif
            endif
            if(nbadcrc.eq.1) cycle 
          endif
        endif

        write(c77,'(77i1)') message77
        read(c77(72:74),'(b3)') n3
        read(c77(75:77),'(b3)') i3
        if(i3.gt.4 .or. (i3.eq.0 .and. n3.gt.5)) cycle
!print *,i3,n3,iaptype
        mycall13=mycall//' '; dxcall13=hiscall//' '
!$omp critical(unpack77)
        call unpack77(c77,1,msg37,unpk77_success)
!$omp end critical(unpack77)
        if(.not.unpk77_success) then
          if(lqsothread .and. lapon .and. (.not.lhound .and. iaptype.ge.3 .or. lhound .and. &
             (iaptype.eq.21 .or. iaptype.eq.23)) .and. .not.lsdone) then
            if(.not.lqsomsgdcd .and. .not.(.not.lmycallstd .and. .not.lhiscallstd)) then
              if(.not.lft8sdec .and. .not.stophint .and. dfqso.lt.2.0) then
                call ft8s(s8,srr,itone,msg37,lft8s,nft8rxfsens,stophint)
                if(lft8s) then; nbadcrc=0; lft8sdec=.true.; endif
              endif
            endif
            lsdone=.true.
            if(nbadcrc.eq.0) then; i3=1; n3=1; exit; endif
          endif
          cycle
        endif
        if(iaptype.eq.1 .and. msg37(1:10).eq.'CQ DE AA00') then; nbadcrc=1; cycle; endif
!print *,i3,n3,msg37
        nbadcrc=0  ! If we get this far: valid codeword, valid (i3,n3), nonquirky message.
        call get_tones_from_77bits(message77,itone)

! 0.1  K1ABC RR73; W9XYZ <KH1/KH7Z> -11   28 28 10 5       71   DXpedition Mode
        i3bit=0; if(i3.eq.0 .and. n3.eq.1) i3bit=1
!        iFreeText=message77(57)
! 0.0  Free text
        if(i3.eq.0 .and. n3.eq.0) then; lFreeText=.true.; else; lFreeText=.false.; endif
! delete braces
        if(.not.lFreeText .and. i3bit.ne.1 .and. index(msg37,'<').gt.0) then
          lhashmsg=.true.
          ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
          ispc3=index(msg37((ispc2+1):),' ')+ispc2
          ieoc1=ispc1-1
          iboc2=ispc1+1; ieoc2=ispc2-1
          if(msg37(1:1).eq.'<' .and. msg37(2:2).ne.'.') then
            msg37(ieoc1:37)=msg37(ieoc1+1:37)//' '
            msg37(1:37)=msg37(2:37)//' '
          else if(msg37(iboc2:iboc2).eq.'<' .and. msg37(iboc2+1:iboc2+1).ne.'.') then
            msg37(ieoc2:37)=msg37(ieoc2+1:37)//' '
            msg37(iboc2:37)=msg37(iboc2+1:37)//' '
          else
            iboc3=ispc2+1; ieoc3=ispc3-1
            if(msg37(iboc3:iboc3).eq.'<' .and. msg37(iboc3+1:iboc3+1).ne.'.') then
              msg37(ieoc3:37)=msg37(ieoc3+1:37)//' '
              msg37(iboc3:37)=msg37(iboc3+1:37)//' '
            endif
          endif
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
      if(dfqso.lt.2.0) lqsomsgdcd=.true.; !$OMP FLUSH (lqsomsgdcd)
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
      go to 4 ! bypass checking to false decode
    endif
!print *,qual,msg37
    rxdt=xdt-0.5
    if(qual.lt.0.39 .or. xsnr.lt.-20.5 .or. rxdt.lt.-0.5 .or. rxdt.gt.1.9) then
      if(i3bit.eq.1) then; call chkspecial8(msg37,msg37_2,nbadcrc)
      else; call chkfalse8(msg37,i3,n3,nbadcrc,iaptype); endif
      if(nbadcrc.eq.1) then; msg37=''; return; endif
    endif
! still some false decodes can come around the thresholds, will focus on ' R ' in the message
! i3=2 'JC6OFB VF3BXC/P R GQ99'
!print *,i3,n3,msg37
!print *,iaptype,msg37
    if(i3.ge.1 .and. i3.le.3 .and. (qual.lt.0.6 .or. xsnr.lt.-22.0 .or. rxdt.lt.-0.5 .or. rxdt.gt.1.0) .and. &
       index(msg37,' R ').gt.0) then
!print *,msg37
      islash1=index(msg37,'/')
      call_a=''; call_b=''
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
      if(islash1.le.0 .and. ispc1.gt.3 .and. ispc2.gt.7) then
        call_a=msg37(1:ispc1-1); call_b=msg37(ispc1+1:ispc2-1)
        include 'call_q1.f90'
        falsedec=.false.
        call chkflscall(call_a,call_b,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif

      if(islash1.gt.0 .and. ispc1.gt.3 .and. ispc2.gt.7) then
        islash2=index(msg37((islash1+1):),'/')+islash1
        if(islash1.gt.ispc1) then
          call_a=msg37(1:ispc1-1); call_b=msg37(ispc1+1:islash1-1)
        else
          call_a=msg37(1:islash1-1)
          if(islash2.gt.islash1) then; call_b=msg37(ispc1+1:islash2-1); else; call_b=msg37(ispc1+1:ispc2-1); endif
        endif
        include 'call_q1.f90'
        falsedec=.false.
        call chkflscall(call_a,call_b,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif
    endif

! prior to subtraction we need to parse message below as 'TU DE 632TGU' + 'QV4UPP 632TGU 529 xxxx'
! i3=3 n3=4 'TU; 6C6VOU IQ5NVQ 599 71' 		  
! i3=3 n3=3 'TU; QV4UPP 632TGU 529'
! 'TU; D47IAQ <...> 559 032'
! 'TU; G3AAA K1ABC R 569 MA'
    if(i3.eq.3 .and. (n3.eq.3 .or. n3.eq.4) .and. msg37(1:3).eq.'TU;') then
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1 
      ispc3=index(msg37((ispc2+1):),' ')+ispc2
      call_a=''; call_b=''; call_a=msg37(ispc1+1:ispc2-1); call_b=msg37(ispc2+1:ispc3-1)
! check for false
      falsedec=.false.
      call chkflscall(call_a,call_b,falsedec)
      if(falsedec) then; nbadcrc=1; msg37=''; return; endif
! parse
      lspecial=.true.
      msg37_2=msg37(5:37)//'    '
      msg37=''; msg37='DE '//trim(call_b)//' TU'
    endif

! -24 -1.3 3013 ~ CQ N99ZCZ FF86          *
! -23  2.8 2165 ~ CQ 3G0ZDC EL07          *
! -24 -1.8 2389 ~ CQ N9OAT/R RP25         *
    if(iaptype.eq.1 .and. i3.eq.1 .and. xsnr.lt.-22.5 .and. (rxdt.lt.-0.5 .or. rxdt.gt.1.9) .and. &
       msg37(1:3).eq.'CQ ') then
      ispc1=index(msg37,' '); ispc2=index(msg37((ispc1+1):),' ')+ispc1
      if(ispc2.gt.6) then
        call_b='            '
        call_b=msg37(ispc1+1:ispc2-1)
        nlencallb=len_trim(call_b)
        if(call_b(nlencallb-1:nlencallb).eq.'/R') then; nbadcrc=1; msg37=''; return; endif
        falsedec=.false.
        call chkflscall('CQ          ',call_b,falsedec)
        if(falsedec) then; nbadcrc=1; msg37=''; return; endif
      endif
    endif

! We do not support contest message decoding by FT8 AP under iaptype=2, consider as false decode
! EA1AHY M83WN/R R QA79   *
    if(i3.eq.1 .and. n3.eq.7 .and. iaptype.eq.2) then; nbadcrc=1; msg37=''; return; endif

4   ldupemsg=.false.
    if(ndecodes.gt.0) then
      do i=1,ndecodes; if(allmessages(i).eq.msg37 .and. abs(allfreq(i)-f1).lt.45.0) ldupemsg=.true.; enddo
    endif

    if(.not.ldupemsg .and. dfqso.lt.2.0 .and. ((i3.eq.1 .and. .not.lft8s) .or. lft8s)) then
      if(msg37(1:msgrootlen+1).eq.trim(msgroot)//' ') then
        lasthcall=hiscall; lastrxmsg(1)%lastmsg=msg37; lastrxmsg(1)%xdt=xdt-0.5; lastrxmsg(1)%lstate=.true.
        lqsomsgdcd=.true.
!$OMP FLUSH (lqsomsgdcd)
      else if(.not.lft8s .and. msg37(1:mycalllen1).ne.trim(mycall)//' ' .and. &
              index(msg37,' '//trim(hiscall)//' ').gt.0) then
        lrepliedother=.true.
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

    if(.not.ldupemsg .and. msg37(1:mycalllen1).eq.trim(mycall)//' ') then
!$omp critical(update_incall)
      incall(20:2:-1)=incall(20-1:1:-1); incall(1)%msg=msg37; incall(1)%xdt=xdt-0.5
!$omp end critical(update_incall)
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
!print *,'iaptype',iaptype
!print *,i3,n3

! protocol violation
!071445 -14  0.1  779 ~ <...>
!071445 -14  0.1  779 ~ W5JZ  ! hash was associated
    if(i3.eq.4 .and. n3.eq.0 .and. len_trim(msg37).lt.9) then; nbadcrc=1; msg37=''; endif
    if(i3.eq.4 .and. msg37(1:8).eq."CQ <...>") then; nbadcrc=1; msg37=''; endif ! false decode

    if(lsubtract .and. .not.ldupemsg) then
      if(nthr.eq.1) then; call subtractft81(itone,f1,xdt2)
      else if(nthr.eq.2) then; call subtractft82(itone,f1,xdt2)
      else if(nthr.eq.3) then; call subtractft83(itone,f1,xdt2)
      else if(nthr.eq.4) then; call subtractft84(itone,f1,xdt2)
      else if(nthr.eq.5) then; call subtractft85(itone,f1,xdt2)
      else if(nthr.eq.6) then; call subtractft86(itone,f1,xdt2)
      else if(nthr.eq.7) then; call subtractft87(itone,f1,xdt2)
      else if(nthr.eq.8) then; call subtractft88(itone,f1,xdt2)
      else if(nthr.eq.9) then; call subtractft89(itone,f1,xdt2)
      else if(nthr.eq.10) then; call subtractft810(itone,f1,xdt2)
      else if(nthr.eq.11) then; call subtractft811(itone,f1,xdt2)
      else if(nthr.eq.12) then; call subtractft812(itone,f1,xdt2)
      endif
    endif

    if(lhidehash .and. index(msg37,'<...>').gt.6) then; nbadcrc=1; msg37=''; msg37_2=''; endif

!if(lapon .and. nbadcrc.eq.0 .and. iaptype.ge.2 .and. iaptype.le.3) then
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