subroutine multimode_decoder(params)

  !$ use omp_lib
  use prog_args
!  use timer_module, only: timer
  use jt65_decode
  use jt9_decode
  use jt9s_decode
  use jt10_decode
  use ft8_decode
  use ft4_decode
  use ft8_mod1, only : ndecodes,allmessages,allsnrs,allfreq,mycall12_0,mycall12_00,hiscall12_0,nmsg,odd,even,oddcopy,     &
                       evencopy,nlasttx,lqsomsgdcd,mycalllen1,msgroot,msgrootlen,lapmyc,lagcc,sumxdtt,avexdt,             &
                       nfawide,nfbwide,mycall,hiscall,lhound,mybcall,hisbcall,lenabledxcsearch,lwidedxcsearch,hisgrid4,   &
                       lmultinst,dd8,nft8cycles,nft8swlcycles,lskiptx1,ncandallthr,nincallthr,incall,msgincall,xdtincall, &
                       maskincallthr,ltxing
  use ft4_mod1, only : llagcc,nFT4decd,nfafilt,nfbfilt,lfilter,lhidetest,lhidetelemetry,dd4
  use packjt77, only : lcommonft8b,ihash22,calls12,calls22

  include 'jt9com.f90'
!  include 'timer_common.inc'

  type, extends(jt65_decoder) :: counting_jt65_decoder
     integer :: decoded
  end type counting_jt65_decoder

  type, extends(jt9_decoder) :: counting_jt9_decoder
     integer :: decoded
  end type counting_jt9_decoder

  type, extends(jt9s_decoder) :: counting_jt9s_decoder
     integer :: decoded
  end type counting_jt9s_decoder
  
  type, extends(jt10_decoder) :: counting_jt10_decoder
     integer :: decoded
  end type counting_jt10_decoder
  
  type, extends(ft8_decoder) :: counting_ft8_decoder
     integer :: decoded
     real :: xdtt(200)
  end type counting_ft8_decoder

  type, extends(ft4_decoder) :: counting_ft4_decoder
     integer :: decoded
  end type counting_ft4_decoder

  logical first,firstsd
  logical(1) swlold,lhoundprev
  integer nutc,ndelay
  type(params_block) :: params
  data ndelay/0/
  data first/.true./
  data firstsd/.true./
  data swlold/.false./
  data lhoundprev/.false./
!  character(len=20) :: datetime
  character(len=6) :: hisgrid !, mygrid,
  save

  logical newdat65,newdat9,nagainjt9,nagainjt9s,nagainjt10,swlchanged,lowrms,fileExists

!character(10) dat, tim1, tim2, zon
!real(8) :: timer1,timer2 ! milliseconds
!integer(4) :: ival(8)

  type(counting_jt65_decoder) :: my_jt65
  type(counting_jt9_decoder) :: my_jt9
  type(counting_jt9s_decoder) :: my_jt9s
  type(counting_jt10_decoder) :: my_jt10
  type(counting_ft8_decoder) :: my_ft8
  type(counting_ft4_decoder) :: my_ft4
  
 !cast C character arrays to Fortran character strings
!  datetime=transfer(params%datetime, datetime)
  mycall=transfer(params%mycall,mycall)
  mybcall=transfer(params%mybcall,mybcall)
  hiscall=transfer(params%hiscall,hiscall)
  hisbcall=transfer(params%hisbcall,hisbcall)
!  mygrid=transfer(params%mygrid,mygrid)
  hisgrid=transfer(params%hisgrid,hisgrid)
  hisgrid4=hisgrid(1:4)

  my_ft8%decoded=0; my_ft8%xdtt=0.
  my_jt65%decoded=0; my_jt9%decoded=0; my_jt9s%decoded=0; my_jt10%decoded=0; my_ft4%decoded=0
  nagainjt9=.false.;  nagainjt9s=.false.;  nagainjt10=.false.; ncandall=0; ncandallthr=0

  if(params%lmodechanged) then; avexdt=0.; if(params%nmode.eq.8) nintcount=3; endif ! avexdt fast track in FT8 after mode change
  if(params%lbandchanged .and. (params%nmode.eq.8 .or. params%nmode.eq.4)) then; ihash22=-1; calls22=''; calls12=''; endif

  if(.not.params%nagain) ndelay=params%ndelay
  lqsomsgdcd=.false.
  if(ndelay.gt.0) then ! received incomplete interval
    if(params%nmode.eq.8) then; call partintft8(ndelay,params%nutc); lqsomsgdcd=.true.
    else if(params%nmode.eq.4) then; call partintft4(ndelay,params%nutc)
    else; call partint(ndelay,params%nutc)
    endif
  endif
  ntrials=params%nranera
  if(params%nsecbandchanged.gt.0) then
  nsamplesdel=params%nsecbandchanged*12000
    if(params%nmode.eq.8) then
      if(params%nsecbandchanged.gt.14) then; dd8=0. ! protection
      else; dd8(1:nsamplesdel)=0.
      endif
    else if(params%nmode.eq.4) then
      if(params%nsecbandchanged.gt.6) then; dd4=0. ! protection: interval length is greater than dd4 index range
      else; dd4(1:nsamplesdel)=0.
      endif
    endif
  endif

!  if (params%nagain .or. (params%nagainfil .and. (params%nmode.eq.65 .or. &
!      params%nmode.eq.(65+9)))) then
!     open(13,file=trim(temp_dir)//'/decoded.txt',status='unknown',position='append')
!  else
!     open(13,file=trim(temp_dir)//'/decoded.txt',status='unknown')
!  endif

  if(.not.params%nagain) nutc=params%nutc
!call date_and_time(date = dat, time = tim1, zone = zon)
!call date_and_time(values = ival)
!timer1 = dble(ival(8)) * 0.001_8 + &
!dble(ival(7)) + dble(ival(6)) * 60.0_8 + &
!dble(ival(5)) * 3600.0_8
!print *,'Decoder start: ',tim1
!print *,params%newdat,params%nagain,params%nagainfil
  swlchanged=.false.
  if(swlold.neqv.params%nswl) then
     swlchanged=.true.
     swlold=params%nswl
  endif
  if(first .or. swlchanged) then; call cwfilter(params%nswl,first,swlchanged); first=.false.; endif ! + ALLCALL to memory
  lenabledxcsearch=params%lenabledxcsearch; lwidedxcsearch=params%lwidedxcsearch

  lmultinst=params%lmultinst; lskiptx1=params%lskiptx1; lhidetest=params%lhidetest; lhidetelemetry=params%lhidetelemetry
  ltxing=params%ltxing
  if(params%nmode.eq.8) then
     mycalllen1=len_trim(mycall)+1
     msgroot=''; msgroot=trim(mycall)//' '//trim(hiscall)//' '; msgrootlen=len_trim(msgroot)
     lcommonft8b=params%lcommonft8b; lagcc=params%nagcc; lhound=params%lhound
     nft8cycles=params%nft8cycles; nft8swlcycles=params%nft8swlcycles; forcedt=0.
     if(params%nagcc .or. params%lforcesync) call agccft8(params%nfa,params%nfb,params%lforcesync,forcedt)
     if((hiscall.ne.hiscall12_0 .and. hiscall.ne.'            ') &
        .or. (mycall.ne.mycall12_0 .and. mycall.ne.'            ') .or. (lhound.neqv.lhoundprev)) then
        if(hiscall.ne.'            ') then
          call tone8(params%lmycallstd,params%lhiscallstd)
          hiscall12_0=hiscall; mycall12_0=mycall
        endif
        lhoundprev=lhound
     endif
     if(params%lmycallstd .and. mycall.ne.'            ' .and. mycall12_00.ne.mycall) then
       call tone8myc(); mycall12_00=mycall
     endif
     ndecodes=0; allmessages=""; allsnrs=0; allfreq=0. !init arrays for multithreading decoding
     numcores=omp_get_num_procs()
     nuserthr=params%nmt

     numthreads=1 ! fallback
     if(nuserthr.eq.0) then ! auto
       if(numcores.eq.1) then; numthreads=1
       else if(numcores.gt.1 .and. numcores.lt.5) then; numthreads=numcores-1
       else if(numcores.gt.4 .and. numcores.lt.9) then; numthreads=numcores-2
       else if(numcores.gt.8 .and. numcores.lt.16) then; numthreads=numcores-3
       else if(numcores.gt.15 .and. numcores.lt.21) then; numthreads=numcores-4
       else if(numcores.gt.20 .and. numcores.lt.30) then; numthreads=numcores-5
       else if(numcores.gt.29) then; numthreads=24
       endif
     else if(nuserthr.gt.0 .and. nuserthr.lt.25) then
! number of threads shall not exceed number of logical cores
       if(numcores.ge.nuserthr) then; numthreads=nuserthr; else; numthreads=numcores; endif
     endif

!print *,nuserthr,numcores,numthreads
     call omp_set_dynamic(.false.)
     call omp_set_nested(.true.)

     nfa=params%nfa; nfb=params%nfb; nfqso=params%nfqso; nfawide=params%nfa; nfbwide=params%nfb
     if(params%nfilter) then  ! 160Hz Filter bandwidth, 580Hz Filter bandwidth in Hound mode
        if(nfqso.lt.nfa .or. nfqso.gt.nfb) then
           write(*,32) nutc,'nfqso is out of bandwidth','d'; 32 format(i6.6,2x,a25,16x,a1); go to 800
        endif
        if(.not.params%lhound) then; nfa=max(nfa,nfqso-60); nfb=min(nfb,nfqso+60)
        else; nfa=max(nfa,nfqso-290); nfb=min(nfb,nfqso+290); endif
        numthreads=min(8,numthreads) ! to do: withdraw limitation when threads are in sync at main passes?
     endif
     if(params%nagainfil) then
        if(nfqso.lt.nfa .or. nfqso.gt.nfb) then
           write(*,64) nutc,'nfqso is out of bandwidth','d'; 64 format(i6.6,2x,a25,16x,a1); go to 800
        endif
        nfa=max(nfa,nfqso-25) ! 50Hz bandwidth for decode via double click
        nfb=min(nfb,nfqso+25)
        numthreads=min(4,numthreads) ! to do: withdraw limitation when threads are in sync at main passes?
     endif
     nsec=mod(nutc,100)
     nmsg=0
     if(nsec.ne.0 .and. nsec.ne.15 .and. nsec.ne.30 .and. nsec.ne.45) then ! reading simulated wav file
      odd%lstate=.false.; even%lstate=.false.; oddcopy%lstate=.false.; evencopy%lstate=.false.  
     endif
     if(firstsd) then; odd%lstate=.false.; even%lstate=.false.; firstsd=.false.; endif
     if(nsec.eq.0 .or. nsec.eq.30) then
       evencopy%msg=even%msg; evencopy%freq=even%freq
       evencopy%dt=even%dt; evencopy%lstate=even%lstate
       even%lstate=.false.
     endif
     if(nsec.eq.15 .or. nsec.eq.45) then
       oddcopy%msg=odd%msg; oddcopy%freq=odd%freq
       oddcopy%dt=odd%dt; oddcopy%lstate=odd%lstate
       odd%lstate=.false.
     endif
     nlasttx=params%nlasttx; lapmyc=params%lapmyc; nFT8decd=0; sumxdt=0.0; if(params%nmode.eq.4) sumxdtt=0.0
     call fillhash(numthreads,.false.)
     if(params%nmode.eq.8) call ft8apset(params%lmycallstd,params%lhiscallstd,numthreads)
!do i=9595,9605; print *,i,dd8(i); enddo ! check wav files processing

!     call timer('decft8  ',0)
if(numthreads.eq.1) then
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
endif

if(numthreads.eq.2) then
     nfmid=nfa+nint(abs(nfb-nfa)/2.); if((nfmid+1).gt.nfb) nfmid=nfb-1
!!!  !$omp parallel sections num_threads(2) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(2) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.3) then
     nfdelta=nint(abs(nfb-nfa)/3.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; if((nfmid2+1).gt.nfb) nfmid2=nfb-1
!!!  !$omp parallel sections num_threads(3) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(3) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.4) then
     nfdelta=nint(abs(nfb-nfa)/4.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     if((nfmid3+1).gt.nfb) nfmid3=nfb-1
!!!  !$omp parallel sections num_threads(4) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(4) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.5) then
     nfdelta=nint(abs(nfb-nfa)/5.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; if((nfmid4+1).gt.nfb) nfmid4=nfb-1
!!!  !$omp parallel sections num_threads(5) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(5) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.6) then
     nfdelta=nint(abs(nfb-nfa)/6.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; if((nfmid5+1).gt.nfb) nfmid5=nfb-1
!!!  !$omp parallel sections num_threads(6) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(6) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.7) then
     nfdelta=nint(abs(nfb-nfa)/7.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; if((nfmid6+1).gt.nfb) nfmid6=nfb-1
!!!  !$omp parallel sections num_threads(7) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(7) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.8) then
     nfdelta=nint(abs(nfb-nfa)/8.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta
     if((nfmid7+1).gt.nfb) nfmid7=nfb-1
!!!  !$omp parallel sections num_threads(8) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(8) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.9) then
     nfdelta=nint(abs(nfb-nfa)/9.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta
     nfmid8=nfmid7+nfdelta; if((nfmid8+1).gt.nfb) nfmid8=nfb-1
!!!  !$omp parallel sections num_threads(9) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(9) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.10) then
     nfdelta=nint(abs(nfb-nfa)/10.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta
     nfmid8=nfmid7+nfdelta; nfmid9=nfmid8+nfdelta; if((nfmid9+1).gt.nfb) nfmid9=nfb-1
!!!  !$omp parallel sections num_threads(10) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(10) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.11) then
     nfdelta=nint(abs(nfb-nfa)/11.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; if((nfmid10+1).gt.nfb) nfmid10=nfb-1
!!!  !$omp parallel sections num_threads(11) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(11) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.12) then
     nfdelta=nint(abs(nfb-nfa)/12.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; if((nfmid11+1).gt.nfb) nfmid11=nfb-1
!!!  !$omp parallel sections num_threads(12) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(12) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.13) then
     nfdelta=nint(abs(nfb-nfa)/13.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     if((nfmid12+1).gt.nfb) nfmid12=nfb-1
!!!  !$omp parallel sections num_threads(13) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(13) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.14) then
     nfdelta=nint(abs(nfb-nfa)/14.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; if((nfmid13+1).gt.nfb) nfmid13=nfb-1
!!!  !$omp parallel sections num_threads(14) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(14) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.15) then
     nfdelta=nint(abs(nfb-nfa)/15.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; nfmid14=nfmid13+nfdelta; if((nfmid14+1).gt.nfb) nfmid14=nfb-1
!!!  !$omp parallel sections num_threads(15) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(15) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfmid14,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid14+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,15,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.16) then
     nfdelta=nint(abs(nfb-nfa)/16.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; nfmid14=nfmid13+nfdelta; nfmid15=nfmid14+nfdelta; if((nfmid15+1).gt.nfb) nfmid15=nfb-1
!!!  !$omp parallel sections num_threads(16) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(16) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfmid14,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid14+1,nfmid15,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,15,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid15+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,16,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.17) then
     nfdelta=nint(abs(nfb-nfa)/17.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; nfmid14=nfmid13+nfdelta; nfmid15=nfmid14+nfdelta; nfmid16=nfmid15+nfdelta
     if((nfmid16+1).gt.nfb) nfmid16=nfb-1
!!!  !$omp parallel sections num_threads(17) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(17) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfmid14,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid14+1,nfmid15,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,15,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid15+1,nfmid16,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,16,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid16+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,17,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.18) then
     nfdelta=nint(abs(nfb-nfa)/18.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; nfmid14=nfmid13+nfdelta; nfmid15=nfmid14+nfdelta; nfmid16=nfmid15+nfdelta
     nfmid17=nfmid16+nfdelta; if((nfmid17+1).gt.nfb) nfmid17=nfb-1
!!!  !$omp parallel sections num_threads(18) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(18) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfmid14,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid14+1,nfmid15,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,15,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid15+1,nfmid16,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,16,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid16+1,nfmid17,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,17,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid17+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,18,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.19) then
     nfdelta=nint(abs(nfb-nfa)/19.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; nfmid14=nfmid13+nfdelta; nfmid15=nfmid14+nfdelta; nfmid16=nfmid15+nfdelta
     nfmid17=nfmid16+nfdelta; nfmid18=nfmid17+nfdelta; if((nfmid18+1).gt.nfb) nfmid18=nfb-1
!!!  !$omp parallel sections num_threads(19) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(19) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfmid14,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid14+1,nfmid15,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,15,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid15+1,nfmid16,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,16,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid16+1,nfmid17,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,17,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid17+1,nfmid18,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,18,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid18+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,19,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.20) then
     nfdelta=nint(abs(nfb-nfa)/20.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; nfmid14=nfmid13+nfdelta; nfmid15=nfmid14+nfdelta; nfmid16=nfmid15+nfdelta
     nfmid17=nfmid16+nfdelta; nfmid18=nfmid17+nfdelta; nfmid19=nfmid18+nfdelta; if((nfmid19+1).gt.nfb) nfmid19=nfb-1
!!!  !$omp parallel sections num_threads(20) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(20) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfmid14,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid14+1,nfmid15,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,15,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid15+1,nfmid16,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,16,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid16+1,nfmid17,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,17,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid17+1,nfmid18,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,18,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid18+1,nfmid19,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,19,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid19+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,20,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.21) then
     nfdelta=nint(abs(nfb-nfa)/21.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; nfmid14=nfmid13+nfdelta; nfmid15=nfmid14+nfdelta; nfmid16=nfmid15+nfdelta
     nfmid17=nfmid16+nfdelta; nfmid18=nfmid17+nfdelta; nfmid19=nfmid18+nfdelta; nfmid20=nfmid19+nfdelta
     if((nfmid20+1).gt.nfb) nfmid20=nfb-1
!!!  !$omp parallel sections num_threads(21) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(21) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfmid14,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid14+1,nfmid15,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,15,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid15+1,nfmid16,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,16,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid16+1,nfmid17,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,17,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid17+1,nfmid18,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,18,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid18+1,nfmid19,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,19,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid19+1,nfmid20,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,20,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid20+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,21,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.22) then
     nfdelta=nint(abs(nfb-nfa)/22.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; nfmid14=nfmid13+nfdelta; nfmid15=nfmid14+nfdelta; nfmid16=nfmid15+nfdelta
     nfmid17=nfmid16+nfdelta; nfmid18=nfmid17+nfdelta; nfmid19=nfmid18+nfdelta; nfmid20=nfmid19+nfdelta
     nfmid21=nfmid20+nfdelta; if((nfmid21+1).gt.nfb) nfmid21=nfb-1
!!!  !$omp parallel sections num_threads(22) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(22) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfmid14,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid14+1,nfmid15,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,15,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid15+1,nfmid16,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,16,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid16+1,nfmid17,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,17,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid17+1,nfmid18,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,18,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid18+1,nfmid19,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,19,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid19+1,nfmid20,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,20,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid20+1,nfmid21,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,21,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid21+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,22,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.23) then
     nfdelta=nint(abs(nfb-nfa)/23.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; nfmid14=nfmid13+nfdelta; nfmid15=nfmid14+nfdelta; nfmid16=nfmid15+nfdelta
     nfmid17=nfmid16+nfdelta; nfmid18=nfmid17+nfdelta; nfmid19=nfmid18+nfdelta; nfmid20=nfmid19+nfdelta
     nfmid21=nfmid20+nfdelta; nfmid22=nfmid21+nfdelta; if((nfmid22+1).gt.nfb) nfmid22=nfb-1
!!!  !$omp parallel sections num_threads(23) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(23) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfmid14,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid14+1,nfmid15,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,15,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid15+1,nfmid16,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,16,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid16+1,nfmid17,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,17,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid17+1,nfmid18,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,18,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid18+1,nfmid19,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,19,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid19+1,nfmid20,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,20,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid20+1,nfmid21,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,21,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid21+1,nfmid22,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,22,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid22+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,23,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

if(numthreads.eq.24) then
     nfdelta=nint(abs(nfb-nfa)/24.); nfmid1=nfa+nfdelta; nfmid2=nfmid1+nfdelta; nfmid3=nfmid2+nfdelta
     nfmid4=nfmid3+nfdelta; nfmid5=nfmid4+nfdelta; nfmid6=nfmid5+nfdelta; nfmid7=nfmid6+nfdelta; nfmid8=nfmid7+nfdelta
     nfmid9=nfmid8+nfdelta; nfmid10=nfmid9+nfdelta; nfmid11=nfmid10+nfdelta; nfmid12=nfmid11+nfdelta
     nfmid13=nfmid12+nfdelta; nfmid14=nfmid13+nfdelta; nfmid15=nfmid14+nfdelta; nfmid16=nfmid15+nfdelta
     nfmid17=nfmid16+nfdelta; nfmid18=nfmid17+nfdelta; nfmid19=nfmid18+nfdelta; nfmid20=nfmid19+nfdelta
     nfmid21=nfmid20+nfdelta; nfmid22=nfmid21+nfdelta; nfmid23=nfmid22+nfdelta; if((nfmid23+1).gt.nfb) nfmid23=nfb-1
!!!  !$omp parallel sections num_threads(24) copyin(/timer_private/) shared(dd8,nmsg,even,odd,lqsomsgdcd,first_osd,gen,ihash22,nzhash,calls10,calls12,calls22) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(24) shared(dd8,nmsg,even,odd,lqsomsgdcd), &
  !$omp& shared(first_osd,gen,ndecodes,allmessages,allsnrs,allfreq) if(.true.) !iif() needed on Mac
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid11+1,nfmid12,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,12,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid12+1,nfmid13,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,13,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid10+1,nfmid11,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,11,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid13+1,nfmid14,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,14,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid9+1,nfmid10,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,10,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid14+1,nfmid15,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,15,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid8+1,nfmid9,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,9,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid15+1,nfmid16,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,16,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid7+1,nfmid8,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,8,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid16+1,nfmid17,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,17,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid6+1,nfmid7,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,7,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid17+1,nfmid18,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,18,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid5+1,nfmid6,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,6,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid18+1,nfmid19,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,19,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid4+1,nfmid5,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,5,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid19+1,nfmid20,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,20,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid3+1,nfmid4,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,4,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid20+1,nfmid21,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,21,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid2+1,nfmid3,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,3,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid21+1,nfmid22,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,22,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid1+1,nfmid2,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,2,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid22+1,nfmid23,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,23,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfa,nfmid1,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,1,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp section
     call my_ft8%decode(ft8_decoded,params%nQSOProgress,nfqso,params%nft8rxfsens,  &
          params%nftx,nutc,nfmid23+1,nfb,params%ncandthin,params%ndtcenter, &
          nsec,params%napwid,params%nswl,params%lmycallstd,params%lhiscallstd, &
          params%nfilter,params%nstophint,24,numthreads,logical(params%nagainfil),params%lft8lowth, &
          params%lft8subpass,params%lhideft8dupes,params%lhidehash)
  !$omp end parallel sections
endif

    do i=1,numthreads
      do m=1,nincallthr(i)
      nindex=maskincallthr(i)+m
      incall(30:2:-1)=incall(30-1:1:-1); incall(1)%msg=msgincall(nindex); incall(1)%xdt=xdtincall(nindex)
      enddo
    enddo

    if(nsec.eq.0 .or. nsec.eq.30) even(nmsg+1:130)%lstate=.false.
    if(nsec.eq.15 .or. nsec.eq.45) odd(nmsg+1:130)%lstate=.false.

!do i=1,nmsg
!  if(nsec.eq.0 .or. nsec.eq.30) print *, even(i)%msg
!  if(nsec.eq.15 .or. nsec.eq.45) print *, odd(i)%msg
!enddo

    if(params%ndelay.eq.0) then
      nFT8decd=my_ft8%decoded; dtmed=0.
      if(params%lforcesync) then; nintcount=3 ! fast track after Sync
      else if(nintcount.gt.0) then; nintcount=nintcount-1
      endif
      if(params%lforcesync .and. nFT8decd.eq.0) then
        avexdt=forcedt
      else
        if(nFT8decd.gt.2) then
          do i=1,nFT8decd
            if(i.lt.nFT8decd-1) then
              if((my_ft8%xdtt(i).gt.my_ft8%xdtt(i+1) .and. my_ft8%xdtt(i).lt.my_ft8%xdtt(i+2)) &
                 .or. (my_ft8%xdtt(i).lt.my_ft8%xdtt(i+1) .and. my_ft8%xdtt(i).gt.my_ft8%xdtt(i+2))) then
                dtmed=my_ft8%xdtt(i)
              else if((my_ft8%xdtt(i+1).gt.my_ft8%xdtt(i) .and. my_ft8%xdtt(i+1).lt.my_ft8%xdtt(i+2)) &
                 .or. (my_ft8%xdtt(i+1).lt.my_ft8%xdtt(i) .and. my_ft8%xdtt(i+1).gt.my_ft8%xdtt(i+2))) then
                dtmed=my_ft8%xdtt(i+1)
              else if((my_ft8%xdtt(i+2).gt.my_ft8%xdtt(i) .and. my_ft8%xdtt(i+2).lt.my_ft8%xdtt(i+1)) &
                 .or. (my_ft8%xdtt(i+2).lt.my_ft8%xdtt(i) .and. my_ft8%xdtt(i+2).gt.my_ft8%xdtt(i+1))) then
                dtmed=my_ft8%xdtt(i+2)
              else
                dtmed=my_ft8%xdtt(i)
              endif
              sumxdt=sumxdt+dtmed
            else
              sumxdt=sumxdt+dtmed ! use last median value
            endif
          enddo
          if(nFT8decd.gt.5) then; avexdt=(avexdt+sumxdt/nFT8decd)/2
          else if(nFT8decd.eq.5) then; avexdt=(1.1*avexdt+0.9*sumxdt/nFT8decd)/2
          else if(nFT8decd.eq.4) then; avexdt=(1.25*avexdt+0.75*sumxdt/nFT8decd)/2
          else if(nFT8decd.eq.3) then; avexdt=(1.35*avexdt+0.65*sumxdt/nFT8decd)/2
          endif
        else if(nFT8decd.gt.0) then
          sumxdt=sum(my_ft8%xdtt(1:nFT8decd))
          if(nFT8decd.eq.2) then; avexdt=(1.5*avexdt+0.5*sumxdt/nFT8decd)/2
          else if(nFT8decd.eq.1) then; avexdt=(1.75*avexdt+0.25*sumxdt)/2
          endif
        endif
      endif
    endif
    if(nFT8decd.gt.10 .and. nintcount.eq.1) avexdt=sumxdt/nFT8decd ! fast track after Sync or mode change on crowded bands
    call fillhash(numthreads,.true.)
    ncandall=sum(ncandallthr(1:numthreads))
!     call timer('decft8  ',1)
    go to 800
  endif

  if(params%nmode.eq.4) then
    if(params%nagcc) call agccft4()
    nfa=params%nfa; nfb=params%nfb; nfqso=params%nfqso; lfilter=params%nfilter
    if(lfilter) then
      nfafilt=max(nfa,nfqso-95); nfbfilt=min(nfb,nfqso+95) ! 84 + 11Hz possible freq error  
      if(nfqso.lt.nfafilt .or. nfqso.gt.nfbfilt) then
        write(*,128) nutc,'nfqso is out of bandwidth','d'; 128 format(i6.6,2x,a25,16x,a1); go to 800
      endif
    endif
    llagcc=params%nagcc; nFT4decd=0; sumxdtt(1)=0.0
    call fillhash(1,.false.)
!    call timer('decft4  ',0)
    call my_ft4%decode(ft4_decoded,params%nQSOProgress,nfqso,nfa,nfb,params%nft4depth, &
         params%nstophint,params%nswl)
!    call timer('decft4  ',1)
    if(params%ndelay.eq.0) then
      sumxdt=sumxdtt(1)
      if(nFT4decd.gt.5) then; avexdt=(avexdt+sumxdt/nFT4decd)/2
      else if(nFT4decd.eq.5) then; avexdt=(1.1*avexdt+0.9*sumxdt/nFT4decd)/2
      else if(nFT4decd.eq.4) then; avexdt=(1.25*avexdt+0.75*sumxdt/nFT4decd)/2
      else if(nFT4decd.eq.3) then; avexdt=(1.35*avexdt+0.65*sumxdt/nFT4decd)/2
      else if(nFT4decd.eq.2) then; avexdt=(1.5*avexdt+0.5*sumxdt/nFT4decd)/2
      endif
    endif
    call fillhash(1,.true.)
    go to 800
  endif

  lowrms=.false.
  call rms_augap(params%nutc,lowrms)
  if(lowrms) go to 800

!n1000=0  ! attempt to make T10 noise blanker, some degradation in decoding
!do i=2,623998
!!if(abs(dd(i)-dd(i-1)).gt.1000) then; n1000=n1000+1; print *,i; endif
!if((dd(i+1)-dd(i)).gt.1000.0) dd(i+1)=dd(i)+(dd(i)+dd(i-1))/2.0
!if((dd(i)-dd(i+1)).gt.1000.0) dd(i+1)=dd(i)-(dd(i)+dd(i-1))/2.0
!enddo

! signal input level diagnostics
!rrr=0.
!ddd=0.
!do i=1,624000
!if (abs(dd(i)).gt.rrr) rrr=abs(dd(i))
!enddo
!ddd=20*log10(rrr)
!print *,''
!print *,'max possible sample level is 32767'
!print *,'max RX sample level',int(rrr)
!print *,''
!print *,'max possible dynamic range is 90dB'
!print *,'RX signal dynamic range',nint(ddd)
!print *,''
! end of signal input level diagnostics

  newdat65=params%newdat
  newdat9=params%newdat
  if(params%nagain .and. .not.params%nagainfil) newdat9=.true.

  nshift=26000

  call process_dd(params%nagcc,params%nmode,params%ntxmode,params%nzhsym)

  if(.not.params%nagainfil .and. params%ntxmode.eq.9 .and. params%nagain) nagainjt9=.true.
  if(.not.params%nagainfil .and. params%nmode.eq.9 .and. params%nagain) nagainjt9s=.true.
  if(.not.params%nagainfil .and. params%nagain) nagainjt10=.true.
  if(params%nagainfil) nagainjt10=.false.

  if(params%nmode.eq.10) then
     call my_jt10%decode(jt10_decoded,nutc,params%nfqso,newdat9,params%npts8,   &
          params%nfa,params%nfb,params%ntol,params%nzhsym,nagainjt10,params%nagainfil,params%ntrials10, &
          params%ntrialsrxf10,params%nfilter,params%nswl,params%nagcc,params%nhint,params%nstophint, &
          params%nlasttx,mycall,hiscall,hisgrid)
     go to 800
  endif

  if(params%nmode.eq.9) then
     call my_jt9s%decode(jt9s_decoded,nutc,params%nfqso,newdat9,params%npts8,   &
          params%nfa,params%nfb,params%nzhsym,params%nfilter,params%nswl,   &
          nagainjt9s,params%nagainfil,params%ndepth,params%nhint,params%nstophint, &
          params%nlasttx,mycall,hiscall,hisgrid)
     go to 800
  endif

  call omp_set_dynamic(.true.)
!!!  !$omp parallel sections num_threads(2) copyin(/timer_private/) shared(ndecoded) if(.true.) !iif() needed on Mac
  !$omp parallel sections num_threads(2) shared(ndecoded) if(.true.) !iif() needed on Mac
  !$omp section
  if(params%nmode.eq.65 .or. (params%nmode.eq.(65+9) .and. params%ntxmode.eq.65)) then
     ! We're in JT65 mode, or should do JT65 first

     nf1=params%nfa
     nf2=params%nfb

!     call timer('jt65a   ',0)
     call my_jt65%decode(jt65_decoded,nutc,nf1,nf2,params%nfqso,  &
          logical(params%nagainfil),ntrials,params%naggressive,params%nhint,mycall, &
          hiscall,hisgrid,params%nprepass,params%nswl,params%nfilter,params%nstophint, &
          params%nlasttx,params%nsdecatt,params%fmaskact,params%ntxmode,params%ntopfreq65, &
          params%nharmonicsdepth,params%showharmonics)
!     call timer('jt65a   ',1)
  else if(params%nmode.eq.(65+9) .and. params%ntxmode.eq.9) then
     ! We're in JT9 mode, or should do JT9 first
!     call timer('decjt9  ',0)
     call my_jt9%decode(jt9_decoded,nutc,params%nfqso,newdat9,params%npts8,   &
          params%nfa,params%nfsplit,params%nfb,params%ntol,params%nzhsym,                   &
          nagainjt9,params%nagainfil,params%ndepth,params%nmode,params%nhint,params%nstophint, &
          params%nlasttx,mycall,hiscall,hisgrid,params%ntxmode)
!     call timer('decjt9  ',1)
  endif

  !$omp section
  if(params%nmode.eq.(65+9)) then          !Do the other mode (we're in dual mode)
     if (params%ntxmode.eq.9) then
        if(.not.nagainjt9) then
           nf1=params%nfa
           nf2=params%nfb
!           call timer('jt65a   ',0)
           call my_jt65%decode(jt65_decoded,nutc,nf1,nf2,params%nfqso, &
           logical(params%nagainfil),ntrials,params%naggressive,params%nhint,mycall,     &
           hiscall,hisgrid,params%nprepass,params%nswl,params%nfilter,params%nstophint, &
           params%nlasttx,params%nsdecatt,params%fmaskact,params%ntxmode,params%ntopfreq65, &
           params%nharmonicsdepth,params%showharmonics)
!           call timer('jt65a   ',1)
        endif
     else
        if (params%ntxmode.eq.65 .and. params%nagain) go to 2
!        call timer('decjt9  ',0)
        call my_jt9%decode(jt9_decoded,nutc,params%nfqso,newdat9,params%npts8,&
             params%nfa,params%nfsplit,params%nfb,params%ntol,params%nzhsym,                &
             nagainjt9,params%nagainfil,params%ndepth,params%nmode,params%nhint,params%nstophint, &
             params%nlasttx,mycall,hiscall,hisgrid,params%ntxmode)
!        call timer('decjt9  ',1)
2       continue
     end if
  endif

  !$omp end parallel sections
!800 call date_and_time(date = dat, time = tim2, zone = zon)
!call date_and_time(values = ival)
!timer2 = dble(ival(8)) * 0.001_8 + &
!dble(ival(7)) + dble(ival(6)) * 60.0_8 + &
!dble(ival(5)) * 3600.0_8
!print *,'Decoding finished: ',tim2
!write (*,'(1x,a15,f6.3,a8)')'Decoding time: ',timer2-timer1, ' seconds'

800 continue

write(*,1010) avexdt,ncandall
1010 format('<DecodeFinished><avexdt>',f6.2,'<ncand>',i5)
  call flush(6)
  if(params%lforcesync .and. nFT8decd.eq.0) avexdt=0. ! reset value to let correct sliding in decoder
!  close(13)

  do i=1,1000
    inquire(file=trim(temp_dir)//'/.lock',exist=fileExists)
    if(fileExists) return ! ready to exit from decoder, Decode button hung up issue
    call sleep_msec(1)
  enddo

  return

contains

  subroutine jt65_decoded (this, utc, snr, dt, freq, decoded, servis)
    use jt65_decode
    implicit none

    class(jt65_decoder), intent(inout) :: this
    integer, intent(in) :: utc
    integer, intent(in) :: snr
    real, intent(in) :: dt
    integer, intent(in) :: freq
    character(len=26), intent(in) :: decoded
    character(len=1), intent(in) :: servis

    real dtshift
    dtshift=(real(nshift))/12000.0
    !$omp critical(decode_results)
    write(*,1010) utc,snr,dt-dtshift,freq,decoded,servis
1010 format(i4.4,i4,f5.1,i5,1x,'#',1x,a26,a1)
!    write(13,1012) utc,snr,dt-dtshift,float(freq),decoded,
!1012 format(i4.4,i5,f6.1,f8.0,3x,a26,' JT65')
    call flush(6)

    !$omp end critical(decode_results)
    select type(this)
    type is (counting_jt65_decoder)
       this%decoded = this%decoded + 1
    end select
 end subroutine jt65_decoded

  subroutine jt9_decoded (this, utc, snr, dt, freq, decoded, servis9)
    use jt9_decode
    implicit none

    class(jt9_decoder), intent(inout) :: this
    integer, intent(in) :: utc
    integer, intent(in) :: snr
    real, intent(in) :: dt
    real, intent(in) :: freq
    character(len=26), intent(in) :: decoded
    character(len=1), intent(in) :: servis9

    !$omp critical(decode_results)
    write(*,1000) utc,snr,dt,nint(freq),decoded,servis9
1000 format(i4.4,i4,f5.1,i5,1x,'@',1x,a26,a1)
!    write(13,1002) utc,snr,dt,freq,decoded
!1002 format(i4.4,i5,f6.1,f8.0,3x,a26,' JT9')
    call flush(6)
    !$omp end critical(decode_results)
    select type(this)
    type is (counting_jt9_decoder)
       this%decoded = this%decoded + 1
    end select
  end subroutine jt9_decoded

  subroutine jt9s_decoded (this, utc, snr, dt, freq, decoded, servis9)
    use jt9s_decode
    implicit none

    class(jt9s_decoder), intent(inout) :: this
    integer, intent(in) :: utc
    integer, intent(in) :: snr
    real, intent(in) :: dt
    real, intent(in) :: freq
    character(len=26), intent(in) :: decoded
    character(len=1), intent(in) :: servis9

    write(*,1000) utc,snr,dt,nint(freq),decoded,servis9
1000 format(i4.4,i4,f5.1,i5,1x,'@',1x,a26,a1)
!    write(13,1002) utc,snr,dt,freq,decoded
!1002 format(i4.4,i5,f6.1,f8.0,3x,a26,' JT9')
    call flush(6)
    select type(this)
    type is (counting_jt9s_decoder)
       this%decoded = this%decoded + 1
    end select
  end subroutine jt9s_decoded

  subroutine jt10_decoded (this, utc, snr, dt, freq, decoded, servis9)
    use jt10_decode
    implicit none

    class(jt10_decoder), intent(inout) :: this
    integer, intent(in) :: utc
    integer, intent(in) :: snr
    real, intent(in) :: dt
    real, intent(in) :: freq
    character(len=26), intent(in) :: decoded
    character(len=1), intent(in) :: servis9

    write(*,1000) utc,snr,dt,nint(freq),decoded,servis9
1000 format(i4.4,i4,f5.1,i5,1x,'+',1x,a26,a1)
!    write(13,1002) utc,snr,dt,freq,decoded
!1002 format(i4.4,i5,f6.1,f8.0,3x,a26,' T10')
    call flush(6)
    select type(this)
    type is (counting_jt10_decoder)
       this%decoded = this%decoded + 1
    end select
  end subroutine jt10_decoded

  subroutine ft8_decoded (this,snr,dt,freq,decoded,servis8)
    use ft8_decode
    implicit none

    class(ft8_decoder), intent(inout) :: this
    integer, intent(in) :: snr
    real, intent(in) :: dt
    real, intent(in) :: freq
    character(len=26), intent(in) :: decoded
    character(len=1), intent(in) :: servis8

    write(*,1000) nutc,snr,dt,nint(freq),decoded,servis8
1000 format(i6.6,i4,f5.1,i5,1x,'~',1x,a26,a1)
!    write(13,1002) nutc,snr,dt,freq,0,decoded
!1002 format(i6.6,i5,f6.1,f8.0,i4,3x,a26,' FT8')
    call flush(6)
!    call flush(13)

    select type(this)
    type is (counting_ft8_decoder)
       this%decoded = this%decoded + 1
       this%xdtt(this%decoded)=dt
    end select

    return
  end subroutine ft8_decoded

  subroutine ft4_decoded (this,snr,dt,freq,decoded,servis4)
    use ft4_decode
    implicit none

    class(ft4_decoder), intent(inout) :: this
    integer, intent(in) :: snr
    real, intent(in) :: dt
    real, intent(in) :: freq
    character(len=26), intent(in) :: decoded
    character(len=1), intent(in) :: servis4

    write(*,1001) nutc,snr,dt,nint(freq),decoded,servis4
1001 format(i6.6,i4,f5.1,i5,1x,':',1x,a26,a1)
    call flush(6)
    
    select type(this)
    type is (counting_ft4_decoder)
       this%decoded = this%decoded + 1
    end select

    return
  end subroutine ft4_decoded

end subroutine multimode_decoder
