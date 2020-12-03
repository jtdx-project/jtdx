module ft8_decode

  type :: ft8_decoder
     procedure(ft8_decode_callback), pointer :: callback
   contains
     procedure :: decode
  end type ft8_decoder

  abstract interface
     subroutine ft8_decode_callback (this,snr,dt,freq,decoded,servis8)
       import ft8_decoder
       implicit none
       class(ft8_decoder), intent(inout) :: this
       integer, intent(in) :: snr
       real, intent(in) :: dt,freq
       character(len=26), intent(in) :: decoded
       character(len=1), intent(in) :: servis8
     end subroutine ft8_decode_callback
  end interface

contains

  subroutine decode(this,callback,nQSOProgress,nfqso,nft8rxfsens,nftx,nutc,nfa,nfb,ncandthin,ndtcenter,lapon,nsec, &
                    napwid,swl,lmycallstd,lhiscallstd,filter,stophint,nthr,numthreads, &
                    nagainfil,lft8lowth,lft8subpass,lft8latestart,lhideft8dupes,lhidehash)
!use wavhdr
!    use timer_module, only: timer
 !$ use omp_lib
    use ft8_mod1, only : ndecodes,allmessages,allsnrs,allfreq,odd,even,nmsg,lastrxmsg,lasthcall,calldt,incall, &
                         oddcopy,evencopy,nFT8decd,sumxdt,avexdt,mycall,hiscall,dd8,dd8m,nft8cycles,nft8swlcycles,ncandall
    use ft4_mod1, only : lhidetest,lhidetelemetry
    include 'ft8_params.f90'
!type(hdr) h

    class(ft8_decoder), intent(inout) :: this
    procedure(ft8_decode_callback) :: callback
!    real sbase(NH1)
!integer*2 iwave(180000)
    real candidate(4,460)
    integer, intent(in) :: nQSOProgress,nfqso,nft8rxfsens,nftx,nfa,nfb,ncandthin,ndtcenter,nsec,napwid,nthr,numthreads
    logical, intent(in) :: lapon,nagainfil
    logical(1), intent(in) :: swl,filter,stophint,lft8lowth,lft8subpass,lft8latestart,lhideft8dupes, &
                              lhidehash,lmycallstd,lhiscallstd
    logical newdat1,lsubtract,ldupe,lFreeText,lspecial
    logical(1) lft8sdec,lft8s,lft8sd,lrepliedother,lhashmsg,lqsothread,lhidemsg,lhighsens,lcqcand
    character msg37*37,msg37_2*37,msg26*26,servis8*1,datetime*13,call2*12
    character*37 msgsrcvd(130)

    type oddtmp_struct
      real freq
      real dt
      logical lstate
      character*37 msg
    end type oddtmp_struct
    type(oddtmp_struct) oddtmp(130)

    type eventmp_struct
      real freq
      real dt
      logical lstate
      character*37 msg
    end type eventmp_struct
    type(eventmp_struct) eventmp(130)

    this%callback => callback

    oddtmp%lstate=.false.; eventmp%lstate=.false.; nmsgloc=0; ncandthr=0
    if(hiscall.eq.'') then; lastrxmsg(1)%lstate=.false. 
      elseif(lastrxmsg(1)%lstate .and. lasthcall.ne.hiscall .and. index(lastrxmsg(1)%lastmsg,trim(hiscall)).le.0) &
          then; lastrxmsg(1)%lstate=.false.
    endif

    lrepliedother=.false.; lft8sdec=.false.; lqsothread=.false.!; lthrdecd=.false.
    ncount=0; servis8=' '; mycalllen1=len_trim(mycall)+1
!print *,lastrxmsg(1)%lstate,lastrxmsg(1)%xdt,lastrxmsg(1)%lastmsg
    write(datetime,1001) nutc        !### TEMPORARY ###
1001 format("000000_",i6.6)

    if(nfqso.ge.nfa .and. nfqso.le.nfb) lqsothread=.true.
    if(lqsothread .and. lapon .and. .not.lastrxmsg(1)%lstate .and. .not.stophint .and. hiscall.ne.'') then
! got incoming call
      do i=1,20
        if(index(incall(i)%msg,(trim(mycall)//' '//trim(hiscall))).eq.1) then
          lastrxmsg(1)%lastmsg=incall(i)%msg; lastrxmsg(1)%xdt=incall(i)%xdt; lastrxmsg(1)%lstate=.true.; exit
        endif
      enddo

      if(.not.lastrxmsg(1)%lstate) then
! calling someone, lastrxmsg still not valid
        if(nsec.eq.0 .or. nsec.eq.30) then
          do i=1,130
            if(.not.evencopy(i)%lstate) cycle
            if(index(evencopy(i)%msg,' '//trim(hiscall)//' ').gt.1) then
              lastrxmsg(1)%lastmsg=evencopy(i)%msg; lastrxmsg(1)%xdt=evencopy(i)%dt; lastrxmsg(1)%lstate=.true.; exit
            endif
          enddo
        elseif(nsec.eq.15 .or. nsec.eq.45) then
          do i=1,130
            if(.not.oddcopy(i)%lstate) cycle
              if(index(oddcopy(i)%msg,' '//trim(hiscall)//' ').gt.1) then
                lastrxmsg(1)%lastmsg=oddcopy(i)%msg; lastrxmsg(1)%xdt=oddcopy(i)%dt; lastrxmsg(1)%lstate=.true.; exit
              endif
          enddo
        endif
      endif
    endif

!print *,'in',lastrxmsg(1)%lstate
!print *,lastrxmsg(1)%lastmsg
!write(*,1018) nQSOProgress,'d'
!1018 format(i1,46x,a1)

! sliding search over +/- 2.5s relative to 0.5s TX start time
    jzb=-62 + avexdt*25.;  jzt=62 + avexdt*25.
! sliding search over +/- 3.5s relative to 0.5s TX start time
    if(lft8latestart .or. swl) then; jzb=-86 + avexdt*25.;  jzt=86 + avexdt*25.; endif

    npass=3 ! fallback
    if(swl) then
      if(nft8swlcycles.eq.1) then; npass=3
      else if(nft8swlcycles.eq.2) then; npass=6
      else if(nft8swlcycles.eq.3) then; npass=9
      else; npass=3
      endif
    else
      if(nft8cycles.eq.1) then; npass=3
      else if(nft8cycles.eq.2) then; npass=6
      else if(nft8cycles.eq.3) then; npass=9
      else; npass=3
      endif
    endif
    syncmin=1.5
    do ipass=1,npass
      newdat1=.true.; lsubtract=.true.
      if(ipass.eq.1 .or. ipass.eq.4 .or. ipass.eq.7) then
        if(lft8lowth .or. swl) syncmin=1.225
      elseif(ipass.eq.2 .or. ipass.eq.5 .or. ipass.eq.8) then
         if(lft8lowth .or. swl) syncmin=1.5
      elseif(ipass.eq.3 .or. ipass.eq.6 .or. ipass.eq.9) then
         if(lft8lowth .or. swl) syncmin=1.1
      endif
      if(ipass.gt.5 .or. (ipass.eq.3 .and. npass.eq.3 .and. .not.swl)) lsubtract=.false.
      if(ipass.eq.4 .or. ipass.eq.7) then
!$omp barrier
        if(nthr.eq.1) then
!$omp critical(change_dd8)
          if(ipass.eq.4) then
            dd8m=dd8
            do i=1,179999; dd8(i)=(dd8(i)+dd8(i+1))/2; enddo
          endif
          if(ipass.eq.7) then
            dd8(1)=dd8m(1)
            do i=2,180000; dd8(i)=(dd8m(i-1)+dd8m(i))/2; enddo
          endif
!$OMP FLUSH (dd8)
!$omp end critical(change_dd8)
        endif
!$omp barrier
      endif
      !call timer('sync8   ',0)
      call sync8(nfa,nfb,syncmin,nfqso,candidate,ncand,jzb,jzt,swl,ipass,lqsothread,ncandthin,filter,ndtcenter)
      !call timer('sync8   ',1)
!      if(ipass.eq.1) then
!        laveraging=.true.
!        do icand=1,ncand
!          if(candidate(3,icand).gt.2.1) then; laveraging=.false.; exit; endif
!        enddo
!      endif
!      if(ipass.gt.1 .and. .not.lthrdecd) laveraging=.true.
      do icand=1,ncand
        sync=candidate(3,icand)
        f1=candidate(1,icand)
        xdt=candidate(2,icand)
        lcqcand=.false.; if(candidate(4,icand).gt.1.0) lcqcand=.true.
        lhighsens=.false.
        if(sync.lt.1.9 .or. ((ipass.eq.2 .or. ipass.eq.4 .or. ipass.eq.6).and. sync.lt.3.15)) lhighsens=.true.
        lspecial=.false.; lFreeText=.false.; i3bit=0; lft8s=.false.; lft8sd=.false.; lhashmsg=.false.; iaptype=0
        msg37='';i3=16;n3=16
        !call timer('ft8b    ',0)
!if(nthr.eq.1) print *,ipass,'nthr1',newdat1
!if(nthr.eq.2) print *,ipass,'nthr2',newdat1
!write (*,"(F5.2,1x,I1,1x,I4,1x,F4.2)") candidate(2,icand)-0.5,ipass,nint(candidate(1,icand)),candidate(3,icand)
        call ft8b(newdat1,nQSOProgress,nfqso,nftx,lapon,napwid,lsubtract, &
                  nagainfil,iaptype,f1,xdt,nbadcrc,lft8sdec,msg37,msg37_2,xsnr,swl,stophint,   &
                  nthr,lFreeText,ipass,lft8subpass,lspecial,lcqcand,                    &
                  i3bit,lhidehash,lft8s,lmycallstd,lhiscallstd,nsec,lft8sd,i3,n3,nft8rxfsens,  &
                  ncount,msgsrcvd,lrepliedother,lhashmsg,lqsothread,lft8lowth,lhighsens)
        nsnr=nint(xsnr) 
        xdt=xdt-0.5
        !call timer('ft8b    ',1)
        if(nbadcrc.eq.0) then
          lhidemsg=.false.
          if(lhidetelemetry .and. i3.eq.0 .and. n3.eq.5) lhidemsg=.true.
          if(lhidetest) then
            if((i3.eq.0 .and. n3.gt.1 .and. n3.lt.5) .or. i3.eq.3 .or. i3.gt.4) then
              if(mycalllen1.lt.4 .or. msg37(1:mycalllen1).ne.trim(mycall)//' ') lhidemsg=.true.
            endif
            if(msg37(1:3).eq.'CQ ') then
              if(msg37(1:6).eq.'CQ RU ' .or. msg37(1:6).eq.'CQ FD ' .or. msg37(1:8).eq.'CQ TEST ') lhidemsg=.true.
            endif
          endif

          if(lspecial) then; nspecial=2; else; nspecial=1; endif
          do k=1,nspecial
            if(k.eq.2) msg37=msg37_2
            ldupe=.false.
            if(msg37(1:6).eq."      ") ldupe=.true. 
            if(.not.ldupe .and. ndecodes.gt.0) then
              do id=1,ndecodes
                if(lhideft8dupes) then
                  if(msg37.eq.allmessages(id) .and. (nsnr.le.allsnrs(id) .or. &
                     (nsnr.gt.allsnrs(id) .and. abs(allfreq(id)-f1).lt.45.0))) then
                    ldupe=.true.; exit
                  endif
                else
                  if(msg37.eq.allmessages(id) .and. ((nsnr.le.allsnrs(id) .and. abs(allfreq(id)-f1).lt.45.0) &
                     .or. (nsnr.gt.allsnrs(id) .and. abs(allfreq(id)-f1).lt.45.0 .and. numthreads.ne.1))) then
                    ldupe=.true.; exit
                  endif
                endif
              enddo
            endif
            if(.not.ldupe) then
              if(.not.lFreeText .and. k.eq.1) call extract_call(msg37,call2)
!$omp critical(update_arrays)
              ndecodes=ndecodes+1
              allmessages(ndecodes)=msg37
              allsnrs(ndecodes)=nsnr
              allfreq(ndecodes)=f1
              if(.not.lhidemsg) then
                if(iaptype.eq.0) then
                  if(.not.lFreeText .or. lspecial) servis8=' '
                  if(.not.lspecial .and. lFreeText) then
                    if(abs(nfqso-nint(f1)).le.10) then; servis8=','; else; servis8='.'; endif
                  endif
                  if(lft8sd .or. lft8s) servis8='^'
                else
                  if(lft8sd .or. lft8s) then; servis8='^'; else; servis8='*'; endif
                endif
                if(i3bit.eq.1) servis8='1'
!write (*,"(F5.2,1x,I1,1x,I4,1x,F4.2)") candidate(2,icand)-0.5,ipass,nint(candidate(1,icand)),candidate(3,icand)
!write (*,"(I1,1x,F4.2)") ipass,candidate(3,icand)
!print *,candidate(2,icand)-0.5,msg37
!print *,msg37
                msg26=msg37(1:26)
                if(associated(this%callback)) call this%callback(nsnr,xdt,f1,msg26,servis8)
!  calldt(200:2:-1)%call2=calldt(200-1:1:-1)%call2
!                lthrdecd=.true.
                calldt(200:2:-1)=calldt(200-1:1:-1); calldt(1)%call2=call2; calldt(1)%dt=xdt
                nFT8decd=nFT8decd+1; sumxdt=sumxdt+xdt
              endif
!$OMP FLUSH (ndecodes,allmessages,allsnrs,allfreq)
!$omp end critical(update_arrays)
              if(i3.eq.4 .and. msg37(1:3).eq.'CQ ' .and. mod(nsec,15).eq.0 .and. nmsgloc.lt.130) then
                nmsgloc=nmsgloc+1
                if(nsec.eq.0 .or. nsec.eq.30) then
                  eventmp(nmsgloc)%msg=msg37; eventmp(nmsgloc)%freq=f1
                  eventmp(nmsgloc)%dt=xdt; eventmp(nmsgloc)%lstate=.true.
                endif
                if(nsec.eq.15 .or. nsec.eq.45) then
                  oddtmp(nmsgloc)%msg=msg37; oddtmp(nmsgloc)%freq=f1
                  oddtmp(nmsgloc)%dt=xdt; oddtmp(nmsgloc)%lstate=.true.
                endif
                go to 4 ! tmp filled in
              endif
              if(.not.lFreeText) then ! protection against any possible free txtmsg bit corruption
                ispc1=index(msg37,' ')
                if(.not.lhashmsg .and. mod(nsec,15).eq.0 .and. ((i3.eq.1 .and. .not.lft8sd) .or. lft8sd) .and. &
                   msg37(1:ispc1-1).ne.trim(mycall) .and. nmsgloc.lt.130 .and. index(msg37,'<').le.0) then
                  if(index(msg37,'/').gt.0 .and. msg37(1:3).ne.'CQ ') go to 4 ! compound not supported
                  nmsgloc=nmsgloc+1
                  if(nsec.eq.0 .or. nsec.eq.30) then
                    eventmp(nmsgloc)%msg=msg37; eventmp(nmsgloc)%freq=f1
                    eventmp(nmsgloc)%dt=xdt; eventmp(nmsgloc)%lstate=.true.
                  endif
                  if(nsec.eq.15 .or. nsec.eq.45) then
                    oddtmp(nmsgloc)%msg=msg37; oddtmp(nmsgloc)%freq=f1
                    oddtmp(nmsgloc)%dt=xdt; oddtmp(nmsgloc)%lstate=.true.
                  endif
                endif
              endif
            endif
4           continue
          enddo
        endif
      enddo
      ncandthr=ncandthr+ncand
    enddo
! h=default_header(12000,NMAX)
! open(10,file='subtract.wav',status='unknown',access='stream')
! iwave(1:180000)=nint(dd8(1:180000))
! write(10) h,iwave
! close(10)
    ncandthr=nint(float(ncandthr)/npass)
!$omp critical(update_structures)
    ncandall=ncandall+ncandthr
!$OMP FLUSH (ncandall)
    if(nmsgloc.gt.0) then
      if(nsec.eq.0 .or. nsec.eq.30) then
        even(nmsg+1:nmsg+nmsgloc)%msg=eventmp(1:nmsgloc)%msg; even(nmsg+1:nmsg+nmsgloc)%freq=eventmp(1:nmsgloc)%freq
        even(nmsg+1:nmsg+nmsgloc)%dt=eventmp(1:nmsgloc)%dt; even(nmsg+1:nmsg+nmsgloc)%lstate=eventmp(1:nmsgloc)%lstate
        nmsg=nmsg+nmsgloc
!$OMP FLUSH (nmsg,even)
      endif
      if(nsec.eq.15 .or. nsec.eq.45) then
        odd(nmsg+1:nmsg+nmsgloc)%msg=oddtmp(1:nmsgloc)%msg; odd(nmsg+1:nmsg+nmsgloc)%freq=oddtmp(1:nmsgloc)%freq
        odd(nmsg+1:nmsg+nmsgloc)%dt=oddtmp(1:nmsgloc)%dt; odd(nmsg+1:nmsg+nmsgloc)%lstate=oddtmp(1:nmsgloc)%lstate
        nmsg=nmsg+nmsgloc
!$OMP FLUSH (nmsg,odd)
      endif
    endif
!$omp end critical(update_structures)
!print *,'out',lastrxmsg(1)%lstate
!print *,lastrxmsg(1)%lastmsg
    return
  end subroutine decode
end module ft8_decode
