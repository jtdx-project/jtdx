module ft4_decode

  type :: ft4_decoder
    procedure(ft4_decode_callback), pointer :: callback
  contains
    procedure :: decode
  end type ft4_decoder

  abstract interface
    subroutine ft4_decode_callback (this,snr,dt,freq,decoded,servis4)
      import ft4_decoder
      implicit none
      class(ft4_decoder), intent(inout) :: this
      integer, intent(in) :: snr
      real, intent(in) :: dt
      real, intent(in) :: freq
      character(len=26), intent(in) :: decoded
      character(len=1), intent(in) :: servis4
    end subroutine ft4_decode_callback
  end interface

contains

  subroutine decode(this,callback,nQSOProgress,nfqso,nfa,nfb,ndepth,stophint,swl)
!    use timer_module, only: timer
    use packjt77
    use ft4_mod1, only : nFT4decd,nfafilt,nfbfilt,lfilter,lhidetest,lhidetelemetry
    use ft8_mod1, only : sumxdtt,avexdt,mycall,hiscall
    include 'ft4/ft4_params.f90'
    class(ft4_decoder), intent(inout) :: this
    procedure(ft4_decode_callback) :: callback
    parameter (NSS=NSPS/NDOWN,NDMAX=NMAX/NDOWN)
    character message*37,msg26*26,msgsent*37,msg37_2*37
    character c77*77
    character*37 decodes(100)
    character*12 mycall0,hiscall0,call_a,call_b
    character*4 servis4
    complex cd2(0:NDMAX-1)                  !Complex waveform
    complex cb(0:NDMAX-1)
    complex cd(0:NN*NSS-1)                       !Complex waveform
    complex ctwk(2*NSS),ctwk2(2*NSS,-16:16)
    real a(5)
    real bitmetrics(2*NN,3)
    real llr(2*ND),llra(2*ND),llrb(2*ND),llrc(2*ND),llrd(2*ND)
    real candidate(2,100)
    integer apbits(2*ND)
    integer*1 message77(77),rvec(77),apmask(2*ND),cw(2*ND)
    integer*1 hbits(2*NN)
    integer i4tone(103)
    integer nappasses(0:5)    ! # of decoding passes for QSO States 0-5
    integer naptypes(0:5,4)   ! nQSOProgress, decoding pass
    integer mcq(29),mrrr(19),m73(19),mrr73(19)
    logical nohiscall,unpk77_success,first,dobigfft,dosubtract,doosd,badsync,lFreeText,lhidemsg
    logical(1), intent(in) :: stophint,swl
    logical(1) falsedec

    data first/.true./
    data     mcq/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0/
    data    mrrr/0,1,1,1,1,1,1,0,1,0,0,1,0,0,1,0,0,0,1/
    data     m73/0,1,1,1,1,1,1,0,1,0,0,1,0,1,0,0,0,0,1/
    data   mrr73/0,1,1,1,1,1,1,0,0,1,1,1,0,1,0,1,0,0,1/
    data rvec/0,1,0,0,1,0,1,0,0,1,0,1,1,1,1,0,1,0,0,0,1,0,0,1,1,0,1,1,0, &
      1,0,0,1,0,1,1,0,0,0,0,1,0,0,0,1,0,1,0,0,1,1,1,1,0,0,1,0,1, &
      0,1,0,1,0,1,1,0,1,1,1,1,1,0,0,0,1,0,1/
    save fs,dt1,tt,txt,twopi,h,first,apbits,nappasses,naptypes, &
      mycall0,hiscall0,ctwk2

    this%callback => callback
    mycalllen1=len_trim(mycall)+1
    smax=0.; smax1=0.; nd1=0 ! smax init value shall be increased to 1.+ ?

    if(first) then
      fs=12000.0/NDOWN                !Sample rate after downsampling
      dt1=1/fs                         !Sample interval after downsample (s)
      tt=NSPS*dt1                      !Duration of "itone" symbols (s)
      txt=NZ*dt1                       !Transmission length (s) without ramp up/down
      twopi=8.0*atan(1.0)
      h=1.0

      do idf=-16,16
        a=0.
        a(1)=real(idf)
        ctwk=1.
        call twkfreq1(ctwk,0,2*NSS,2*NSS,fs/2.0,a,ctwk2(:,idf))
      enddo

      mcq=2*mod(mcq+rvec(1:29),2)-1
      mrrr=2*mod(mrrr+rvec(59:77),2)-1
      m73=2*mod(m73+rvec(59:77),2)-1
      mrr73=2*mod(mrr73+rvec(59:77),2)-1
      nappasses(0)=2; nappasses(1)=2; nappasses(2)=2; nappasses(3)=2; nappasses(4)=2; nappasses(5)=3

! iaptype
!   1        CQ     ???    ???           (29 ap bits)
!   2        MyCall ???    ???           (29 ap bits)
!   3        MyCall DxCall ???           (58 ap bits)
!   4        MyCall DxCall RRR           (77 ap bits)
!   5        MyCall DxCall 73            (77 ap bits)
!   6        MyCall DxCall RR73          (77 ap bits)

      naptypes(0,1:4)=(/1,2,0,0/) ! Tx6 selected (CQ)
      naptypes(1,1:4)=(/2,3,0,0/) ! Tx1
      naptypes(2,1:4)=(/2,3,0,0/) ! Tx2
      naptypes(3,1:4)=(/3,6,0,0/) ! Tx3
      naptypes(4,1:4)=(/3,6,0,0/) ! Tx4
      naptypes(5,1:4)=(/3,1,2,0/) ! Tx5

      mycall0=''; hiscall0=''; first=.false.
    endif

    l1=index(mycall,char(0)); if(l1.ne.0) mycall(l1:)=" "
    l1=index(hiscall,char(0)); if(l1.ne.0) hiscall(l1:)=" "
    if(mycall.ne.mycall0 .or. hiscall.ne.hiscall0) then
      apbits=0; apbits(1)=99; apbits(30)=99
      if(len(trim(mycall)) .lt. 3) go to 10
      nohiscall=.false.; hiscall0=hiscall
! use mycall for dummy hiscall - mycall won't be hashed
      if(len(trim(hiscall0)).lt.3) then; hiscall0=mycall; nohiscall=.true.; endif
      message=trim(mycall)//' '//trim(hiscall0)//' RR73'
      i3=-1; n3=-1
      call pack77(message,i3,n3,c77,1); call unpack77(c77,1,msgsent,unpk77_success,1)
      if(i3.ne.1 .or. (message.ne.msgsent) .or. .not.unpk77_success) go to 10
      read(c77,'(77i1)') message77
      message77=mod(message77+rvec,2)
      call encode174_91(message77,cw)
      apbits=2*cw-1
      if(nohiscall) apbits(30)=99
10    continue
      mycall0=mycall; hiscall0=hiscall
    endif

    maxcand=100; ndecodes=0; decodes=' '; fa=nfa; fb=nfb
! ndepth=1: 1 pass, no subtraction
! ndepth=2: 3 passes, bp only
! ndepth=3: 3 passes, bp+osd
    max_iterations=40; syncmin=1.2; dosubtract=.true.; doosd=.true.; nsp=3
    if(ndepth.eq.2) doosd=.false.
    if(ndepth.eq.1) then; nsp=1; dosubtract=.false.; doosd=.false.; endif

    do isp = 1,nsp
      if(isp.eq.2) then; if(ndecodes.eq.0) exit; nd1=ndecodes
      elseif(isp.eq.3) then; nd2=ndecodes-nd1; if(nd2.eq.0) exit
      endif

      candidate=0.0
      ncand=0
      call getcandidates4(fa,fb,syncmin,nfqso,maxcand,candidate,ncand)
      dobigfft=.true.
      do icand=1,ncand
        f0=candidate(1,icand)
        nf0=nint(f0); if(lfilter .and. (nf0.lt.nfafilt .or. nf0.gt.nfbfilt)) cycle
        snr=candidate(2,icand)-1.0
!print *,icand,f0,snr
        call ft4_downsample(dobigfft,f0,cd2)  !Downsample to 32 Sam/Sym
        if(dobigfft) dobigfft=.false.
        sum2=sum(cd2*conjg(cd2))/(real(NMAX)/real(NDOWN))
        if(sum2.gt.0.0) cd2=cd2/sqrt(sum2)
! Sample rate is now 12000/18 = 666.67 samples/second
! +/- 1.1 +/- 735 (1470); +/- 1.4 +/- 934.5 (1869); 0.5 sec 334 samples
        if(swl) then; ibwindow=623; else; ibwindow=490; endif ! /3
        ibottom=(0.5+avexdt)*666.67-667
        do iseg=1,3                ! DT search is done over 3 segments
          do isync=1,2          
            if(isync.eq.1) then
              idfmin=-12
              idfmax=12
              idfstp=3
!-1.0..+1.4; -1.2..+1.7 start window
              if(abs(avexdt).lt.1.e-6) then
                if(iseg.eq.1) then
                  if(swl) then; ibmin=179; ibmax=823; else; ibmin=194; ibmax=730; endif
                elseif(iseg.eq.2) then
                  smax1=smax
                  if(swl) then; ibmin=824; ibmax=1467; else; ibmin=731; ibmax=1266; endif
                elseif(iseg.eq.3) then
                  if(swl) then; ibmin=-467; ibmax=178; else; ibmin=-344; ibmax=193; endif
                endif
              else
                if(iseg.eq.1) then
                  ibmin=ibottom+ibwindow+1; ibmax=ibottom+ibwindow*2
                elseif(iseg.eq.2) then
                  smax1=smax
                  ibmin=ibottom+ibwindow*2+1; ibmax=ibottom+ibwindow*3
                elseif(iseg.eq.3) then
                  ibmin=ibottom; ibmax=ibottom+ibwindow
                endif
              endif
              ibstp=4
            else
              idfmin=idfbest-4
              idfmax=idfbest+4
              idfstp=1
              ibmin=ibest-5
              ibmax=ibest+5
              ibstp=1
            endif
            ibest=-1
            idfbest=0
            smax=-99.
            do idf=idfmin,idfmax,idfstp
              do istart=ibmin,ibmax,ibstp
                call sync4d(cd2,istart,ctwk2(:,idf),1,sync)  !Find sync power
                if(sync.gt.smax) then
                  smax=sync
                  ibest=istart
                  idfbest=idf
                endif
              enddo
            enddo
          enddo
          if(iseg.eq.1) smax1=smax
          if(smax.lt.1.2) cycle
          if(iseg.gt.1 .and. smax.lt.smax1) cycle 
          f1=f0+real(idfbest)
          if( f1.le.10.0 .or. f1.ge.4990.0 ) cycle
          call ft4_downsample(dobigfft,f1,cb) !Final downsample, corrected f0
          sum2=sum(abs(cb)**2)/(real(NSS)*NN)
          if(sum2.gt.0.0) cb=cb/sqrt(sum2)
          cd=0.
          if(ibest.ge.0) then
            it=min(NDMAX-1,ibest+NN*NSS-1)
            np=it-ibest+1
            cd(0:np-1)=cb(ibest:it)
          else
            cd(-ibest:ibest+NN*NSS-1)=cb(0:NN*NSS+2*ibest-1)
          endif
          call get_ft4_bitmetrics(cd,bitmetrics,badsync)
          if(badsync) cycle
          hbits=0
          where(bitmetrics(:,1).ge.0) hbits=1
          ns1=count(hbits(  1:  8).eq.(/0,0,0,1,1,0,1,1/))
          ns2=count(hbits( 67: 74).eq.(/0,1,0,0,1,1,1,0/))
          ns3=count(hbits(133:140).eq.(/1,1,1,0,0,1,0,0/))
          ns4=count(hbits(199:206).eq.(/1,0,1,1,0,0,0,1/))
          nsync_qual=ns1+ns2+ns3+ns4
          if(nsync_qual.lt. 20) cycle

          scalefac=2.83
          llra(  1: 58)=bitmetrics(  9: 66, 1)
          llra( 59:116)=bitmetrics( 75:132, 1)
          llra(117:174)=bitmetrics(141:198, 1)
          llra=scalefac*llra
          llrb(  1: 58)=bitmetrics(  9: 66, 2)
          llrb( 59:116)=bitmetrics( 75:132, 2)
          llrb(117:174)=bitmetrics(141:198, 2)
          llrb=scalefac*llrb
          llrc(  1: 58)=bitmetrics(  9: 66, 3)
          llrc( 59:116)=bitmetrics( 75:132, 3)
          llrc(117:174)=bitmetrics(141:198, 3)
          llrc=scalefac*llrc

          apmag=maxval(abs(llra))*1.1
          npasses=3+nappasses(nQSOProgress)
          if(stophint) npasses=4
          if(ndepth.eq.1) npasses=3
          do ipass=1,npasses
            if(ipass.eq.1) llr=llra; if(ipass.eq.2) llr=llrb; if(ipass.eq.3) llr=llrc
            if(ipass.le.3) then; apmask=0; iaptype=0; endif
            if(ipass.gt.3) then
              llrd=llra
              iaptype=naptypes(nQSOProgress,ipass-3)
              if(stophint) iaptype=1
! Conditions that cause us to bail out of AP decoding
              napwid=50
              if(iaptype.ge.3 .and. (abs(f1-nfqso).gt.napwid)) cycle
              if(iaptype.ge.2 .and. apbits(1).gt.1) cycle  ! No, or nonstandard, mycall
              if(iaptype.ge.3 .and. apbits(30).gt.1) cycle ! No, or nonstandard, dxcall

              if(iaptype.eq.1) then; apmask=0; apmask(1:29)=1; llrd(1:29)=apmag*mcq(1:29); endif ! CQ
              if(iaptype.eq.2) then; apmask=0; apmask(1:29)=1; llrd(1:29)=apmag*apbits(1:29); endif ! MyCall,???,???
              if(iaptype.eq.3) then; apmask=0; apmask(1:58)=1; llrd(1:58)=apmag*apbits(1:58); endif ! MyCall,DxCall,???
              if(iaptype.eq.4 .or. iaptype.eq.5 .or. iaptype.eq.6) then ! mycall, hiscall, RRR|73|RR73
                apmask=0; apmask(1:77)=1; if(iaptype.eq.6) llrd(1:77)=apmag*apbits(1:77)
              endif
              llr=llrd
            endif
            message77=0; dmin=0.0
            call bpdecode174_91(llr,apmask,max_iterations,message77,cw,nharderror,niterations)
            if(doosd .and. nharderror.lt.0) then
              ndeep=3
!              if(abs(nfqso-f1).le.napwid) ndeep=4
              call osd4_174_91(llr,apmask,ndeep,message77,cw,nharderror,dmin)
            endif

            if(sum(message77).eq.0) cycle
            if(nharderror.ge.0) then
              message77=mod(message77+rvec,2) ! remove rvec scrambling
              write(c77,'(77i1)') message77(1:77); read(c77(72:74),'(b3)') n3; read(c77(75:77),'(b3)') i3
              call unpack77(c77,1,message,unpk77_success,1)
              if(message.eq."") cycle ! being treated as false decode
              if(unpk77_success.and.dosubtract) then
                call get_ft4_tones_from_77bits(message77,i4tone)
                dt=real(ibest)/666.67
                call subtractft4(i4tone,f1,dt)
              endif

              lhidemsg=.false.
              if(lhidetelemetry .and. i3.eq.0 .and. n3.eq.5) lhidemsg=.true.
              if(lhidetest) then
                if((i3.eq.0 .and. n3.gt.1 .and. n3.lt.5) .or. i3.eq.3 .or. i3.gt.4) then
                  if(mycalllen1.lt.4 .or. message(1:mycalllen1).ne.trim(mycall)//' ') lhidemsg=.true.
                endif
                if(message(1:3).eq.'CQ ') then
                  if(message(1:6).eq.'CQ RU ' .or. message(1:6).eq.'CQ FD ' .or. message(1:8).eq.'CQ TEST ') &
                    lhidemsg=.true.
                endif
              endif

              lFreeText=.false.; if(i3.eq.0 .and. n3.eq.0) lFreeText=.true.
! delete braces
              if(.not.lFreeText .and. index(message,'<').gt.0) then ! DXpedition being not supported in FT4
                ispc1=index(message,' '); ispc2=index(message((ispc1+1):),' ')+ispc1
                ispc3=index(message((ispc2+1):),' ')+ispc2
                ieoc1=ispc1-1; iboc2=ispc1+1; ieoc2=ispc2-1
                if(message(1:1).eq.'<' .and. message(2:2).ne.'.') then
                  message(ieoc1:37)=message(ieoc1+1:37)//' '; message(1:37)=message(2:37)//' '
                else if(message(iboc2:iboc2).eq.'<' .and. message(iboc2+1:iboc2+1).ne.'.') then
                  message(ieoc2:37)=message(ieoc2+1:37)//' '; message(iboc2:37)=message(iboc2+1:37)//' '
                else
                  iboc3=ispc2+1; ieoc3=ispc3-1
                  if(message(iboc3:iboc3).eq.'<' .and. message(iboc3+1:iboc3+1).ne.'.') then
                    message(ieoc3:37)=message(ieoc3+1:37)//' '; message(iboc3:37)=message(iboc3+1:37)//' '
                  endif
                endif
              endif

              idupe=0
              do i=1,ndecodes; if(decodes(i).eq.message) idupe=1; enddo
              if(idupe.eq.1) exit
              ndecodes=ndecodes+1; decodes(ndecodes)=message
              if(snr.gt.0.0) then; xsnr=10*log10(snr)-14.8; else; xsnr=-21.0; endif
              nsnr=nint(max(-21.0,xsnr))
              xdt=ibest/666.67 - 0.5
! check for false decodes
! i3=3 n3=4  TU; B69FWJ 8Z6IB 559 580  
! i3=3 n3=3  TU; FD9GRU HT1HHY R 529 11
              if(message(1:3).eq.'TU;' .and. nsnr.lt.-15 .and. i3.eq.3 .and. (n3.eq.3 .or. n3.eq.4)) then
                ispc1=index(message,' '); ispc2=index(message((ispc1+1):),' ')+ispc1 
                ispc3=index(message((ispc2+1):),' ')+ispc2
                call_a=''; call_b=''; call_a=message(ispc1+1:ispc2-1); call_b=message(ispc2+1:ispc3-1)
                falsedec=.false.; call chkflscall(call_a,call_b,falsedec)
                if(falsedec) then; message=''; cycle; endif
              endif
              if(iaptype.eq.1 .and. xsnr.lt.-15.) then
                nbadcrc=0; call chkfalse8(message,i3,n3,nbadcrc,iaptype,.false.)
                if(nbadcrc.eq.1) then; message=''; cycle; endif
              endif
! EA1AHY M83WN/R R QA79   *
! MS8QQS UX3QBS/P R NG63  i3=2 n3=7
! 3B4NDC/R C40AUZ/R R IR83  i3=1 n3=7
! EA1AHY PW1BSL R GR47 i3=1 n3=3 mycall
! EA1AHY PW1BSL R GR47 *  i3=1 n3=1 mycall
              if((i3.eq.1 .or. i3.eq.2) .and. index(message,' R ').gt.0) then
                ispc1=index(message,' '); ispc2=index(message((ispc1+1):),' ')+ispc1
                ispc3=index(message((ispc2+1):),' ')+ispc2
                if(message(ispc2:ispc3).eq.' R ') then 
                  call_a='            '; call_b='            '
                  if(message(1:ispc1-1).eq.trim(mycall)) then
                    call_a='CQ          '
                  else
                    if((i3.eq.1 .and. message(ispc1-2:ispc1-1).eq.'/R') .or. &
                       (i3.eq.2 .and. message(ispc1-2:ispc1-1).eq.'/R')) then
                      call_a=message(1:ispc1-3)
                    else
                      call_a=message(1:ispc1-1)
                    endif
                  endif
                  if((i3.eq.1 .and. message(ispc2-2:ispc2-1).eq.'/R') .or. &
                     (i3.eq.2 .and. message(ispc2-2:ispc2-1).eq.'/P')) then
                    call_b=message(ispc1+1:ispc2-3)
                  else
                    call_b=message(ispc1+1:ispc2-1)
                  endif
                  falsedec=.false.; call chkflscall(call_a,call_b,falsedec)
                  if(falsedec) then; nbadcrc=1; message=''; return; endif
                endif
              endif
!write(21,'(i6.6,i5,2x,f4.1,i6,2x,a37,2x,f4.1,3i3,f5.1,i4,i4,i4)') &
!  nutc,nsnr,xdt,nint(f1),message,smax,iaptype,ipass,isp,dmin,nsync_qual,nharderror,iseg
              if(i3.eq.0 .and. n3.eq.1) then ! special DXpedition msg
                call msgparser(message,msg37_2)
                servis4="1"
                msg26=message(1:26); call this%callback(nsnr,xdt,f1,msg26,servis4)
                msg26=msg37_2(1:26); call this%callback(nsnr,xdt,f1,msg26,servis4)
              else
                msg26=message(1:26); servis4=""
                if(lFreeText) then; if(abs(nfqso-nint(f1)).le.10) then; servis4=','; else; servis4='.'; endif; endif
                if(.not.lhidemsg) call this%callback(nsnr,xdt,f1,msg26,servis4)
              endif
              nFT4decd=nFT4decd+1; sumxdtt(1)=sumxdtt(1)+xdt
              exit
            endif
          enddo !Sequence estimation
          if(nharderror.ge.0) exit
        enddo !3 DT segments
      enddo    !Candidate list
    enddo       !Subtraction loop

    return
  end subroutine decode

end module ft4_decode
