! last time modified by Igor UA3DJY on 20200104

module jt10_decode

  type :: jt10_decoder
     procedure(jt10_decode_callback), pointer :: callback
   contains
     procedure :: decode
  end type jt10_decoder

  abstract interface
     subroutine jt10_decode_callback (this, utc, snr, dt, freq, decoded, servis9)
       import jt10_decoder
       implicit none
       class(jt10_decoder), intent(inout) :: this
       integer, intent(in) :: utc
       integer, intent(in) :: snr
       real, intent(in) :: dt
       real, intent(in) :: freq
       character(len=26), intent(in) :: decoded
       character(len=1), intent(in) ::  servis9
     end subroutine jt10_decode_callback
  end interface

contains

  subroutine decode(this,callback,nutc,nfqso,newdatin,npts8,nfa,nfb,ntol,nzhsym,nagain,nagainfil, &
             ntrials10,ntrialsrxf10,filter,swl,agcc,hint,stophint,nlasttx,mycall12,hiscall12,hisgrid6)
    use timer_module, only: timer
!    use jt9_mod2
!!use wavhdr
!!use jt65_mod6
    class(jt10_decoder), intent(inout) :: this
    procedure(jt10_decode_callback) :: callback
    integer, parameter :: NSMAX10=11520, MAXCANDRXF=5, MAXCAND=200
!!type(hdr) h
!!integer*2 idd(1:648000)
    character(len=12), intent(in) :: mycall12, hiscall12
    character(len=6), intent(in) :: hisgrid6
    integer, intent(in) :: nlasttx
    logical, intent(in) :: newdatin,nagain
    logical(1), intent(in) :: nagainfil,filter,swl,agcc,hint,stophint
    logical newdat
    real s3(0:7,69)
    logical done(NSMAX10)
    integer*1 i1SoftSymbols(207)
    integer*4 i4Decoded6BitWords(12),i4GrayCodedSym(69)
    integer mrs(69),mrs2(69)
    character msg26*26,msg*22,servis9*1,mycall*6,hiscall*6,hisgrid*4,chint*1,hiscallrxf*6
    integer ncand0
    logical(1) freemsg,dupe,agcclast,rxfreq,hintdec,dofano,tryhint
    type accepted_decode
       integer i
       character*22 decoded
    end type accepted_decode
    type(accepted_decode) dec(200)
    type rxfreq_decode
       real freq
       real xdt
       character*22 msg
    end type rxfreq_decode
    type(rxfreq_decode) rxfdec(1)
    type candrxf
       real ccfred
       integer i
    end type candrxf
    type(candrxf) carxf(MAXCANDRXF)
    type candidate
       real ccfred
       integer i
    end type candidate
    type(candidate) ca(MAXCAND)
    save ncand0,agcclast,rxfdec

    this%callback => callback
    ndecoded=0; noff=3; chint=' '
    nsps=6912                                   !Params for JT9-1
    df3=12000.0/27648.0 ! 0.434027(7) Hz frequency bin width
    dec%i=-1; dec%decoded=''
    dupe=.false.; rxfreq=.false.; tryhint=.false.
    ntol1=ntol !supress compiler warning
    newdat=newdatin
    mycall=mycall12(1:6); hiscall=hiscall12(1:6); hisgrid=hisgrid6(1:4); hiscallrxf=''

    if(filter) then; nfa=max(1,nfqso-80); nfb=min(4945,nfqso+80); endif
    if(nagain .or. nagainfil) then; nfa=max(1,nfqso-20); nfb=min(4945,nfqso+20); endif
    ia=max(1,nint(nfa/df3))
    ib=min(NSMAX10,nint(nfb/df3))
    irxf=nint(nfqso/df3)+1

    th_wide=2.6; th_rxf=2.0; th_agn=2.0
    if(nagain .or. nagainfil) then; th_wide=th_agn; th_rxf=th_agn; newdat=.false.; endif
    if((nagain .or. nagainfil) .and. agcc.neqv.agcclast) newdat=.true.
    agcclast=agcc

    nsps8=nsps/8
!    df8=1500.0/nsps8 ! 1.73611116
    dblim=db(864.0/nsps8) - 26.2

!    fac=ntrials10**2
    facrxf=ntrialsrxf10**2
!print *,'ncand0',ncand0; ncount=0

 do ipass=1,2
    done=.false.
    call cpu_time(t1)
!    call timer('sync9   ',0)
    call sync10(nzhsym,ia,ib,irxf,th_wide,th_rxf,ncand0,newdat,carxf,ca)
!    call timer('sync9   ',1)

!    limit=100000*fac ! max limit is 20648000

    if(.not.nagainfil) then ! RX frequency candidate list decoding
       limit=300000*facrxf
       if(nagain) limit=1000000*ntrialsrxf10
       if(swl) limit=2*limit
       limitr=limit/4

       do icand=1,maxcandrxf
!print *,carxf(icand)%i,carxf(icand)%ccfred
          if(carxf(icand)%i.lt.0) cycle
          dofano=.true.
          fpk=(carxf(icand)%i-1)*df3
          if(ncand0.gt.199 .and. icand.gt.2) limit=limitr
!print *,fpk,carxf(icand)%ccfred
!print *,fpk
          tryhint=.false.
!          call timer('softsym ',0)
          call softsym10(npts8,nsps8,newdat,fpk,syncpk,snrdb,xdt,    &
               freq,drift,schk,i1SoftSymbols,qualf,syncro,mrs,mrs2,s3,tryhint,xdtrxf)
!          call timer('softsym ',1)
          sync=(syncpk+1)/4.0
!print *,freq,syncpk,schk
!print *,freq,syncro
!print *,schk,qualf
!print *,sync
!          if(sync.lt.0.5 .or. schk.lt.1.0) cycle
          if(sync.lt.0.5 .or. schk.lt.2.0 .or. qualf.lt.2.4) dofano=.false.
          rxfreq=.false.
          if(abs(freq-real(nfqso)).lt.2.0 .and. xdt.gt.-0.5 .and. xdt.lt.1.5) rxfreq=.true.
!!!          if(syncro.lt.0.2 .or. syncro.gt.5.) cycle
          freemsg=.false.; servis9=' '
!          call timer('jt9fano ',0)
          call decode10(i1SoftSymbols,limit,msg,freemsg,i4Decoded6BitWords,mrs,mrs2,hint, &
               stophint,nlasttx,mycall,hiscall,hisgrid,rxfreq,s3,i4GrayCodedSym,hintdec, &
               chint,dofano,snrdb,tryhint)
!          call timer('jt9fano ',1)
          if(freemsg) servis9=','; if(hintdec) servis9=chint
          if(sync.lt.0.0 .or. snrdb.lt.dblim-2.0) sync=0.0
          nsync=int(sync)
          if(nsync.gt.10) nsync=10
          nsnr=nint(snrdb)
          ndrift=nint(drift/df3)

          if(msg.ne.'                      ') then
             dupe=.false.
             if(ndecoded.ne.0) then
                if(.not.hintdec) then
                   do k=1,200
                      if(dec(k)%i.eq.-1) exit
                      if(dec(k)%decoded.eq.msg .and. abs(dec(k)%i-carxf(icand)%i).lt.104) dupe=.true. ! 104 lt 45Hz
                   enddo
                else
                  do k=1,200
                     if(dec(k)%i.eq.-1) exit
                     if(dec(k)%decoded(1:10).eq.msg(1:10) .and. abs(dec(k)%i-carxf(icand)%i).lt.104) dupe=.true. !45Hz
                  enddo
                endif
             endif

             if(dupe) then
                done((irxf-1):(irxf+1))=.true.
                cycle
             endif
             if (associated(this%callback)) then
                msg26=msg//'    '
                call this%callback(nutc,nsnr,xdt,freq,msg26,servis9)
             endif

             ndecoded=ndecoded+1
!print *, carxf(icand)%i, carxf(icand)%ccfred
             if(nsnr.ge.15) noff=6; if(nsnr.le.14 .and. nsnr.ge.-5) noff=5
             if(nsnr.le.-6 .and. nsnr.ge.-10) noff=4
             if(nsnr.le.-11 .and. nsnr.ge.-16) noff=3; if(nsnr.le.-17 .and. nsnr.ge.-19) noff=2
             if(nsnr.eq.-20) noff=1; if(nsnr.le.-21) noff=0
             done((irxf-noff):(irxf+noff))=.true.
             dec(ndecoded)%i=carxf(icand)%i
             dec(ndecoded)%decoded=msg
             if(.not.freemsg .and. abs(freq-real(nfqso)).lt.2.0 .and. msg(1:4).eq.mycall(1:4)) then
                rxfdec(1)%msg=msg
                rxfdec(1)%xdt=xdt
                rxfdec(1)%freq=freq
             endif
             if(ipass.ne.2) call subtract10(i4Decoded6BitWords,freq,drift,xdt,i4GrayCodedSym,hintdec)
             exit
          else
             done(irxf)=.true.
          endif
          if(icand.gt.1) rxfreq=.false.
       enddo
    endif

    if(hint) then ! try to decode exact RX frequency with Hint
       rxfreq=.false.; dofano=.false.; fpk=real(nfqso)

       tryhint=.false.
       if(ipass.eq.2) then
          if(abs(real(nfqso)-rxfdec(1)%freq).lt.2.0 .and. len_trim(hiscall).gt.2) then
             i1=index(rxfdec(1)%msg,' ')
             i2=index(rxfdec(1)%msg((i1+1):),' ')
             if(i1.lt.8 .and. i2.lt.15) hiscallrxf=rxfdec(1)%msg((i1+1):(i1+i2-1))
             if(hiscall.eq.hiscallrxf .and. len_trim(hiscallrxf).ne.0) then
                fpk=rxfdec(1)%freq
                xdtrxf=rxfdec(1)%xdt
                if(xdtrxf.gt.-2.5 .and. xdtrxf.lt.5.0) tryhint=.true.
             endif
          endif
       endif

!          call timer('softsym ',0)
       call softsym10(npts8,nsps8,newdat,fpk,syncpk,snrdb,xdt,    &
            freq,drift,schk,i1SoftSymbols,qualf,syncro,mrs,mrs2,s3,tryhint,xdtrxf)
!          call timer('softsym ',1)
       sync=(syncpk+1)/4.0
!          if(syncro.lt.0.2 .or. syncro.gt.5.) cycle
       freemsg=.false.; servis9=' '
       if(xdt.gt.-0.5 .and. xdt.lt.1.5) rxfreq=.true.
       call decode10(i1SoftSymbols,limit,msg,freemsg,i4Decoded6BitWords,mrs,mrs2,hint, &
            stophint,nlasttx,mycall,hiscall,hisgrid,rxfreq,s3,i4GrayCodedSym,hintdec, &
            chint,dofano,snrdb,tryhint)
       if(freemsg) servis9=','; if(hintdec) servis9=chint
       if(sync.lt.0.0 .or. snrdb.lt.dblim-2.0) sync=0.0
       nsync=int(sync)
       if(nsync.gt.10) nsync=10
       nsnr=nint(snrdb)
       ndrift=nint(drift/df3)

       if(msg.ne.'                      ') then
          dupe=.false.
          if(ndecoded.ne.0) then
             do k=1,200
                if(dec(k)%i.eq.-1) exit
                if(dec(k)%decoded(1:10).eq.msg(1:10) .and. abs(dec(k)%i-irxf).lt.104) dupe=.true. !45Hz
             enddo
          endif
          if(dupe) done(irxf)=.true.
          if(.not.dupe) then
             if(associated(this%callback)) then
                msg26=msg//'    '
                call this%callback(nutc,nsnr,xdt,freq,msg26,servis9)
             endif

             ndecoded=ndecoded+1
!print *, carxf(icand)%i, carxf(icand)%ccfred
             if(nsnr.ge.15) noff=6; if(nsnr.le.14 .and. nsnr.ge.-5) noff=5
             if(nsnr.le.-6 .and. nsnr.ge.-10) noff=4
             if(nsnr.le.-11 .and. nsnr.ge.-16) noff=3; if(nsnr.le.-17 .and. nsnr.ge.-19) noff=2
             if(nsnr.eq.-20) noff=1; if(nsnr.le.-21) noff=0
             done((irxf-noff):(irxf+noff))=.true.
             dec(ndecoded)%i=irxf
             dec(ndecoded)%decoded=msg
             if(ipass.eq.1) then
                if(.not.freemsg .and. abs(freq-real(nfqso)).lt.2.0 .and. msg(1:4).eq.mycall(1:4)) then
                   rxfdec(1)%msg=msg
                   rxfdec(1)%xdt=xdt
                   rxfdec(1)%freq=freq
                endif
             endif
          endif
          call subtract10(i4Decoded6BitWords,freq,drift,xdt,i4GrayCodedSym,hintdec)
       endif
    endif
!goto 256 ! diag only
    ncurcand=0

    if(.not.nagain) then ! wideband candidate list decoding
       limit=200000*ntrials10
       if(nagainfil) limit=500000*facrxf
       if(swl) limit=2*limit
       limitr=limit/4
       rxfreq=.false.

       do icand=1,maxcand
!print *,ca(icand)%i,ca(icand)%ccfred
!print *, ca(icand)%i,'B'
          if(ca(icand)%i.lt.0) cycle; if(done(ca(icand)%i)) cycle
!print *, ca(icand)%i,'A'
          if(nagainfil .and. icand.gt.10) exit
!print *,done(ca(icand)%i),ccfok(ca(icand)%i)
          dofano=.true.
          ncurcand=ncurcand+1
          fpk=(ca(icand)%i-1)*df3
!print *,fpk
          if((.not.nagainfil .and. ncand0.gt.199 .and. ncurcand.gt.10) .or. ncurcand.gt.15) limit=limitr
!ncount=ncount+1; print *,ncount
          tryhint=.false.
!          call timer('softsym ',0)
          call softsym10(npts8,nsps8,newdat,fpk,syncpk,snrdb,xdt,    &
               freq,drift,schk,i1SoftSymbols,qualf,syncro,mrs,mrs2,s3,tryhint,xdtrxf)
!          call timer('softsym ',1)
          sync=(syncpk+1)/4.0
!print *,freq,syncpk,schk
!print *,freq,sync
!print *,syncro
!          if(sync.lt.1.0 .or. schk.lt.1.5) cycle
          if(sync.lt.1.0 .or. schk.lt.2.0 .or. qualf.lt.2.4) then
             if(ncurcand.gt.0) ncurcand=ncurcand-1
             dofano=.false.
          endif
          if(syncro.lt.0.2 .or. syncro.gt.5.) then
             if(ncurcand.gt.0) ncurcand=ncurcand-1
             cycle
          endif
!print *,qualf,syncro
          freemsg=.false.; servis9=' '
!          call timer('jt9fano ',0)
          call decode10(i1SoftSymbols,limit,msg,freemsg,i4Decoded6BitWords,mrs,mrs2,hint, &
               stophint,nlasttx,mycall,hiscall,hisgrid,rxfreq,s3,i4GrayCodedSym,hintdec, &
               chint,dofano,snrdb,tryhint)
!          call timer('jt9fano ',1)
          if(abs(fpk-float(nfqso)).lt.3.0 .and. freemsg) servis9=',' ! keep it for hide freemsg filter
          if(abs(fpk-float(nfqso)).ge.3.0 .and. freemsg) servis9='.'
          if(hintdec) servis9=chint

          if(sync.lt.0.0 .or. snrdb.lt.dblim-2.0) sync=0.0
          nsync=int(sync)
          if(nsync.gt.10) nsync=10
          nsnr=nint(snrdb)
          ndrift=nint(drift/df3)

          if(msg.ne.'                      ') then
             dupe=.false.
             if(ndecoded.ne.0) then
                do k=1,200
                   if(dec(k)%i.eq.-1) exit
                   if(dec(k)%decoded.eq.msg .and. abs(dec(k)%i-ca(icand)%i).lt.104) dupe=.true. ! 104 lt 45Hz
                enddo
             endif
!!print *, freq, ca(icand)%i
!!print *, msg, ca(icand)%ccfred
!print *, ca(icand)%i, ca(icand)%ccfred
             if(dupe) then
                iaa=max(1,ca(icand)%i-2)
                ibb=min(NSMAX10,ca(icand)%i+2)
                done(iaa:ibb)=.true.
                cycle
             endif
             if(associated(this%callback)) then
                msg26=msg//'    '
                call this%callback(nutc,nsnr,xdt,freq,msg26,servis9)
             end if
             ndecoded=ndecoded+1
!print *,schk,qualf
             if(nsnr.ge.15) noff=6; if(nsnr.le.14 .and. nsnr.ge.-5) noff=5
             if(nsnr.le.-6 .and. nsnr.ge.-10) noff=4
             if(nsnr.le.-11 .and. nsnr.ge.-16) noff=3; if(nsnr.le.-17 .and. nsnr.ge.-19) noff=2
             if(nsnr.eq.-20) noff=1; if(nsnr.le.-21) noff=0
             iaa=max(1,ca(icand)%i-noff)
             ibb=min(NSMAX10,ca(icand)%i+noff)
             done(iaa:ibb)=.true.
             dec(ndecoded)%i=ca(icand)%i
             dec(ndecoded)%decoded=msg
             if(ipass.ne.2) call subtract10(i4Decoded6BitWords,freq,drift,xdt,i4GrayCodedSym,hintdec)
          endif
          call cpu_time(t2)
          if(.not.swl .and. (t2-t1).gt.5.0) exit
          if(swl .and. (t2-t1).gt.10.0) exit
       enddo
    endif
!256 continue ! diag only
    if(ipass.eq.1) newdat=.true.
 enddo

!!h=default_header(12000,648000)
!!idd(1:624000)=nint(dd(1:624000))
!!idd(624001:)=0
!!open(10,file='out.wav',access='stream',status='unknown')
!!write(10) h,idd(1:648000)
!!close(10)

    return
  end subroutine decode
end module jt10_decode
