! last time modified by Igor UA3DJY on 20200104

module jt9s_decode

  type :: jt9s_decoder
     procedure(jt9s_decode_callback), pointer :: callback
   contains
     procedure :: decode
  end type jt9s_decoder

  abstract interface
     subroutine jt9s_decode_callback (this, utc, snr, dt, freq, decoded, servis9)
       import jt9s_decoder
       implicit none
       class(jt9s_decoder), intent(inout) :: this
       integer, intent(in) :: utc
       integer, intent(in) :: snr
       real, intent(in) :: dt
       real, intent(in) :: freq
       character(len=26), intent(in) :: decoded
       character(len=1), intent(in) ::  servis9
     end subroutine jt9s_decode_callback
  end interface

contains

  subroutine decode(this,callback,nutc,nfqso,newdatin,npts8,nfa,nfb,nzhsym,filter,swl,  &
       nagain,nagainfil,ndepth,hint,stophint,nlasttx,mycall12,hiscall12,hisgrid6)
    use timer_module, only: timer
!npts8 = 74736
    include 'constants.f90'
    class(jt9s_decoder), intent(inout) :: this
    procedure(jt9s_decode_callback) :: callback
    integer, parameter :: MAXCANDRXF=5, MAXCAND=700
    character(len=12), intent(in) :: mycall12, hiscall12
    character(len=6), intent(in) :: hisgrid6
    integer, intent(in) :: nlasttx
    integer i4Decoded6BitWords(12),i4GrayCodedSym(69),mrs(69),mrs2(69)
    real s3(0:7,69)
    real t1,t2,trxf
    logical, intent(in) :: newdatin,nagain
    logical(1), intent(in) :: nagainfil,hint,stophint,filter,swl
    logical newdat,done(NSMAX)
    integer*1 i1SoftSymbols(207)
    character msg26*26,msg*22,servis9*1,mycall*6,hiscall*6,hisgrid*4,chint*1,hiscallrxf*6
    logical(1) freemsg,dupe,rxfreq,hintdec,dofano,tryhint
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
    save rxfdec

    this%callback => callback
    ndecoded=0; chint=' '; nsps=6912
    df3=1500.0/2048.0 ! 0.732421875 Hz
    irxf=nint(nfqso/df3)+1
    dec%i=-1; dec%decoded=''
    newdat=newdatin
    mycall=mycall12(1:6); hiscall=hiscall12(1:6); hisgrid=hisgrid6(1:4); hiscallrxf=''
    dupe=.false.; rxfreq=.false.; tryhint=.false.
    ntxmode=9 ! being used for compatibility with JT65+JT9 mode decode9 subroutine

    nsps8=nsps/8
    dblim=db(864.0/nsps8) - 26.2
    nf1=nfa
    ia=max(1,nint(nf1/df3))
    ib=min(NSMAX,nint(nfb/df3))

    if(nagain .or. nagainfil) newdat=.true.

!    if(nagain .or. nagainfil) then; th_wide=th_agn; th_rxf=th_agn; newdat=.false.; endif
!    if((nagain .or. nagainfil) .and. agcc.neqv.agcclast) newdat=.true.
    call cpu_time(t1)
  do ipass=1,2
    done=.false.

!    if(newdat) then
!       call timer('sync9   ',0)
       call sync9s(ca,carxf,nzhsym,ia,ib,irxf,ndepth,nagain,nagainfil,ipass,filter)
!       call timer('sync9   ',1)
!    endif

    if(.not.nagainfil) then ! RX frequency candidate list decoding
       limit=200000
       if(nagain) limit=3000000
!print *,'nagain',nagain
       do icand=1,maxcandrxf
!print *,'rxf', ca(icand)%i
!print *,(df3*(carxf(icand)%i-1))
!print *,icand
          if(nagain) then
             call cpu_time(trxf)
             if(.not.swl) then
                if((trxf-t1).gt.10.0) exit
             else
                if((trxf-t1).gt.30.0) exit
             endif
          endif
          if(carxf(icand)%i.le.0) cycle
          if(done(carxf(icand)%i)) cycle
          if(.not.swl .and. nagain .and. icand.ge.2) limit=300000
          fpk=df3*(carxf(icand)%i-1)
!print *,icand,fpk
          tryhint=.false.
!          call timer('softsym9s ',0)
          call softsym9s(npts8,nsps8,newdat,fpk,syncpk,snrdb,xdt,    &
               freq,drift,a3,schk,i1SoftSymbols,mrs,mrs2,s3,tryhint,xdtrxf)
!          call timer('softsym9s ',1)
          sync=(syncpk+1)/4.0
          dofano=.true.
          if(sync.lt.0.5 .or. schk.lt.1.) dofano=.false.
          rxfreq=.false.
          if(abs(freq-real(nfqso)).lt.2.0 .and. xdt.gt.-0.5 .and. xdt.lt.1.5) rxfreq=.true.
          freemsg=.false.; servis9=' '
!if(rxfreq) print *,fpk,freq,xdt
          call decode9(i1SoftSymbols,limit,msg,freemsg,i4Decoded6BitWords,mrs,mrs2,hint, &
               stophint,nlasttx,mycall,hiscall,hisgrid,rxfreq,s3,i4GrayCodedSym,hintdec, &
               chint,dofano,snrdb,ntxmode,tryhint)
          if(abs(fpk-real(nfqso)).lt.3.0 .and. freemsg) servis9=','
          if(abs(fpk-real(nfqso)).ge.3.0 .and. freemsg) servis9='.'
          if(hintdec) servis9=chint

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
                      if(dec(k)%decoded.eq.msg .and. abs(dec(k)%i-carxf(icand)%i).lt.24) dupe=.true.
                   enddo
                else
                   do k=1,200
                      if(dec(k)%i.eq.-1) exit
                      if(dec(k)%decoded(1:10).eq.msg(1:10) .and. abs(dec(k)%i-irxf).lt.104) dupe=.true. !45Hz
                   enddo
                endif
             endif

             if(dupe) cycle
!print *,carxf(icand)%ccfred
             if(associated(this%callback)) then
                msg26=msg//'    '
                call this%callback(nutc,nsnr,xdt,freq,msg26,servis9)
             endif
             if(.not.hintdec) then
                iaa=max(1,carxf(icand)%i-15)
                ibb=min(NSMAX,carxf(icand)%i+23)
             else
                iaa=max(1,irxf-1)
                ibb=min(NSMAX,irxf+1)
             endif
             ndecoded=ndecoded+1
             done(iaa:ibb)=.true.
             dec(ndecoded)%i=carxf(icand)%i
             dec(ndecoded)%decoded=msg
             if(.not.freemsg .and. abs(freq-real(nfqso)).lt.2.0 .and. msg(1:4).eq.mycall(1:4)) then
                rxfdec(1)%msg=msg
                rxfdec(1)%xdt=xdt
                rxfdec(1)%freq=freq
             endif
             if(ipass.ne.2) call subtract9s(i4Decoded6BitWords,freq,drift,xdt,i4GrayCodedSym,hintdec)
             exit ! RX freq decoding done, no need to try more candidates
          endif
       enddo
    endif


    if(hint) then ! try to decode exact RX frequency with Hint
       fpk=real(nfqso)
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
!       call timer('softsym9s ',0)
       call softsym9s(npts8,nsps8,newdat,fpk,syncpk,snrdb,xdt,    &
            freq,drift,a3,schk,i1SoftSymbols,mrs,mrs2,s3,tryhint,xdtrxf)
!       call timer('softsym9s ',1)

       sync=(syncpk+1)/4.0
       dofano=.false. 
       rxfreq=.false.
       if(abs(freq-real(nfqso)).lt.2.0 .and. xdt.gt.-0.5 .and. xdt.lt.1.5) rxfreq=.true.
       freemsg=.false.; servis9=' '

!if(rxfreq) print *,fpk,freq,xdt
       call decode9(i1SoftSymbols,limit,msg,freemsg,i4Decoded6BitWords,mrs,mrs2,hint, &
            stophint,nlasttx,mycall,hiscall,hisgrid,rxfreq,s3,i4GrayCodedSym,hintdec, &
            chint,dofano,snrdb,ntxmode,tryhint)

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
                if(dec(k)%decoded(1:10).eq.msg(1:10) .and. abs(dec(k)%i-irxf).lt.104) dupe=.true. !45Hz
             enddo
          endif
          if(dupe) cycle
          if(associated(this%callback)) then
             msg26=msg//'    '
             call this%callback(nutc,nsnr,xdt,freq,msg26,servis9)
          endif
         iaa=max(1,irxf-1)
         ibb=min(NSMAX,irxf+1)
         ndecoded=ndecoded+1
         done(iaa:ibb)=.true.
         dec(ndecoded)%i=irxf
         dec(ndecoded)%decoded=msg
         if(.not.freemsg .and. abs(freq-real(nfqso)).lt.2.0 .and. msg(1:4).eq.mycall(1:4)) then
            rxfdec(1)%msg=msg
            rxfdec(1)%xdt=xdt
            rxfdec(1)%freq=freq
         endif
         if(ipass.ne.2) call subtract9s(i4Decoded6BitWords,freq,drift,xdt,i4GrayCodedSym,hintdec)
      endif
    endif


    if(.not.nagain) then ! wideband candidate list decoding
       limit=5000
       if(ndepth.eq.2) limit=10000
       if(ndepth.ge.3) limit=30000
       if(nagainfil) limit=300000
       if(swl) limit=limit*2

       do icand=1,maxcand
          call cpu_time(t2)
          if(.not.swl) then
             if(ipass.eq.1 .and. (t2-t1).gt.5.0) exit
             if(ipass.eq.2 .and. (t2-t1).gt.10.0) exit
          else
             if(ipass.eq.1 .and. (t2-t1).gt.9.0) exit
             if(ipass.eq.2 .and. (t2-t1).gt.30.0) exit
          endif
          if(nagainfil .and. icand.gt.5) exit
!print *,'W', ca(icand)%i
          if(ca(icand)%i.le.0) cycle
          if(done(ca(icand)%i)) cycle
          fpk=df3*(ca(icand)%i-1)

          tryhint=.false.
!          call timer('softsym9s ',0)
          call softsym9s(npts8,nsps8,newdat,fpk,syncpk,snrdb,xdt,    &
               freq,drift,a3,schk,i1SoftSymbols,mrs,mrs2,s3,tryhint,xdtrxf)
!          call timer('softsym9s ',1)
          sync=(syncpk+1)/4.0
          dofano=.true.
          if(sync.lt.1.0 .or. schk.lt.1.5) cycle
          rxfreq=.false.
          if(abs(freq-real(nfqso)).lt.2.0 .and. xdt.gt.-0.5 .and. xdt.lt.1.5) rxfreq=.true.
          freemsg=.false.; servis9=' '

!if(rxfreq) print *,fpk,freq,xdt
          call decode9(i1SoftSymbols,limit,msg,freemsg,i4Decoded6BitWords,mrs,mrs2,hint, &
               stophint,nlasttx,mycall,hiscall,hisgrid,rxfreq,s3,i4GrayCodedSym,hintdec, &
               chint,dofano,snrdb,ntxmode,tryhint)

          if(abs(fpk-real(nfqso)).lt.3.0 .and. freemsg) servis9=','
          if(abs(fpk-real(nfqso)).ge.3.0 .and. freemsg) servis9='.'
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
                   if(dec(k)%decoded.eq.msg .and. abs(dec(k)%i-ca(icand)%i).lt.24) dupe=.true.
                enddo
             endif
!print *,ipass,fpk
!print *,msg
!endif
             if(dupe) cycle
!print *,ipass,(t2-t1)
             if(associated(this%callback)) then
                msg26=msg//'    '
                call this%callback(nutc,nsnr,xdt,freq,msg26,servis9)
             endif
             iaa=max(1,ca(icand)%i-15)
             ibb=min(NSMAX,ca(icand)%i+23)
             ndecoded=ndecoded+1
             done(iaa:ibb)=.true.
             dec(ndecoded)%i=ca(icand)%i
             dec(ndecoded)%decoded=msg
             if(.not.freemsg .and. abs(freq-real(nfqso)).lt.2.0 .and. msg(1:4).eq.mycall(1:4)) then
                rxfdec(1)%msg=msg
                rxfdec(1)%xdt=xdt
                rxfdec(1)%freq=freq
             endif
             if(ipass.ne.2) call subtract9s(i4Decoded6BitWords,freq,drift,xdt,i4GrayCodedSym,hintdec)
          endif
       enddo
    endif
    if(ipass.eq.1) newdat=.true.
  enddo
    return
  end subroutine decode
end module jt9s_decode
