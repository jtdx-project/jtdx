module jt9_decode

  type :: jt9_decoder
     procedure(jt9_decode_callback), pointer :: callback
   contains
     procedure :: decode
  end type jt9_decoder

  abstract interface
     subroutine jt9_decode_callback (this, utc, snr, dt, freq, decoded, servis9)
       import jt9_decoder
       implicit none
       class(jt9_decoder), intent(inout) :: this
       integer, intent(in) :: utc
       integer, intent(in) :: snr
       real, intent(in) :: dt
       real, intent(in) :: freq
       character(len=26), intent(in) :: decoded
       character(len=1), intent(in) ::  servis9
     end subroutine jt9_decode_callback
  end interface

contains

  subroutine decode(this,callback,nutc,nfqso,newdatin,npts8,nfa,nfsplit,nfb,ntol,  &
       nzhsym,nagain,nagainfil,ndepth,nmode,hint,stophint,nlasttx,mycall12,hiscall12,hisgrid6,ntxmode)
    use timer_module, only: timer
!npts8 = 74736
    include 'constants.f90'
    class(jt9_decoder), intent(inout) :: this
    procedure(jt9_decode_callback) :: callback
    character(len=12), intent(in) :: mycall12, hiscall12
    character(len=6), intent(in) :: hisgrid6
    integer, intent(in) :: nlasttx
    integer i4Decoded6BitWords(12),i4GrayCodedSym(69),mrs(69),mrs2(69)
    real ccfred(NSMAX),red2(NSMAX),s3(0:7,69)
    logical, intent(in) :: newdatin,nagain
    logical(1), intent(in) :: nagainfil,hint,stophint
    logical newdat,ccfok(NSMAX),done(NSMAX)
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
    save ccfred,red2,rxfdec

    this%callback => callback
    ndecoded=0; chint=' '; nsps=6912; ncycles=1
    df3=1500.0/2048.0
    if(nfqso.lt.nfa) then
      if(nfsplit.gt.nfa .and. nfsplit.lt.(nfb-18)) then; nfqso=nfsplit
      else; nfqso=nfa
      endif
    endif
    irxf=nint(nfqso/df3)+1
    dec%i=-1; dec%decoded=''
    newdat=newdatin
    mycall=mycall12(1:6); hiscall=hiscall12(1:6); hisgrid=hisgrid6(1:4); hiscallrxf=''
    tstep=0.5*nsps/12000.0                      !Half-symbol step (seconds)
    dupe=.false.; rxfreq=.false.; tryhint=.false.

    nf1=nfa
    if(nmode.eq.74) then ! 65+9
      if(nfsplit.gt.0 .and. nfsplit.lt.4983) then; nf1=nfsplit
      else; nf1=1
      endif
      if(nfb.lt.1 .or. nfb.gt.4982) nfb=4982
    endif
    if(nagain .or. nagainfil) then
       if(nfqso.ge.21) nf1=nfqso-20; if(nfqso.lt.21) nf1=nfqso
       if(nfqso.le.4962) then; nfb=nfqso+20
       else if(nfqso.gt.4962 .and. nfqso.lt.4983) then; nfb=nfqso
       else; nfb=4982
       endif
    endif
    ia=max(1,nint(nf1/df3))
    ib=min(NSMAX,nint(nfb/df3))
    lag1=-int(2.5/tstep + 0.9999)
    lag2=int(5.0/tstep + 0.9999)

  do ipass=1,2
!    call timer('sync9   ',0)
    call sync9(nzhsym,lag1,lag2,ia,ib,ccfred,red2)
!    call timer('sync9   ',1)
  
    done=.false.
    ipeak1=0; ipeak2=0; ipeak3=0; ipeak4=0; ipeak5=0
    if(.not.nagain .and. (.not.nagainfil)) then
       if(nfqso.ge.4) infa=nint((nfqso-3)/df3)
       if(nfqso.lt.4) infa=nint((nfqso)/df3)
       if(nfqso.le.4979) infb=nint((nfqso+3)/df3)
       if(nfqso.gt.4979) infb=nint((nfqso)/df3)
       if(infa.gt.0 .and. infb.gt.0 .and. infa.lt.NSMAX .and. infb.lt.NSMAX) then
          ccfredmax=0.
          do i=infa,infb
             if(ccfred(i).gt.ccfredmax) then
                ipeak1=i
                ccfredmax=ccfred(i)
             endif
          enddo
          ccfredmax=0.
          do i=infa,infb
             if(ccfred(i).gt.ccfredmax .and. i.ne.ipeak1) then
                ipeak2=i
                ccfredmax=ccfred(i)
             endif
          enddo
          ccfredmax=0.
          do i=infa,infb
             if(ccfred(i).gt.ccfredmax .and. i.ne.ipeak1 .and. i.ne.ipeak2) then
                ipeak3=i
                ccfredmax=ccfred(i)
             endif
          enddo
          ccfredmax=0.
          do i=infa,infb
             if(ccfred(i).gt.ccfredmax .and. i.ne.ipeak1 .and. i.ne.ipeak2 .and. i.ne.ipeak3) then
                ipeak4=i
                ccfredmax=ccfred(i)
             endif
          enddo
          ccfredmax=0.
          do i=infa,infb
             if(ccfred(i).gt.ccfredmax .and. i.ne.ipeak1 .and. i.ne.ipeak2 .and. i.ne.ipeak3 .and. i.ne.ipeak4) then
                ipeak5=i
                ccfredmax=ccfred(i)
             endif
          enddo
       endif
    endif
	
    if(nagain .or. nagainfil) then
    ipeak1=0; ipeak2=0; ipeak3=0; ipeak4=0; ipeak5=0
       if(nagain) then
         if(nfqso.ge.4) infa=nint((nfqso-3)/df3)
         if(nfqso.lt.4) infa=nint((nfqso)/df3)
         if(nfqso.le.4979) infb=nint((nfqso+3)/df3)
         if(nfqso.gt.4979) infb=nint((nfqso)/df3)
       endif
       if(nagainfil) then
          if(nfqso.ge.21) infa=nint((nfqso-20)/df3)
          if(nfqso.lt.21) infa=nint((nfqso)/df3)
          if(nfqso.le.4962) infb=nint((nfqso+20)/df3)
          if(nfqso.gt.4962) infb=nint((nfqso)/df3)
       endif
       if(infa.gt.0 .and. infb.gt.0 .and. infa.lt.NSMAX .and. infb.lt.NSMAX) then
          ccfredmax=0.
          do i=infa,infb
             if(ccfred(i).gt.ccfredmax) then
                ipeak1=i
                ccfredmax=ccfred(i)
             endif
          enddo
          ccfredmax=0.
          do i=infa,infb
             if(ccfred(i).gt.ccfredmax .and. i.ne.ipeak1) then
                ipeak2=i
                ccfredmax=ccfred(i)
             endif
          enddo
          ccfredmax=0.
          do i=infa,infb
             if(ccfred(i).gt.ccfredmax .and. i.ne.ipeak1 .and. i.ne.ipeak2) then
                ipeak3=i
                ccfredmax=ccfred(i)
             endif
          enddo
          ccfredmax=0.
          do i=infa,infb
             if(ccfred(i).gt.ccfredmax .and. i.ne.ipeak1 .and. i.ne.ipeak2 .and. i.ne.ipeak3) then
                ipeak4=i
                ccfredmax=ccfred(i)
             endif
          enddo
          ccfredmax=0.
          do i=infa,infb
             if(ccfred(i).gt.ccfredmax .and. i.ne.ipeak1 .and. i.ne.ipeak2 .and. i.ne.ipeak3 .and. i.ne.ipeak4) then
                ipeak5=i
                ccfredmax=ccfred(i)
             endif
          enddo
       endif
    endif

    nsps8=nsps/8
    df8=1500.0/nsps8
    dblim=db(864.0/nsps8) - 26.2

    ia1=1                         !quel compiler gripe
    ib1=1                         !quel compiler gripe
    if(hint .and. ntxmode.eq.9) then; ncycles=2; else; ncycles=1; endif
    do nqd=ncycles,0,-1
!if(nqd.eq.0) exit !diag only
       if(nqd.eq.1 .and. nagainfil) cycle
       limit=5000
       ccflim=3.0
       red2lim=1.6
       if(ndepth.eq.2) then
          limit=10000
          ccflim=2.7
       endif
       if(ndepth.ge.3) then
          limit=30000
          ccflim=2.5
       endif
       if(nqd.eq.1) then
          limit=200000
          ccflim=2.4
       endif
       if(nagain) then
          limit=5000000
          ccflim=2.3
       endif
       if(nagainfil) then
          limit=300000
          ccflim=2.3
       endif

       ccfok=.false.

       ntol1=ntol !supress compiler warning
       if(nqd.ge.1) then
          nfa1=nfqso-2
          nfb1=nfqso+2
          ia=max(1,nint(nfa1/df3))
          ib=min(NSMAX,nint(nfb1/df3))
          ccfok(ia:ib)=(ccfred(ia:ib).gt.(ccflim-2.0)) .and.               &
               (red2(ia:ib).gt.(red2lim-1.0))
          ia1=ia
          ib1=ib
       else
          nfa1=nf1
          nfb1=nfb
          ia=max(1,nint(nfa1/df3))
          ib=min(NSMAX,nint(nfb1/df3))
          do i=ia,ib
             ccfok(i)=ccfred(i).gt.ccflim .and. red2(i).gt.red2lim
          enddo
          ccfok(ia1:ib1)=.false.
       endif

       fgood=0.
       do i=ia,ib
          if((nagain.or.nagainfil.or.nqd.eq.1) .and. i.ne.ipeak1 .and. i.ne.ipeak2 .and. i.ne.ipeak3 &
              .and. i.ne.ipeak4 .and. i.ne.ipeak5) cycle
          if((nagain.or.nagainfil.or.nqd.eq.1) .and. i.eq.ipeak1 .and. ipeak1.eq.0) cycle
          if((nagain.or.nagainfil.or.nqd.eq.1) .and. i.eq.ipeak2 .and. ipeak2.eq.0) cycle
          if((nagain.or.nagainfil.or.nqd.eq.1) .and. i.eq.ipeak3 .and. ipeak3.eq.0) cycle
          if((nagain.or.nagainfil.or.nqd.eq.1) .and. i.eq.ipeak4 .and. ipeak4.eq.0) cycle
          if((nagain.or.nagainfil.or.nqd.eq.1) .and. i.eq.ipeak5 .and. ipeak5.eq.0) cycle

          if(nqd.ne.2 .and. done(i) .or. (.not.ccfok(i))) cycle
          f=(i-1)*df3

          if(nqd.ge.1 .or.                                                   &
               (ccfred(i).ge.ccflim .and. abs(f-fgood).gt.10.0*df8)) then

!             call timer('softsym ',0)
             fpk=df3*(i-1)
             if(nqd.eq.2) fpk=real(nfqso)
             tryhint=.false.
             if(nqd.eq.2 .and. ipass.eq.2) then
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
             call softsym(npts8,nsps8,newdat,fpk,syncpk,snrdb,xdt,    &
                  freq,drift,a3,schk,i1SoftSymbols,mrs,mrs2,s3,tryhint,xdtrxf,nmode)
!             call timer('softsym ',1)

             sync=(syncpk+1)/4.0
             dofano=.true.
             if(nqd.eq.2) dofano=.false. 
             if(nqd.eq.1 .and. ((sync.lt.0.5) .or. (schk.lt.1.0))) dofano=.false.
             if(nqd.eq.0 .and. ((sync.lt.1.0) .or. (schk.lt.1.5))) cycle

             rxfreq=.false.
             if(abs(freq-real(nfqso)).lt.2.0 .and. xdt.gt.-0.5 .and. xdt.lt.1.5) rxfreq=.true.
             freemsg=.false.; servis9=' '

!if(rxfreq) print *,fpk,freq,xdt
             call decode9(i1SoftSymbols,limit,msg,freemsg,i4Decoded6BitWords,mrs,mrs2,hint, &
                  stophint,nlasttx,mycall,hiscall,hisgrid,rxfreq,s3,i4GrayCodedSym,hintdec, &
                  chint,dofano,snrdb,ntxmode,tryhint)
             if(nqd.ne.2) then
                if(abs(f-real(nfqso)).lt.3.0 .and. freemsg) servis9=','
                if(abs(f-real(nfqso)).ge.3.0 .and. freemsg) servis9='.'
             endif
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
                         if(dec(k)%decoded.eq.msg .and. abs(dec(k)%i-i).lt.24) dupe=.true.
                      enddo
                   else
                     do k=1,200
                        if(dec(k)%i.eq.-1) exit
                        if(dec(k)%decoded(1:10).eq.msg(1:10) .and. abs(dec(k)%i-irxf).lt.104) dupe=.true. !45Hz
                     enddo
                   endif
                endif

                if(dupe) cycle
 
                if (associated(this%callback)) then
                   msg26=msg//'    '
                   call this%callback(nutc,nsnr,xdt,freq,msg26,servis9)
                end if
                if(.not.hintdec) then
                   iaa=max(1,i-1)
                   ibb=min(NSMAX,i+11)
                else
                   iaa=max(1,irxf-1)
                   ibb=min(NSMAX,irxf+1)
                endif
                fgood=f
                ndecoded=ndecoded+1
                ccfok(iaa:ibb)=.false.
                done(iaa:ibb)=.true.
                if(nqd.lt.2) then; dec(ndecoded)%i=i; else; dec(ndecoded)%i=irxf; endif
                dec(ndecoded)%decoded=msg
                if(.not.freemsg .and. abs(freq-real(nfqso)).lt.2.0 .and. msg(1:4).eq.mycall(1:4)) then
                   rxfdec(1)%msg=msg
                   rxfdec(1)%xdt=xdt
                   rxfdec(1)%freq=freq
                endif
                if(ipass.ne.2) call subtract9(i4Decoded6BitWords,freq,drift,xdt,i4GrayCodedSym,hintdec)
             endif
          endif
          if(nqd.eq.2) exit
       enddo
       if(nagain) exit
    enddo
    if(ipass.eq.1) newdat=.true.
  enddo
    return
  end subroutine decode
end module jt9_decode
