! This source code file was last time modified by Igor UA3DJY on August 21st, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine decode65a(newfft,f0,ntrials,naggressive,hint_on,mycall,hiscall, &
   hisgrid,sync2,a,dt,nft,decoded,ncand_lt9,hintedrxfreq,nfqso,hint,npass1,  &
   sync1_lt1,ncandcount,nstophint,freemsg,nlasttx,ipass,nsdecatt,isfalse, &
   bypass_ftrsd,ncand,jt65bc,nharmonicsdepth,showharmonics)

! Apply AFC corrections to a candidate JT65 signal, then decode it.

  use timer_module, only: timer
  use jt65_mod11 !ca and dtchk data structures

  parameter (NMAX=60*12000)          !Samples per 60 s
  complex cx(NMAX/8)                 !Data at 1378.125 samples/s
  complex c5x(NMAX/64)               !Data at 172.154 Hz
  complex c5a(512)
  complex w,wstep!,cpwrfactor
  real s2(66,126),s2b(66,126),s2c(66,126),a(2)
  data twopi/6.283185307/
!  save

  character decoded*22,mycall*12,hiscall*12,hisgrid*6,hint*1,isfalse*1
  real dtlow,dthigh
  integer nft,nsym
  logical(1) freemsg,hintedrxfreq,hintedw,newfft,chkdecode,hint_on,bypass_hintcq,bypass_hintall, &
             bypass_ftrsd,criterion10,criterion15,criterion20,npass1,ncand_lt9, &
             sync1_lt1,offset1,nstophint,hinteddyn,hintedrxfdt,jt65bc(2),showharmonics
  
  chkdecode=.false.; bypass_hintcq=.false.; bypass_hintall=.false.; offset1=.false.; dtlow=-0.5

  dthigh=float(naggressive) ! DT range in advanced settings tab
  if(naggressive.le.3) dtlow=-0.5*naggressive
  if(naggressive.eq.4) dtlow=-2.5
  if(naggressive.eq.5) dtlow=-3.5 
  
! Mix sync tone to baseband, low-pass filter, 
! downsample to 1378.125 Hz
! apply 2nd low-pass antialiasing filter
! downsample to 172.265625 Hz
!  call timer('filbig  ',0)
  call filbig(f0,newfft,cx,n5,sq0,c5x,n6)
!  call timer('filbig  ',1)

! cx has sample rate 12000*77125/672000 = 1378.125 Hz
! c5x has sample rate  1378.125/8 = 172.265625 Hz
! Find best DF, drift, curvature, and DT.

  fsample=1378.125/8

!  call timer('afc65b  ',0)
! Best fit for DF, drift, banana-coefficient, and dt. fsample = 172.154 S/s
  call afc65b(c5x,n6,fsample,a,ccfbest,dtbest)

!  call timer('afc65b  ',1)
!if(lstart) ccfbest=ccfbest*0.11*bstartdec*(2.0+bstartdec/22.0)/agcfacdec
  do ibig=1,2 ! deviation of the start sample
    if(ibig.eq.1) then
       offset1=.true.
       dtbest=dtbest-0.000251 ! remove decimation filter and coh. integrator delay
    else
    if(ipass.ge.4 .and. ipass.le.6 .and. nsdecatt.gt.1) go to 2
  !apply 2nd pass of ibig cycle for QSO freq only if npass.gt.1 and for crowded band
       if((.not.hintedrxfreq .and. .not.npass1) .or. ncand_lt9) then
          nft=0
          decoded='                      '
          exit
       endif
2      bypass_hintall=.true.
       offset1=.false.
    endif

!  if(.not.hinted .and. npass.eq.3) dtbest=dtbest+0.000251+0.0004

  dt=dtbest !return new, improved estimate of dt
! if (dt.lt.-0.74) return !limiting DT bottom edge to -3.9sec, eliminating some good decodes too
  sync2=1.5e-4*ccfbest/sq0                    !Constant is empirical
! avoiding some false hinted decodes caused by AF path overload
  if(sync2.gt.0.3) bypass_hintall=.true. ! -05dB SNR threshold
! now limiting QSO frequency hinted decoding to dtlow...dthigh DT range, advanced tab settings
  hintedrxfdt=.true.
  if((dt-1.0-2.6/1.2).lt.dtlow .or. (dt-1.0-2.6/1.2).gt.dthigh) hintedrxfdt=.false.  !26k samples shift +1sec
  if((nint(f0+a(1)).gt.nfqso+3) .or. (nint(f0+a(1)).lt.nfqso-3)) hintedrxfreq=.false.
! now limiting wideband CQ/CQ DX hinted decoding to -0.5...1.0 DT range
  hintedw=.true.
  hinteddyn=.true.
  if((dt-1.0-2.6/1.2).lt.-0.5 .or. (dt-1.0-2.6/1.2).gt.1.0) hintedw=.false.
  if((dt-1.0-2.6/1.2).lt.-1.0 .or. (dt-1.0-2.6/1.2).gt.2.5) hinteddyn=.false. 

if(.not.npass1) then
  criterion10=.false.;criterion15=.false.;criterion20=.false.
  if(ccfbest.lt.10000.0 .and. dtbest.lt.1.0) criterion10=.true.
  if(ccfbest.lt.2000.0 .and. dtbest.lt.1.5) criterion15=.true.
  if(ccfbest.lt.500.0 .and. dtbest.lt.2.0) criterion20=.true.
  
    if(.not.hintedrxfreq .and. .not.hintedrxfdt) then ! if (out of QSO frequency and out of DT*ccfbest range)
      if(criterion10 .or. criterion15 .or. criterion20) then
! reducing number of candidates for decoding at npass 2...4, hence reducing decoding time
        nft=0
        decoded='                      '
        return
      endif
    endif
! now eliminating some false FTRSD decodes
  if(hintedrxfreq .and. hintedrxfdt) then ! if on QSO frequency and (sync less than 1.0 or out of DT*ccfbest range)
    if(sync1_lt1 .or. criterion10 .or. criterion15 .or. criterion20)  bypass_ftrsd=.true.
  endif
endif

if(npass1 .and. sync1_lt1)  bypass_ftrsd=.true.
!now checking if candidate's SYNC is harmonic of the some deocded signal
!this candidate, if decoded, will be checked through the CALL3.txt data 
  ncandcount=ncandcount+1
  nctemp=ncandcount
  if(ncandcount.gt.MAXINTCAND) then
     print *, 'input audio signal is distorted'
     return
  endif
  if(ncandcount.gt.2) then
    do i=1,nctemp-1
    if(dtchk(i)%cand_decoded) then
      if(abs(dtbest-dtchk(i)%cand_dtbest).lt.0.005) then 
        ncandcount=ncandcount-1 
        chkdecode=.true.
      endif
    endif
    enddo
  endif
!now removing some false hinted decodes caused by harmonics of the SYNC signal
  if(chkdecode .and. f0.gt.1500.0 .and. ccfbest.gt.10000.0) bypass_hintcq=.true.

 if((bypass_ftrsd .and. hint_on .and. bypass_hintall) .or. (bypass_ftrsd .and. .not.hint_on)) then
! there is nothing to decode
    nft=0
    decoded='                      '
    return
 endif
  
! Apply AFC corrections to the time-domain signal
! Now we are back to using the 1378.125 Hz sample rate, enough to 
! accommodate the full JT65C bandwidth.
! Apply AFC corrections to the cx data

  w=1.0
  wstep=1.0
  x0=0.5*n5
  do i=1,n5
     if(mod(i,100).eq.1) then
        x=(float(i)-x0)/float(n5)
        dphi=(a(1) + x*a(2)) * (twopi/1378.125)
        wstep=cmplx(cos(dphi),sin(dphi))
     endif
     w=w*wstep
     cx(i)=w*cx(i)
  enddo

! Compute spectrum for each symbol.
  nsym=126
  nfft=512
  fltj=dtbest*1378.125
  j=nint(fltj)
  if(ibig.eq.2) then ! try other side
     deltaj=fltj-dtbest*1378.125
     if(deltaj.gt.0.0) then
        j=j-1
     else
        j=j+1
     endif
  endif

  c5a=cmplx(0.0,0.0)
!  call timer('sh_ffts ',0)
  do k=1,nsym
     do i=1,nfft
        j=j+1
        if(j.ge.1 .and. j.le.NMAX/8) then
           c5a(i)=cx(j)
        else
           c5a(i)=0.
        endif
     enddo
     call four2a(c5a,nfft,1,1,1)
     ijtb=1; ijtc=1
     do i=1,66
        if(ncand.ge.17 .or. sync2.ge.0.01) s2(i,k)=SQRT(real(c5a(i))**2 + aimag(c5a(i))**2)
        if(ncand.lt.17 .and. sync2.lt.0.01) s2(i,k)=real(c5a(i))**2 + aimag(c5a(i))**2
        s2b(i,k)=SQRT(real(c5a(ijtb))**2 + aimag(c5a(ijtb))**2)
        s2c(i,k)=SQRT(real(c5a(ijtc))**2 + aimag(c5a(ijtc))**2)
        ijtb=ijtb+2; ijtc=ijtc+3
     enddo
 enddo

!  call timer('sh_ffts ',1)
!  call timer('dec65b  ',0)
  call decode65b(s2,s2b,s2c,ntrials,hint_on,mycall,hiscall,hisgrid,nft,decoded, &
                 hintedrxfreq,hint,npass1,hintedw,bypass_hintcq,bypass_hintall, &
                 bypass_ftrsd,offset1,nstophint,freemsg,hinteddyn,dt,hintedrxfdt,nlasttx, &
                 ipass,sync2,isfalse,jt65bc,nharmonicsdepth,showharmonics)
!  call timer('dec65b  ',1)

  if(decoded.ne.'                      ') then
!  print *,a(1),a(2)
!if(abs(a(1)).gt.1.0) print *,'a1',a(1)
!if(abs(a(2)).gt.1.0) print *,'a2',a(2)
    if(chkdecode) ncandcount=ncandcount+1
    dtchk(ncandcount)%cand_dtbest=dtbest
    dtchk(ncandcount)%cand_decoded=.true.
    exit !exit from ibig if message is decoded on the first cycle
  endif
  enddo
  return
end subroutine decode65a
