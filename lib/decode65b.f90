! This source code file was last time modified by Igor UA3DJY on August 19th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine decode65b(s2,s2b,s2c,ntrials,hint_on,mycall,hiscall,hisgrid,nft,decoded,hintedrxfreq,hint, &
           npass1,hintedw,bypass_hintcq,bypass_hintall,bypass_ftrsd,offset1,nstophint,freemsg,hinteddyn,&
           dt,hintedrxfdt,nlasttx,ipass,sync2,isfalse,jt65bc,nharmonicsdepth,showharmonics)

  use jt65_mod2, only : s3
  use jt65_mod4 ! prc(126)
  
  real s2(66,126),s2b(66,126),s2c(66,126),s3b(64,63),s3c(64,63),syncpwr,storedsyncpwr1
  character mycall*12,hiscall*12,hisgrid*6,decoded*22,hint*1,isfalse*1
  integer mdat(63),nft
  logical(1) hintedrxfreq,hintedw,freemsg,hint_on,msg_decoded,bypass_hintcq, &
             bypass_ftrsd,npass1,first,offset1,bypass_hintall,nstophint,hinteddyn, &
             hintedrxfdt,jt65bc(2),showharmonics
  data first/.true./,storedsyncpwr1/0.0/
  save first,mdat,storedsyncpwr1

  if(first) then ! Determine locations of data and reference symbols
  nsym=126

  m=0
  do i=1,nsym
     if(.not.prc(i)) then
        m=m+1
        mdat(m)=i
     endif
  enddo
  first=.false.
  endif

! dupe candidates have the same sync signal power shall not go to decoders
  syncpwr=0.
  do i=1,126
     if(prc(i)) then
        syncpwr=syncpwr+s2(1,i)
     endif
  enddo
  if(syncpwr.eq.storedsyncpwr1) then
     nft=0
     decoded='                      '
     msg_decoded=.false.
     return
  endif
  if(offset1) storedsyncpwr1=syncpwr
  
  do j=1,63
     k=mdat(j)                       !Points to data symbol
     do i=1,64
        s3(i,j)=s2(i+2,k)
        s3b(i,j)=s2b(i+2,k)
        s3c(i,j)=s2c(i+2,k)
     enddo
  enddo
  
  call extract(s3b,s3c,ntrials,hint_on,mycall,hiscall,hisgrid,msg_decoded,decoded, &
               freemsg,nft,hintedrxfreq,hint,npass1,hintedw,bypass_hintcq, &
               bypass_hintall,bypass_ftrsd,nstophint,hinteddyn,dt,hintedrxfdt,syncpwr, &
               nlasttx,ipass,sync2,isfalse,jt65bc,nharmonicsdepth,showharmonics)

  return
end subroutine decode65b
