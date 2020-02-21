! This source code file was last time modified by Igor UA3DJY on June 10th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine decode9(i1SoftSymbols,limit,msg,freemsg,i4Decoded6BitWords,mrs,mrs2,hint, &
           stophint,nlasttx,mycall,hiscall,hisgrid,rxfreq,s3,i4GrayCodedSym,hintdec, &
           chint,dofano,snrdb,ntxmode,tryhint)

  character msg*22,decoded*22,mycall*6,hiscall*6,hisgrid*4,chint*1
  real s3(0:7,69)
  integer i4Decoded6BitWords(12),mrs(69),mrs2(69),i4GrayCodedSym(69)
  integer*1 i1SoftSymbols(207)
  integer nlasttx,nft
  logical(1) freemsg,hint,stophint,rxfreq,hintdec,dofano,tryhint

! nlasttx last transmitted message matrix
! nlasttx   last TX message  expected RX message
!   0           Halt TX              -
!   1           GRID             -01, and may be again CQ DXCall based
!   2           -01                R-01, and may be again GRID DXCall based
!   3           R-01            RRR/RR73/73,  and may be again -01
!   4           RRR/RR73        RRR/RR73/73
!   5           73/freemsg      RRR/RR73/73
!   6           CQ              GRID fm CALL3
!   8           freemsg         RRR/RR73/73

  nft=0; hintdec=.false.; chint=' '; msg='                      '

  if(dofano) then
     call jt9fano(i1SoftSymbols,limit,msg,freemsg,i4Decoded6BitWords)
     if(msg.ne.'                      ') nft=1
  endif

  hintth=-24.0
  if(tryhint) hintth=28.0
!if(nft.eq.0) then ! diag only
  if(.not.stophint .and. hint .and. ntxmode.eq.9 .and. nft.eq.0 .and. snrdb.lt.hintth) then
     if(rxfreq) then
        if(nlasttx.eq.1) then
           call hintdxr9(mrs,mrs2,mycall,hiscall,decoded,hintdec,s3,i4GrayCodedSym)
           if(hintdec) go to 2
        endif
        if(nlasttx.eq.2) then
           call hintdxrr9(mrs,mrs2,mycall,hiscall,decoded,hintdec,s3,i4GrayCodedSym)
           if(hintdec) go to 2
           call hintdxgrid9(mrs,mrs2,mycall,hiscall,hisgrid,decoded,hintdec,s3,i4GrayCodedSym)
           if(hintdec) go to 2
        endif
        if(nlasttx.eq.3) then
           call hintdx739(mrs,mrs2,mycall,hiscall,decoded,hintdec,s3,i4GrayCodedSym)
           if(hintdec) go to 2
           call hintdxr9(mrs,mrs2,mycall,hiscall,decoded,hintdec,s3,i4GrayCodedSym)
           if(hintdec) go to 2
        endif
        if(nlasttx.eq.4 .or. nlasttx.eq.5 .or. nlasttx.eq.8) then
           call hintdx739(mrs,mrs2,mycall,hiscall,decoded,hintdec,s3,i4GrayCodedSym)
           if(hintdec) go to 2
        endif
        if(nlasttx.eq.6) then !CALL3 based decoder mycall+hiscall+grid message
           call hintrxgrid9(mrs,mrs2,mycall,decoded,hintdec,s3,i4GrayCodedSym)
           if(hintdec) go to 2
        endif
     endif
2    if(hintdec) then 
        nft=2; chint='*'; freemsg=.false.
        msg=decoded
     else
        msg='                      '
     endif
  endif

  return
end subroutine decode9
