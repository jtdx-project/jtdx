! This source code file was last time modified by Igor Chernikov UA3DJY on June 11th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine s3avg(nsave,mode65,nutc,nhz,xdt,npol,ntol,s3,nsum,nkv,decoded)

! Save the current synchronized spectra, s3(64,63), for possible
! decoding of average.

  real s3(64,63)                        !Synchronized spectra for 63 symbols
  real s3a(64,63,64)                    !Saved spectra
  real s3b(64,63)                       !Average spectra
  integer iutc(64),ihz(64),ipol(64)
  real dt(64)
  character*22 decoded
  logical first
  logical(1) freemsg
  data first/.true./
  save

  if(first) then
     iutc=-1
     ihz=0
     ipol=0
     first=.false.
     ihzdiff=min(100,ntol)
     dtdiff=0.2
  endif

  do i=1,64
     if(nutc.eq.iutc(i) .and. abs(nhz-ihz(i)).lt.ihzdiff) then
        nsave=mod(nsave-1+64,64)+1
        go to 10
     endif
  enddo
  
  iutc(nsave)=nutc                          !Save UTC
  ihz(nsave)=nhz                            !Save freq in Hz
  ipol(nsave)=npol                          !Save pol
  dt(nsave)=xdt                             !Save DT
  s3a(1:64,1:63,nsave)=s3                   !Save the spectra

10 s3b=0.
  do i=1,64                                 !Accumulate avg spectra
     if(iutc(i).lt.0) cycle
     if(mod(iutc(i),2).ne.mod(nutc,2)) cycle !Use only same sequence
     if(abs(nhz-ihz(i)).gt.ihzdiff) cycle   !Freq must match
     if(abs(xdt-dt(i)).gt.dtdiff) cycle     !DT must match
     s3b=s3b + s3a(1:64,1:63,i)
     nsum=nsum+1
  enddo
 
  decoded='                      '
  if(nsum.ge.2) then                        !Try decoding the sverage
     nadd=mode65*nsum
     call extract(s3b,nadd,ncount,nhist,decoded,freemsg)     !Extract the message
     nkv=nsum
     if(ncount.lt.0) then 
        nkv=0
        decoded='                      '
     endif
  endif

  return
end subroutine s3avg
