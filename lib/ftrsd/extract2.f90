! This source code file was last time modified by Igor UA3DJY on November 27th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine extract2(s3,nadd,ntrials,param,msg)

  real s3(64,63)
  real tmp(4032)
  integer np1(0:7,0:7),np2(0:7,0:7),np0(0:7,0:7)
  integer ns(0:7,0:7)
  integer perr(0:7,0:7),pmr2(0:7,0:7)
  character msg*22
  integer dat4(12)
  integer mrsym(0:62),mr2sym(0:62),mrprob(0:62),mr2prob(0:62)
  integer correct(0:62)
  integer param(0:7)
  integer indx(0:62)
  logical first
  real*8 tt
  common/extcom/ntdecode
  data ndone/0/,ngood/0/,nbad/0/,first/.true./
  save

  if(first) then
     np1=0
     np2=0
     np0=0
     ns=0
     first=.false.
  endif

  nfail=0
1 call demod64a(s3,nadd,mrsym,mrprob,mr2sym,mr2prob,ntest,nlow)

!  if(ntest.lt.50 .or. nlow.gt.20) then
!     ncount=-999                         !Flag bad data
!     go to 900
!  endif

  call chkhist(mrsym,nhist,ipk)
  if(nhist.ge.20) then
     nfail=nfail+1
     call pctile(s3,tmp,4032,50,base)     ! ### or, use ave from demod64a ?
     do j=1,63
        s3(ipk,j)=base
     enddo
     go to 1
  endif

  call graycode(mrsym,63,-1)
  call interleave63(mrsym,-1)
  call interleave63(mrprob,-1)

  call graycode(mr2sym,63,-1)
  call interleave63(mr2sym,-1)
  call interleave63(mr2prob,-1)

  nverbose=0
  call ftrsd2(mrsym,mrprob,mr2sym,mr2prob,ntrials,nverbose,correct,   &
       param,indx,tt,ntry)
  ncandidates=param(0)
  nhard=param(1)
  nsoft=param(2)
  nera=param(3)
  ngmd=param(4)
  ndone=ndone+1

  do i=1,12
     dat4(i)=correct(12-i)
  enddo


  msg='                      '
! if(nhard.ge.0) then
  if(nhard.ge.0 .and. nhard.le.42 .and. nsoft.le.32 .and.              &
       (nhard+nsoft).le.73) then
     call unpackmsg(dat4,msg) !Unpack the user message
     if(msg.eq.'VK7MO K1JT FN20       ') then
        ngood=ngood+1
        do k=0,62
           j=indx(k)
           i=(62-j)
           p1=mrprob(i)/255.0 + 1.e-10
           p2=mr2prob(i)/255.0
           ii=k/8
           jj=7.9999*p2/p1
           ns(ii,jj)=ns(ii,jj)+1
           if(correct(j).eq.mrsym(i)) then
              np1(ii,jj)=np1(ii,jj)+1
           else
              np0(ii,jj)=np0(ii,jj)+1
           endif
           if(correct(j).eq.mr2sym(i)) np2(ii,jj)=np2(ii,jj)+1
        enddo
     else
        nbad=nbad+1
     endif
  endif
  fgood=float(ngood)/ndone
  fbad=float(nbad)/ndone

  nboth=nhard+nsoft
  if(nhard.lt.0) then
     nsoft=99
     nera=99
     nboth=99
  endif
  write(*,1010) ndone,fgood,fbad,ncandidates,nhard,nsoft,nera,nboth,   &
       ntry,tt,msg
  write(32,1010) ndone,fgood,fbad,ncandidates,nhard,nsoft,nera,nboth,  &
       ntry,tt,msg
1010 format(i4,2f7.3,i7,4i3,i8,f8.1,1x,a22)
  flush(32)

  if(msg.eq.'VK7MO K1JT FN20       ') then
     write(33,1012) ndone,ngood,nbad,ntry,log10(float(1+ntry))
1012 format(4i8,f8.3)
     flush(33)
  endif

  rewind 40
  write(40,1080)
1080 format('Totals:')
  write(40,1090) ns
1090 format(8i7)
  write(40,1091)
1091 format(/'error:')
  write(40,1090) np0
  write(40,1092)
1092 format(/'sym = mrsym:')
  write(40,1090) np1
  write(40,1093)
1093 format(/'sym = mr2sym:')
  write(40,1090) np2

  write(40,1095)
1095 format(/'Probability of error:')
  perr=nint(100.0*float(np0)/(ns+0.001))
  write(40,1096) perr
1096 format(8i7)

  write(40,1097)
1097 format(/'P(mr2 correct | mr not correct) :')
  pmr2=nint(100.0*float(np2)/(np0+0.001))
  write(40,1096) pmr2
  flush(40)

  return
end subroutine extract2
