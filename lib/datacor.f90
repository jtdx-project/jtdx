! This source code file was last time modified by Igor UA3DJY on July 22nd, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine datacor(datapwr,datacorr)

  use jt65_mod2, only : s3,correct

  real datapwr,datacorr

! calculate information tones power and datapower/syncpower ratio
  datapwr=0.0
  do i=1,63
     datapwr=datapwr+s3(correct(i)+1,i)
  enddo

! use short correlation function in time and frequency domains
  timeplus=0.; timeminus=0.; freqplus=0.; freqminus=0.
  do i=1,63
     k=i+1
     m=i-1
     if(i.eq.1) m=2
     if(i.eq.63) k=62
     timeplus=timeplus+s3(correct(i)+1,k)
     timeminus=timeminus+s3(correct(i)+1,m)
     if(correct(i).lt.63) then
        j=1
     else
        j=-1
     endif
     if(correct(i).gt.1) then
        n=-1
     else
        n=1
     endif
     freqplus=freqplus+s3(correct(i)+1+j,i)
     freqminus=freqminus+s3(correct(i)+1+n,i)
  enddo
  datacorr=datapwr*4.0/(timeplus+timeminus+freqplus+freqminus)

  return
end subroutine datacor
