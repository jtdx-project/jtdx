! This source code file was last time modified by Igor UA3DJY on July 22nd, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine demod64a(mrsym,mrprob,mr2sym,mr2prob)

! Demodulate the 64-bin spectra for each of 63 symbols in a frame.

! Parameters
!    mrsym    most reliable symbol value
!    mr2sym   second most likely symbol value
!    mrprob   probability that mrsym was the transmitted value
!    mr2prob  probability that mr2sym was the transmitted value

  use jt65_mod2, only : s3

  integer mrsym(63),mrprob(63),mr2sym(63),mr2prob(63)

  scale1=255.999

  i1=1                                      !Silence warning
  i2=1

! Compute probabilities for most reliable symbol values
  do j=1,63
     s1=0.0
     psum=0.   ! used for sfrsd metrics
     do i=1,64
        psum=psum+s3(i,j)
        if(s3(i,j).gt.s1) then
           s1=s3(i,j)
           i1=i                              !Most reliable
        endif
     enddo
     if(psum.eq.0.0) psum=1.e-6
     s2=0.0
     do i=1,64
        if(i.ne.i1 .and. s3(i,j).gt.s2) then
           s2=s3(i,j)
           i2=i                              !Second most reliable
        endif
     enddo
     p1=s1/psum                !Use these for sfrsd
     p2=s2/psum                !...
     mrsym(j)=i1-1
     mr2sym(j)=i2-1
     mrprob(j)=scale1*p1
     mr2prob(j)=scale1*p2
  enddo

  return
end subroutine demod64a
