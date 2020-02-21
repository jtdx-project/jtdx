! This source code file was last time modified by Igor UA3DJY on January 24th, 2018
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine downsam9(npts8,newdat,fpk,c2,nmode)

!Downsample from dd9() into c2() so as to yield nspsd samples per symbol, 
!mixing from fpk down to zero frequency.  The downsample factor is 432.

  use, intrinsic :: iso_c_binding
  use FFTW3
  use timer_module, only: timer
  use jt9_mod1

  include 'constants.f90'
  integer(C_SIZE_T) NMAX1
  parameter (NMAX1=653184)
  parameter (NFFT1=653184,NFFT2=1512)
  type(C_PTR) :: plan                        !Pointers plan for big FFT
  logical, intent(inout) :: newdat
  real*4, pointer :: x1(:)
  complex c1(0:NFFT1/2)
  complex c2(0:NFFT2-1)
  real s(5000)
  logical first
  data first/.true./
  common/patience/npatience,nthreads
  save plan,first,c1,s,x1

  nthreadsmin=1
  if(nmode.eq.74) then
    if(nthreads.ge.8 .and. nthreads.lt.12) nthreadsmin=2
    if(nthreads.ge.12) nthreadsmin=3
  endif

  df1=12000.0/NFFT1
  npts=8*npts8  ! 597888 and 618624 for dec on 52nd second
  if(npts.gt.NFFT1) npts=NFFT1  !### Fix! ###

  if(first) then
     nflags=FFTW_ESTIMATE
     if(npatience.eq.1) nflags=FFTW_ESTIMATE_PATIENT
     if(npatience.eq.2) nflags=FFTW_MEASURE
     if(npatience.eq.3) nflags=FFTW_PATIENT
     if(npatience.eq.4) nflags=FFTW_EXHAUSTIVE
! Plan the FFTs just once

     !$omp critical(fftw) ! serialize non thread-safe FFTW3 calls
     plan=fftwf_alloc_real(NMAX1)
     call c_f_pointer(plan,x1,[NMAX1])
     x1(0:NMAX1-1) => x1        !remap bounds
     call fftwf_plan_with_nthreads(nthreadsmin)
     plan=fftwf_plan_dft_r2c_1d(NFFT1,x1,c1,nflags)
     !$omp end critical(fftw)

     first=.false.
  endif

  if(newdat) then
     x1(0:npts-1)=dd9(0:npts-1)
     x1(npts:NFFT1-1)=0.                      !Zero the rest of x1
     call timer('FFTbig9 ',0)
     call fftwf_execute_dft_r2c(plan,x1,c1)
     call timer('FFTbig9 ',1)

     nadd=int(1.0/df1)
     s=0.
     do i=1,5000
        j=int((i-1)/df1)
        do n=1,nadd
           j=j+1
           s(i)=s(i)+real(c1(j))**2 + aimag(c1(j))**2
        enddo
     enddo
     newdat=.false.
  endif

  nh2=NFFT2/2
  nf=nint(fpk)
  i0=int(fpk/df1)

  nw=100
  ia=max(1,nf-nw)
  ib=min(5000,nf+nw)
  call pctile(s(ia),ib-ia+1,40,avenoise)

  fac=sqrt(1.0/avenoise)
  do i=0,NFFT2-1
     j=i0+i
     if(i.gt.nh2) j=j-NFFT2
     if(j.lt.0) j=0 ! protection of getting to negative index values
     c2(i)=fac*c1(j)
  enddo
  call four2a(c2,NFFT2,1,1,1)              !FFT back to time domain

  return
end subroutine downsam9
