subroutine filbig(f0,newfft,c4a,n4,sq0,c6a,n2)

! Filter and downsample the real data in array dd(npts), sampled at 12000 Hz.
! Output is complex, sampled at 1378.125 Hz.

  use, intrinsic :: iso_c_binding
  use FFTW3
  use timer_module, only: timer
  use jt65_mod5 !ref(3413)
  use jt65_mod6 !NPTS=624000 dd(NPTS)

  parameter (NFFT1=672000,NFFT2=77175,NFFT3=9646,NSZ=3413)
  parameter (NZ2=1000)
  
  real*4 rca(NFFT1)
  complex ca(NFFT1/2+1)                      !FFT of input
  complex c4a(NFFT2),c6a(NFFT3)              !Output data
  real*4 s(NZ2)
  integer i,j
  real halfpulse(15),halfpulse2(15)  !Impulse response of filter (one sided)
  complex cfilt(NFFT2),cfilt2(NFFT2)    !Filter (complex; imag = 0)
  real rfilt(NFFT2),rfilt2(NFFT2)     !Filter (real)
  type(C_PTR) :: plan1,plan2,plan3,plan4,plan5    !Pointers to FFTW plans
  logical first
  logical(1) newfft
  equivalence (rfilt,cfilt),(rfilt2,cfilt2),(rca,ca)
  data first/.true./

! Kaiser-Bessel FIR LPF  http://www.arc.id.au/FilterDesign.html
! fsample    (Hz)  12000      Input sample rate
! Ntaps            29          Number of filter taps
! fc         (Hz)  4353       Cutoff frequency
! dF/2       (Hz)  328        Half transition band
! Stop Atten (dB)  30        Stopband attenuation
! fout       (Hz)  1378.125   Output sample rate
  data halfpulse/0.725500000, 0.240786311, -0.154852552, 0.053941884, 0.022651887, -0.053126870, &
  0.041168005, -0.009120071, -0.017722128, 0.025226891, -0.015037961, -0.001056840, 0.011283798, &
 -0.011212205, 0.004348607/

! Kaiser-Bessel FIR LPF  http://www.arc.id.au/FilterDesign.html
! fsample    (Hz)  1378.125   Input sample rate
! Ntaps            29          Number of filter taps
! fc         (Hz)  86         Cutoff frequency
! dF/2       (Hz)  38        Half transition band
! Stop Atten (dB)  30        Stopband attenuation
! fout       (Hz)  172.265625   Output sample rate
   data halfpulse2/0.124952381, 0.121297716, 0.110774440, 0.094635363, 0.074749989, 0.053318415, &
  0.032545585, 0.014332629, 0.000036722, -0.009663997, -0.014785038, -0.015937306, -0.014143718, &
 -0.010611825, -0.006507996/
  common/patience/npatience,nthreads
  save first,plan1,plan2,plan3,plan4,plan5,rfilt,cfilt,rfilt2,cfilt2,ca

  if(f0.lt.0.) go to 900                    !Clean up at end of program

  nthreadsmin=nthreads !single JT65 mode
  if(nnmode.eq.74) then
    nthreadsmin=max(nthreads-1,1)
    if(nthreads.ge.8 .and. nthreads.lt.12) nthreadsmin=nthreads-2
    if(nthreads.ge.12) nthreadsmin=nthreads-3
  endif
  
  if(first) then
     nflags=FFTW_ESTIMATE
     if(npatience.eq.1) nflags=FFTW_ESTIMATE_PATIENT
     if(npatience.eq.2) nflags=FFTW_MEASURE
     if(npatience.eq.3) nflags=FFTW_PATIENT
     if(npatience.eq.4) nflags=FFTW_EXHAUSTIVE

! Plan the FFTs just once
     !$omp critical(fftw) ! serialize non thread-safe FFTW3 calls
     call fftwf_plan_with_nthreads(nthreadsmin)
     plan1=fftwf_plan_dft_r2c_1d(nfft1,rca,ca,nflags)
     plan2=fftwf_plan_dft_1d(nfft2,c4a,c4a,-1,nflags)
     plan3=fftwf_plan_dft_1d(nfft2,cfilt,cfilt,+1,nflags)
     call fftwf_plan_with_nthreads(1)
     plan4=fftwf_plan_dft_1d(nfft3,cfilt2,cfilt2,+1,nflags)
     plan5=fftwf_plan_dft_1d(nfft3,c6a,c6a,-1,nflags)
     !$omp end critical(fftw)

! Convert impulse response to filter function
     cfilt=0.
     cfilt2=0.
     fac=0.00625/nfft1
     fac2=0.03

     cfilt(1)=fac*halfpulse(1)
     cfilt2(1)=fac2*halfpulse2(1)
     do i=2,15
        cfilt(i)=fac*halfpulse(i)
        cfilt(nfft2+2-i)=fac*halfpulse(i)
     enddo
     do i=2,15
        cfilt2(i)=fac2*halfpulse2(i)
        cfilt2(nfft3+2-i)=fac2*halfpulse2(i)
     enddo

     call fftwf_execute_dft(plan3,cfilt,cfilt)
     call fftwf_execute_dft(plan4,cfilt2,cfilt2)

     base=real(cfilt(38588)) ! nfft2/2+1
     base2=real(cfilt2(nfft3/2+1))
     do i=1,nfft2
        rfilt(i)=real(cfilt(i))-base
     enddo
     do i=1,nfft3
        rfilt2(i)=real(cfilt2(i))-base2
     enddo
     first=.false.
  endif

! When new data comes along, we need to compute a new "big FFT"
! If we just have a new f0, continue with the existing data in ca.

  if(newfft) then
!     call timer('FFTbig  ',0)
     rca(1:npts)=dd(1:npts)
     rca(npts+1:)=0.

     call fftwf_execute_dft_r2c(plan1,rca,ca)
!     call timer('FFTbig  ',1)

     ib=0
     do j=1,NSZ
        ia=ib+1
        ib=nint(float(j)*(672000./8192)) ! NFFT1/NFFT
        if(ref(j).le.0.0) ref(j)=0.0001
        fac=sqrt(1.0/ref(j))
        ca(ia:ib)=fac*conjg(ca(ia:ib))
     enddo
  endif

! NB: f0 is the frequency at which we want our filter centered.
!     i0 is the bin number in ca closest to f0.
!  call timer('loops   ',0)
  i0=nint(f0*56.) + 1 ! f0/df where delta f = 12000/672000 = 1/56
  nh=38587 ! nfft2/2

  do i=1,nh                                !Copy data into c4a and apply
     j=i0+i-1                              !the filter function
     if(j.ge.1 .and. j.le.nfft1/2+1) then
        c4a(i)=rfilt(i)*ca(j)
     else
        c4a(i)=0.
     endif
  enddo
  do i=nh+1,nfft2
     j=i0+i-1-nfft2
     if(j.ge.1) then
        c4a(i)=rfilt(i)*ca(j)
     else
        c4a(i)=rfilt(i)*conjg(ca(2-j))
     endif
  enddo
  
  nh=nfft3/2
  do i=1,nh
     c6a(i)=rfilt2(i)*c4a(i)
  enddo
  do i=nh+1,nfft3
     c6a(i)=rfilt2(i)*c4a(nfft2-nfft3+i)
  enddo

  nadd=nfft2/(3.0*NZ2) ! 180Hz BW

  i=0
  do j=1,NZ2
     s(j)=0.
     do n=1,nadd
        i=i+1
        s(j)=s(j) + real(c4a(i))**2 + aimag(c4a(i))**2
     enddo
  enddo
  call pctile(s,NZ2,10,sq0)
  sq0=sq0*1.732 ! SQRT(3.0)
!  call timer('loops   ',1)

! Do the short reverse transform, to go back to time domain.
!  call timer('FFTsmall',0)
  call fftwf_execute_dft(plan2,c4a,c4a)
  call fftwf_execute_dft(plan5,c6a,c6a)
!  call timer('FFTsmall',1)
  n4=min(npts/8,nfft2)
  n2=nfft3

  return

900 continue

  !$omp critical(fftw) ! serialize non thread-safe FFTW3 calls
  call fftwf_destroy_plan(plan1)
  call fftwf_destroy_plan(plan2)
  call fftwf_destroy_plan(plan3)
  call fftwf_destroy_plan(plan4)
  call fftwf_destroy_plan(plan5)
  !$omp end critical(fftw)
  
  return
end subroutine filbig
