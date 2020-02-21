! This source code file was last time modified by Igor UA3DJY on May 30th, 2017
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine subtract9(i4Decoded6BitWords,freq,drift,xdt,i4GrayCodedSym,hintdec)

! Subtract a jt9 signal
!
! Measured signal  : dd(t)    = a(t)cos(2*pi*f0*t+theta(t))
! Reference signal : cref(t)  = exp( j*(2*pi*f0*t+phi(t)) )
! Complex amp      : cfilt(t) = LPF[ dd(t)*CONJG(cref(t)) ]
! Subtract         : dd(t)    = dd(t) - 2*REAL{cref*cfilt}

!  use timer_module, only: timer
  use packjt
  use jt9_mod1 ! real*4 dd9(NPTS9) NPTS9=597888
  parameter (NFILT=1600)
  complex cw(720000)
  complex, DIMENSION(:), ALLOCATABLE :: cref,camp,cfilt
  integer*4 i4Decoded6BitWords(13)        !72-bit message as 6-bit words
  integer*1 i1Msg8BitBytes(13)            !72 bits and zero tail as 8-bit bytes
  integer*1 i1EncodedBits(207)            !Encoded information-carrying bits
  integer*1 i1ScrambledBits(207)          !Encoded bits after interleaving
  integer*4 i4DataSymbols(69)             !Data symbols (values 0-7)
  integer*4 i4GrayCodedSymbols(69)        !Gray-coded symbols (values 0-7)
  integer*4 i4GrayCodedSym(69)            !Gray-coded symbols (values 0-7)
  integer*4 i4tone(85)                    !Tone #s, data and sync (values 0-8)
  integer k,nsym,ns,nref,id,ind
  real*4 window(-NFILT/2:NFILT/2)
  real freq,drift,xdt,dt,dphi,phi,twopi,omega,a2,x0,x1
  logical(1) first,hintdec
  data first/.true./
  include 'jt9sync.f90'
  save first,cw

  allocate(cref(720000), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"
  allocate(camp(720000), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"
  allocate(cfilt(720000), STAT = nAllocateStatus1)
  if(nAllocateStatus1.ne.0) STOP "Not enough memory"

  twopi=6.28318531; pi512=1608.49548; phi=0.0 ! pi512=pi*512
  nsym=85; nsym2=206; ns=6912; ind=1; isym=1; id=1; nref=587520 ! nref=nsym*ns

  dt=xdt+1.0 ! one second TX offset
  nstart=nint(dt*12000.0)+1

  if(hintdec) then
     i4GrayCodedSymbols=i4GrayCodedSym
     goto 2
  endif

  call entail(i4Decoded6BitWords,i1Msg8BitBytes)  !Add tail, make 8-bit bytes
  call encode232(i1Msg8BitBytes,nsym2,i1EncodedBits)   !Encode K=32, r=1/2
  i1EncodedBits(207)=0
  call interleave9(i1EncodedBits,1,i1ScrambledBits)    !Interleave bits
  i1ScrambledBits(207)=0
  call packbits(i1ScrambledBits,69,3,i4DataSymbols)    !Pk 3-bits into words
  call graycode(i4DataSymbols,69,1,i4GrayCodedSymbols) !Apply Gray code

!do i=200,207
!print *,i1EncodedBits(i),i1ScrambledBits(i)
!print *,i4DataSymbols(69)
!enddo

2 continue
! Insert sync symbols at ntone=0 and add 2 to the data-tone numbers.
  j=0
  do i=1,85
     if(isync(i).eq.1) then
        i4tone(i)=0
     else
        j=j+1
        i4tone(i)=i4GrayCodedSymbols(j)+1
     endif
  enddo

!  call timer('subtr_1 ',0)
  x0=653184.0/2 ! 653184 samples are used for freq drift calculation, centering it
  x1=dt*12000.0+1.0
  a2=drift/(-2.0)
  do k=1,nsym
    omega=(pi512*(freq+a2*(k*ns+x1-x0)/653184.0+1.73611111*i4tone(isym)))/256 ! =2*pi*(f0+...
    isym=isym+1
    dphi=omega/12000.0
    do i=1,ns
        cref(ind)=cexp(cmplx(0.0,phi))
        phi=mod(phi+dphi,twopi)
        id=nstart-1+ind
        if(id.gt.(npts9-1)) exit
        if(id.ge.1) camp(ind)=dd9(id)*conjg(cref(ind))
        ind=ind+1
    enddo
  enddo
!  call timer('subtr_1 ',1)

!  call timer('subtr_2 ',0)
! Smoothing filter: do the convolution by means of FFTs. Ignore end-around 
! cyclic effects for now.

  nfft=588000
  if(first) then
! Create and normalize the filter
     sum1=0.0
     do j=-nfilt/2,nfilt/2
        window(j)=cos(twopi*j/(2*nfilt))
        sum1=sum1+window(j)
     enddo
     cw=0.
     do l=-nfilt/2,nfilt/2
        j1=l+1
        if(j1.lt.1) j1=j1+nfft
        cw(j1)=window(l)/sum1
     enddo
     call four2a(cw,nfft,1,-1,1)
     first=.false.
  endif

  cfilt(1:nref)=camp(1:nref)
  cfilt(nref+1:nfft)=0.
  call four2a(cfilt,nfft,1,-1,1)
  fac=1.0/float(nfft)
  cfilt(1:nfft)=fac*cfilt(1:nfft)*cw(1:nfft)
  call four2a(cfilt,nfft,1,1,1)
!  call timer('subtr_2 ',1)

! Subtract the reconstructed signal
!  call timer('subtr_3 ',0)
  do m=1,nref
     j2=nstart+m-1
     if(j2.ge.1 .and. j2.le.(npts9-1)) dd9(j2)=dd9(j2)-2*REAL(cfilt(m)*cref(m))
  enddo
!  call timer('subtr_3 ',1)

  deallocate (cfilt, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'
  deallocate (camp, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'
  deallocate (cref, STAT = nDeAllocateStatus1)
  if (nDeAllocateStatus1.ne.0) print *, 'failed to release memory'

  return
end subroutine subtract9
