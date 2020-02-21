subroutine pltanh(x,y)
  isign=+1
  z=x
  if( x.lt.0 ) then
    isign=-1
    z=abs(x)
  endif
  if( z.le. 0.8 ) then
    y=0.83*x
    return
  elseif( z.le. 1.6 ) then
    y=isign*(0.322*z+0.4064)
    return  
  elseif( z.le. 3.0 ) then
    y=isign*(0.0524*z+0.8378)
    return
  elseif( z.lt. 7.0 ) then
    y=isign*(0.0012*z+0.9914)
    return
  else
    y=isign*0.9998
    return
  endif
end subroutine pltanh

subroutine platanh(x,y)
  isign=+1
  z=x
  if( x.lt.0 ) then
    isign=-1
    z=abs(x)
  endif
  if( z.le. 0.664 ) then
    y=x/0.83
    return
  elseif( z.le. 0.9217 ) then
    y=isign*(z-0.4064)/0.322
    return
  elseif( z.le. 0.9951 ) then
    y=isign*(z-0.8378)/0.0524
    return
  elseif( z.le. 0.9998 ) then
    y=isign*(z-0.9914)/0.0012
    return
  else
    y=isign*7.0
    return
  endif
end subroutine platanh

subroutine bpdecode144(llr,maxiterations,decoded,niterations)
!
! A log-domain belief propagation decoder for the msk144 code.
! The code is a regular (128,80) code with column weight 3 and row weight 8. 
! k9an August, 2016
!
integer, parameter:: N=128, K=80, M=N-K
integer*1 codeword(N),cw(N)
integer*1 colorder(N)
integer*1 decoded(K)
integer Nm(8,M)  ! 8 bits per check 
integer Mn(3,N)  ! 3 checks per bit
integer synd(M)
real tov(3,N)    ! single precision seems to be adequate in log-domain
real toc(8,M)
real tanhtoc(8,M)
real zn(N)
real llr(N)
real Tmn

data colorder/0,1,2,3,4,5,6,7,8,9, &
              10,11,12,13,14,15,24,26,29,30, &
              32,43,44,47,60,77,79,97,101,111, &
              96,38,64,53,93,34,59,94,74,90, &
              108,123,85,57,70,25,69,62,48,49, &
              50,51,52,33,54,55,56,21,58,36, &
              16,61,23,63,20,65,66,67,68,46, &
              22,71,72,73,31,75,76,45,78,17, &
              80,81,82,83,84,42,86,87,88,89, &
              39,91,92,35,37,95,19,27,98,99, &
              100,28,102,103,104,105,106,107,40,109, &
              110,18,112,113,114,115,116,117,118,119, &
              120,121,122,41,124,125,126,127/

data Mn/               &
   1,  14,  38, &
   2,   4,  41, &
   3,  19,  39, &
   5,  29,  34, &
   6,  35,  40, &
   7,  20,  45, &
   8,  28,  48, &
   9,  22,  25, &
  10,  24,  36, &
  11,  12,  37, &
  13,  43,  44, &
  15,  18,  46, &
  16,  17,  47, &
  21,  32,  33, &
  23,  30,  31, &
  26,  27,  42, &
   1,  12,  46, &
   2,  36,  38, &
   3,   5,  10, &
   4,   9,  23, &
   6,  13,  39, &
   7,  15,  17, &
   8,  18,  27, &
  11,  33,  40, &
  14,  28,  44, &
  16,  29,  31, &
  19,  20,  22, &
  21,  30,  42, &
  24,  26,  47, &
  25,  37,  48, &
  32,  34,  45, &
   8,  35,  41, &
  12,  31,  43, &
   1,  19,  21, &
   2,  43,  45, &
   3,   4,  11, &
   5,  18,  33, &
   6,  25,  47, &
   7,  28,  30, &
   9,  14,  34, &
  10,  35,  42, &
  13,  15,  22, &
  16,  37,  38, &
  17,  41,  44, &
  20,  24,  29, &
  18,  23,  39, &
  12,  26,  32, &
  27,  38,  40, &
  15,  36,  48, &
   2,  30,  46, &
   1,   4,  13, &
   3,  28,  32, &
   5,  43,  47, &
   6,  34,  46, &
   7,   9,  40, &
   8,  11,  45, &
  10,  17,  23, &
  14,  31,  35, &
  16,  22,  42, &
  19,  37,  44, &
  20,  33,  48, &
  21,  24,  41, &
  25,  27,  29, &
  26,  39,  48, &
  19,  31,  36, &
   1,   5,   7, &
   2,  29,  39, &
   3,  16,  46, &
   4,  26,  37, &
   6,  28,  45, &
   8,  22,  33, &
   9,  21,  43, &
  10,  25,  38, &
  11,  14,  24, &
  12,  17,  40, &
  13,  27,  30, &
  15,  32,  35, &
  18,  44,  47, &
  20,  23,  36, &
  34,  41,  42, &
   1,  32,  48, &
   2,   3,  33, &
   4,  29,  42, &
   5,  14,  37, &
   6,   7,  36, &
   8,   9,  39, &
  10,  13,  19, &
  11,  18,  30, &
  12,  16,  20, &
  15,  29,  44, &
  17,  34,  38, &
   6,  21,  22, &
  23,  32,  40, &
  24,  27,  46, &
  25,  41,  45, &
   7,  26,  43, &
  28,  31,  47, &
  20,  35,  38, &
   1,  33,  41, &
   2,  42,  44, &
   3,  23,  48, &
   4,  31,  45, &
   5,   8,  30, &
   9,  16,  36, &
  10,  40,  47, &
  11,  17,  46, &
  12,  21,  34, &
  13,  24,  28, &
  14,  18,  43, &
  15,  25,  26, &
  19,  27,  35, &
  22,  37,  39, &
   1,  16,  18, &
   2,   6,  20, &
   3,  30,  43, &
   4,  28,  33, &
   5,  22,  23, &
   7,  39,  42, &
   8,  12,  38, &
   9,  35,  46, &
  10,  27,  32, &
  11,  15,  34, &
  13,  36,  37, &
  14,  41,  47, &
  17,  21,  25, &
  19,  29,  45, &
  24,  31,  48, &
  26,  40,  44/

data Nm/               &
   1,  17,  34,  51,  66,  81,  99, 113, &
   2,  18,  35,  50,  67,  82, 100, 114, &
   3,  19,  36,  52,  68,  82, 101, 115, &
   2,  20,  36,  51,  69,  83, 102, 116, &
   4,  19,  37,  53,  66,  84, 103, 117, &
   5,  21,  38,  54,  70,  85,  92, 114, &
   6,  22,  39,  55,  66,  85,  96, 118, &
   7,  23,  32,  56,  71,  86, 103, 119, &
   8,  20,  40,  55,  72,  86, 104, 120, &
   9,  19,  41,  57,  73,  87, 105, 121, &
  10,  24,  36,  56,  74,  88, 106, 122, &
  10,  17,  33,  47,  75,  89, 107, 119, &
  11,  21,  42,  51,  76,  87, 108, 123, &
   1,  25,  40,  58,  74,  84, 109, 124, &
  12,  22,  42,  49,  77,  90, 110, 122, &
  13,  26,  43,  59,  68,  89, 104, 113, &
  13,  22,  44,  57,  75,  91, 106, 125, &
  12,  23,  37,  46,  78,  88, 109, 113, &
   3,  27,  34,  60,  65,  87, 111, 126, &
   6,  27,  45,  61,  79,  89,  98, 114, &
  14,  28,  34,  62,  72,  92, 107, 125, &
   8,  27,  42,  59,  71,  92, 112, 117, &
  15,  20,  46,  57,  79,  93, 101, 117, &
   9,  29,  45,  62,  74,  94, 108, 127, &
   8,  30,  38,  63,  73,  95, 110, 125, &
  16,  29,  47,  64,  69,  96, 110, 128, &
  16,  23,  48,  63,  76,  94, 111, 121, &
   7,  25,  39,  52,  70,  97, 108, 116, &
   4,  26,  45,  63,  67,  83,  90, 126, &
  15,  28,  39,  50,  76,  88, 103, 115, &
  15,  26,  33,  58,  65,  97, 102, 127, &
  14,  31,  47,  52,  77,  81,  93, 121, &
  14,  24,  37,  61,  71,  82,  99, 116, &
   4,  31,  40,  54,  80,  91, 107, 122, &
   5,  32,  41,  58,  77,  98, 111, 120, &
   9,  18,  49,  65,  79,  85, 104, 123, &
  10,  30,  43,  60,  69,  84, 112, 123, &
   1,  18,  43,  48,  73,  91,  98, 119, &
   3,  21,  46,  64,  67,  86, 112, 118, &
   5,  24,  48,  55,  75,  93, 105, 128, &
   2,  32,  44,  62,  80,  95,  99, 124, &
  16,  28,  41,  59,  80,  83, 100, 118, &
  11,  33,  35,  53,  72,  96, 109, 115, &
  11,  25,  44,  60,  78,  90, 100, 128, &
   6,  31,  35,  56,  70,  95, 102, 126, &
  12,  17,  50,  54,  68,  94, 106, 120, &
  13,  29,  38,  53,  78,  97, 105, 124, &
   7,  30,  49,  61,  64,  81, 101, 127/

nrw=8; ncw=3; toc=0; tov=0; tanhtoc=0

! initial messages to checks
do j=1,M
  do i=1,nrw
    toc(i,j)=llr((Nm(i,j)))
  enddo
enddo

ncnt=0; nclast=0

do iter=0,maxiterations

! Update bit log likelihood ratios
  do i=1,N
    zn(i)=llr(i)+sum(tov(1:ncw,i))
  enddo

! Check to see if we have a codeword
  cw=0
  where( zn .gt. 0. ) cw=1
  ncheck=0
  do i=1,M
    synd(i)=sum(cw(Nm(:,i)))
    if( mod(synd(i),2) .ne. 0 ) ncheck=ncheck+1
  enddo

  if( ncheck .eq. 0 ) then ! we have a codeword
    niterations=iter
    codeword=cw(colorder+1)
    decoded=codeword(M+1:N)
    return
  endif

  if( iter.gt.0 ) then  ! this code block implements an early stopping criterion
    nd=ncheck-nclast
    if( nd .lt. 0 ) then ! # of unsatisfied parity checks decreased
      ncnt=0  ! reset counter
    else
      ncnt=ncnt+1
    endif
!    write(*,*) iter,ncheck,nd,ncnt
    if( ncnt .ge. 3 .and. iter .ge. 5 .and. ncheck .gt. 10) then
      niterations=-1
      return
    endif
  endif
  nclast=ncheck 
 
! Send messages from bits to check nodes 
  do j=1,M
    do i=1,nrw
      ibj=Nm(i,j)
      toc(i,j)=zn(ibj)  
      do kk=1,ncw ! subtract off what the bit had received from the check
        if( Mn(kk,ibj) .eq. j ) then  ! Mn(3,128)
          toc(i,j)=toc(i,j)-tov(kk,ibj)
        endif
      enddo
    enddo
  enddo

! send messages from check nodes to variable nodes
  do i=1,M
    tanhtoc(1:nrw,i)=tanh(-toc(1:nrw,i)/2)
  enddo

  do j=1,N
    do i=1,ncw
      ichk=Mn(i,j)  ! Mn(:,j) are the checks that include bit j
      Tmn=product(tanhtoc(:,ichk),mask=Nm(:,ichk).ne.j)
      call platanh(-Tmn,y)
      tov(i,j)=2*y
    enddo
  enddo

enddo
niterations=-1
end subroutine bpdecode144
