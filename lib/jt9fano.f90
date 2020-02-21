! This source code file was last time modified by Igor UA3DJY on 20181215
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine jt9fano(i1SoftSymbols,limit,msg,freemsg,i4Decoded6BitWords)

! Decoder for JT9
! Input:   i1SoftSymbols(207) - Single-bit soft symbols
! Output:  msg                - decoded message (blank if erasure)

  use packjt
  character*22 msg
  integer*4 i4DecodedBytes(9)
  integer*4 i4Decoded6BitWords(12)
  integer*1 i1DecodedBytes(13)   !72 bits and zero tail as 8-bit bytes
  integer*1 i1SoftSymbols(207)
  integer*1 i1DecodedBits(72)
  logical(1) freemsg
  real*4 xx0(0:255)

  logical first
  integer*4 mettab(-128:127,0:1)
  data first/.true./
  data xx0/                                                      & !Metric table
        1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  &
        1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  &
        1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  &
        1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  &
        1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  &
        1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  &
        0.988, 1.000, 0.991, 0.993, 1.000, 0.995, 1.000, 0.991,  &
        1.000, 0.991, 0.992, 0.991, 0.990, 0.990, 0.992, 0.996,  &
        0.990, 0.994, 0.993, 0.991, 0.992, 0.989, 0.991, 0.987,  &
        0.985, 0.989, 0.984, 0.983, 0.979, 0.977, 0.971, 0.975,  &
        0.974, 0.970, 0.970, 0.970, 0.967, 0.962, 0.960, 0.957,  &
        0.956, 0.953, 0.942, 0.946, 0.937, 0.933, 0.929, 0.920,  &
        0.917, 0.911, 0.903, 0.895, 0.884, 0.877, 0.869, 0.858,  &
        0.846, 0.834, 0.821, 0.806, 0.790, 0.775, 0.755, 0.737,  &
        0.713, 0.691, 0.667, 0.640, 0.612, 0.581, 0.548, 0.510,  &
        0.472, 0.425, 0.378, 0.328, 0.274, 0.212, 0.146, 0.075,  &
        0.000,-0.079,-0.163,-0.249,-0.338,-0.425,-0.514,-0.606,  &
       -0.706,-0.796,-0.895,-0.987,-1.084,-1.181,-1.280,-1.376,  &
       -1.473,-1.587,-1.678,-1.790,-1.882,-1.992,-2.096,-2.201,  &
       -2.301,-2.411,-2.531,-2.608,-2.690,-2.829,-2.939,-3.058,  &
       -3.164,-3.212,-3.377,-3.463,-3.550,-3.768,-3.677,-3.975,  &
       -4.062,-4.098,-4.186,-4.261,-4.472,-4.621,-4.623,-4.608,  &
       -4.822,-4.870,-4.652,-4.954,-5.108,-5.377,-5.544,-5.995,  &
       -5.632,-5.826,-6.304,-6.002,-6.559,-6.369,-6.658,-7.016,  &
       -6.184,-7.332,-6.534,-6.152,-6.113,-6.288,-6.426,-6.313,  &
       -9.966,-6.371,-9.966,-7.055,-9.966,-6.629,-6.313,-9.966,  &
       -5.858,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,  &
       -9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,  &
       -9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,  &
       -9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,  &
       -9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,  &
       -9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966,-9.966/
  save

  if(first) then
! Get the metric table
     bias=0.5
     scale=50
     ndelta=nint(3.4*scale)
     ib=160                          !Break point
     slope=2                         !Slope beyond break
     do i=0,255
        mettab(i-128,0)=nint(scale*(xx0(i)-bias))
        if(i.gt.ib) mettab(i-128,0)=mettab(ib-128,0) - slope*(i-ib)
        if(i.ge.1) mettab(128-i,1)=mettab(i-128,0)
     enddo
     mettab(-128,1)=mettab(-127,1)
     first=.false.
  endif

  msg='                      '
  nbits=72
  call fano232(i1SoftSymbols,nbits+31,mettab,ndelta,limit,i1DecodedBytes,   &
       ncycles,metric,ierr)

  nlim=ncycles/(nbits+31)
  if(ncycles.lt.((nbits+31)*limit)) then
     nbytes=(nbits+7)/8
     do i=1,nbytes
        n=i1DecodedBytes(i)
        i4DecodedBytes(i)=iand(n,255)
     enddo
     freemsg=.false.
     call unpackbits(i4DecodedBytes,nbytes,8,i1DecodedBits)
     call packbits(i1DecodedBits,12,6,i4Decoded6BitWords)
     if(iand(i4Decoded6BitWords(10),8).ne.0) freemsg=.true.
     call unpackmsg(i4Decoded6BitWords,msg)                !Unpack decoded msg
     if(index(msg,'000AAA ').gt.0) msg='                      '
  endif

  return
end subroutine jt9fano
