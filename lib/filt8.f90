subroutine filt8(f0,nslots,width,wave)

  parameter (NFFT=180000,NH=NFFT/2)
  real wave(NFFT)
  real x(NFFT)
  real s1(0:NH)
  real s2(0:NH)
  complex cx(0:NH)
  equivalence (x,cx)

  x=wave
  call four2a(cx,NFFT,1,-1,0)              !r2c
  df=12000.0/NFFT
  fa=f0 - 0.5*6.25
  fb=f0 + 7.5*6.25 + (nslots-1)*60.0
  ia2=nint(fa/df)
  ib1=nint(fb/df)
  ia1=nint(ia2-width/df)
  ib2=nint(ib1+width/df)
  pi=4.0*atan(1.0)
  do i=ia1,ia2
     fil=(1.0 + cos(pi*df*(i-ia2)/width))/2.0
     cx(i)=fil*cx(i)
  enddo
  do i=ib1,ib2
     fil=(1.0 + cos(pi*df*(i-ib1)/width))/2.0
     cx(i)=fil*cx(i)
  enddo
  cx(0:ia1-1)=0.
  cx(ib2+1:)=0.

  call four2a(cx,nfft,1,1,-1)                  !c2r
  wave=x/nfft

!###
  if(nslots.ne.99) return
  x=wave
  call four2a(cx,NFFT,1,-1,0)              !r2c
  do i=0,NH
     s1(i)=real(cx(i))**2 + aimag(cx(i))**2
  enddo
  nadd=20
  call smo(s1,NH+1,s2,nadd)
  do i=0,NH
     freq=i*df
     write(29,3101) freq,db(s2(i)) - 72.0
3101 format(2f12.3)
  enddo
!###

  return
end subroutine filt8
