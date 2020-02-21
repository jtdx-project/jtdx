subroutine mixlpf(x1,nbfo,c0)

  real*4 x1(512)
  real*8 twopi,phi,dphi
  complex c1(512),c2(105+512)
  complex c0(64)
  data phi/0.d0/
  save phi,c2

  twopi=8.d0*atan(1.d0)
  dphi=twopi*nbfo/12000.d0
  
  do i=1,512
     phi=phi+dphi
     if(phi.gt.twopi) phi=phi-twopi
     xphi=phi
     c1(i)=x1(i)*cmplx(cos(xphi),sin(xphi))
  enddo
  c2(106:105+512)=c1
  
  call fil3c(c2,105+512,c0,n2)
  c2(1:105)=c1(512-104:512)                       !Save 105 trailing samples

  return
end subroutine mixlpf
