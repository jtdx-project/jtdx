program t6

  parameter (MAXFFT=1404)
  complex c(0:MAXFFT-1)
  real s(0:MAXFFT-1)

  m1=45
  m2=67
  m3=89
  nsym=3*11 + m1 + m2 + m3
  nfft=6*nsym
  nh=nfft/2

  best=9999.
!  do m1=22,67
!     do m2=37,97
  do m1=30,67
     do m2=26,100
        m3=201-m2-m1
        if(m3.lt.13) cycle
        c=0.
        n1=6*(11+m1)
        n2=n1+6*(11+m2)
        c(1:66)=1.
        c(1+n1:66+n1)=1.
        c(1+n2:66+n2)=1.
  
        call four2a(c,nfft,1,-1,1)            !c2c FFT

        df=12000.0/nfft
        smax=0.
        do i=0,nfft-1
           s(i)=real(c(i))**2 + aimag(c(i))**2
           if(i.ne.0) smax=max(s(i),smax)
        enddo
        sidelobe=db(smax/s(0))
        
        if(sidelobe.lt.best) then
           write(*,1000) m1,m2,m3,sidelobe
1000       format(3i5,f8.2)
           best=sidelobe
           s=s/s(0)
           rewind 13
           do j=0,nfft-1
              i=mod(j+nh,nfft)
              f=i*df
              if(i.gt.nh) f=f-12000.0
              write(13,1020) f,s(i)
1020          format(2f12.4)
           enddo
        endif
     enddo
  enddo
  
end program t6

