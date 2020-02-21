program t1

  parameter (NSPM=1404)
  complex csig(0:NSPM-1)
  complex c(0:NSPM-1)
  complex cnoise(0:NSPM-1)
  complex cd(0:11,0:3)
  complex z,zmax
  integer itone(234)
  real r(234)
  real ss(0:3)
  character*12 arg

  nargs=iargc()
  if(nargs.ne.3) then
     print*,'Usage: t1 nsymtest snrdb iters'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) nsymtest
  call getarg(2,arg)
  read(arg,*) snrdb
  call getarg(3,arg)
  read(arg,*) iters

  call init_random_seed()       ! seed Fortran RANDOM_NUMBER generator
  call sgran()                  ! see C rand generator (used in gran)

  call random_number(r)
  itone=0
  where(r.gt.0.5) itone=1

  twopi=8.0*atan(1.0)
  fmid=1500.0
  f0=fmid-500.
  f1=fmid+500.
  dt=1.0/12000.0

  phi=0.
  do n=0,3
     k=-1
     dphi=twopi*f0*dt
     if(n.ge.2) dphi=twopi*f1*dt
     do i=0,5
        k=k+1
        phi=phi+dphi
        if(phi.gt.twopi) phi=phi-twopi
        cd(k,n)=cmplx(cos(phi),sin(phi))
     enddo

     dphi=twopi*f0*dt
     if(mod(n,2).eq.1) dphi=twopi*f1*dt
     do i=6,11
        k=k+1
        phi=phi+dphi
        if(phi.gt.twopi) phi=phi-twopi
        cd(k,n)=cmplx(cos(phi),sin(phi))
     enddo
  enddo

!  do k=0,11
!     write(13,1000) k,cd(k,0:3)
!1000 format(i4,8f9.3)
!enddo

! Generate signal waveform
  k=-1
  phi=0.
  do j=1,234
     dphi=twopi*f0*dt
     if(itone(j).eq.1) dphi=twopi*f1*dt
     do i=1,6
        k=k+1
        phi=phi+dphi
        if(phi.gt.twopi) phi=phi-twopi
        csig(k)=cmplx(cos(phi),sin(phi))
!        write(14,1000) k,csig(k)
     enddo
  enddo

     write(*,1010) 
1010 format('  S/N  (S+N)/N    BER'/'----------------------')

  isnra=10
  isnrb=-3
  nsyms=234
  do isnr=isnra,isnrb,-1
     snr=10.0**(0.1*isnr)
     if(snrdb.ne.0.0) snr=10.0**(0.1*snrdb)
     fac=1.0/sqrt(snr)
     nsumerr=0
     do iter=1,iters
        do i=0,NSPM-1
           x=gran()
           y=gran()
           cnoise(i)=cmplx(x,y)
        enddo

        c=csig + fac*cnoise
        nerr=0
        n1=2
        nstep=2
        iz=5
        if(nsymtest.eq.2) then
           n1=3
           nstep=1
           iz=11
        endif

        do j=1,nsyms
           smax=0.
           do n=0,n1,nstep
              s=0.
              k=6*(j-1)
              do i=0,iz
                 s=s + aimag(c(k+i))*aimag(cd(i,n))
              enddo
              ss(n)=s
              if(abs(s).gt.abs(smax)) then
                 smax=s
                 npk=n
              endif
           enddo
           sym=max(abs(ss(2)),abs(ss(3))) - max(abs(ss(0)),abs(ss(1)))
!           ibit=npk/2
           ibit=0
           if(sym.ge.0.0) ibit=1
           if(ibit.ne.itone(j)) nerr=nerr+1
        enddo
        nsumerr=nsumerr+nerr
     enddo

     write(*,1020) 10.0*log10(snr),10.0*log10(snr+1.0),            &
          float(nsumerr)/(nsyms*iters)
1020 format(f5.1,f7.1,f9.3)
     if(snrdb.ne.0.0) exit
  enddo

999 end program t1

