program stats

  character*8 arg
  character*40 infile
  character decoded*22

  nargs=iargc()
  if(nargs.lt.1) then
     print*,'Usage: stats file1 ...'
     go to 999
  endif

  ttol=0.1
  nftol=1
  write(*,1000)
1000 format(' SNR  Files  Sync    BM    FT  Hint Total  False BadSync'/  &
          56('-'))

  do ifile=1,nargs
     call getarg(ifile,infile)
     open(10,file=infile,status='old')
     i1=index(infile,".")+1
     i2=40
     if(index(infile,"_").gt.1) i2=index(infile,"_") - 1
     snrgen=0.
     read(infile(i1:i2),*,err=1) snrgen
1     snrgen=-snrgen
     nsynced=0
     nbm=0
     nftok=0
     nhint=0
     ngood=0
     nbad=0
     nbadsync=0

     do iline=1,999999
        read(10,1010,end=100) nutc,sync,nsnr,dt,nfreq,ncandidates,nhard,  &
             ntotal,rtt,ntry,nft,nqual,decoded
1010    format(i4.4,f5.1,i4,f5.1,i5,i6,i3,i4,f6.3,i8,i2,i3,1x,a22)

        ndfreq=abs(nfreq-1500)
        if(sync.ge.1.0 .and. abs(dt).le.ttol .and. ndfreq.le.nftol) then
           nsynced=nsynced+1
        else
           nbadsync=nbadsync+1
        endif

        if(decoded.eq.'                      ') cycle
        if(nft.eq.2 .or. (ntotal.le.81 .and. rtt.le.0.87)) then       !nag=0
           if(decoded(1:11).eq.'K1ABC W9XYZ') then
              ngood=ngood+1
              if(nft.eq.1 .and. ncandidates.eq.0) nbm=nbm+1
              if(nft.eq.1) nftok=nftok+1
              if(nft.ge.2) nhint=nhint+1
           else
              nbad=nbad+1
           endif
        endif
     enddo

100  write(*,1100) snrgen,nutc,nsynced,nbm,nftok,nhint,ngood,nbad,   &
          nbadsync
1100 format(f5.1,8i6)
  enddo

999 end program stats
