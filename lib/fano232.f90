subroutine fano232(symbol,nbits,mettab,ndelta,maxcycles,dat,     &
     ncycles,metric,ierr)

! Sequential decoder for K=32, r=1/2 convolutional code using 
! the Fano algorithm.  Translated from C routine for same purpose
! written by Phil Karn, KA9Q.

  parameter (MAXBITS=103)
  parameter (MAXBYTES=(MAXBITS+7)/8)
  integer*1 symbol(0:2*MAXBITS-1)  !Soft symbols (as unsigned i*1)
  integer*1 dat(MAXBYTES)          !Decoded user data, 8 bits per byte
  integer mettab(-128:127,0:1)     !Metric table

! These were the "node" structure in Karn's C code:
  integer nstate(0:MAXBITS)        !Encoder state of next node
  integer gamma(0:MAXBITS)         !Cumulative metric to this node
  integer metrics(0:3,0:MAXBITS)   !Metrics indexed by all possible Tx syms
  integer tm(0:1,0:MAXBITS)        !Sorted metrics for current hypotheses
  integer ii(0:MAXBITS)            !Current branch being tested

  logical noback
  include 'conv232.f90'            !Polynomials defined here

  ntail=nbits-31

! Compute all possible branch metrics for each symbol pair.
! This is the only place we actually look at the raw input symbols
  i4a=0
  i4b=0
  do np=0,nbits-1
     j=2*np
     i4a=symbol(j)
     i4b=symbol(j+1)
     metrics(0,np) = mettab(i4a,0) + mettab(i4b,0)
     metrics(1,np) = mettab(i4a,0) + mettab(i4b,1)
     metrics(2,np) = mettab(i4a,1) + mettab(i4b,0)
     metrics(3,np) = mettab(i4a,1) + mettab(i4b,1)
  enddo

  np=0
  nstate(np)=0

  n=iand(nstate(np),npoly1)                  !Compute and sort branch metrics 
  n=ieor(n,ishft(n,-16))                     !from the root node
  lsym=partab(iand(ieor(n,ishft(n,-8)),255))
  n=iand(nstate(np),npoly2)
  n=ieor(n,ishft(n,-16))
  lsym=lsym+lsym+partab(iand(ieor(n,ishft(n,-8)),255))
  m0=metrics(lsym,np)
  m1=metrics(ieor(3,lsym),np)
  if(m0.gt.m1) then
     tm(0,np)=m0                             !0-branch has better metric
     tm(1,np)=m1
  else
     tm(0,np)=m1                             !1-branch is better
     tm(1,np)=m0
     nstate(np)=nstate(np) + 1               !Set low bit
  endif

  ii(np)=0                                   !Start with best branch
  gamma(np)=0
  nt=0

  do i=1,nbits*maxcycles                     !Start the Fano decoder
     ngamma=gamma(np) + tm(ii(np),np)        !Look forward
     if(ngamma.ge.nt) then
! Node is acceptable.  If first time visiting this node, tighten threshold:
        if(gamma(np).lt.(nt+ndelta)) nt=nt + ndelta * ((ngamma-nt)/ndelta)
        gamma(np+1)=ngamma                   !Move forward
        nstate(np+1)=ishft(nstate(np),1)
        np=np+1
        if(np.eq.nbits) go to 100          !We're done!

        n=iand(nstate(np),npoly1)
        n=ieor(n,ishft(n,-16))
        lsym=partab(iand(ieor(n,ishft(n,-8)),255))
        n=iand(nstate(np),npoly2)
        n=ieor(n,ishft(n,-16))
        lsym=lsym+lsym+partab(iand(ieor(n,ishft(n,-8)),255))
            
        if(np.ge.ntail) then
           tm(0,np)=metrics(lsym,np)      !We're in the tail, now all zeros
        else
           m0=metrics(lsym,np)
           m1=metrics(ieor(3,lsym),np)
           if(m0.gt.m1) then
              tm(0,np)=m0                 !0-branch has better metric
              tm(1,np)=m1
           else
              tm(0,np)=m1                 !1-branch is better
              tm(1,np)=m0
              nstate(np)=nstate(np) + 1   !Set low bit
           endif
        endif
        ii(np)=0                          !Start with best branch
     else
        do while(.true.)
           noback=.false.                 !Threshold violated, can't go forward
           if(np.eq.0) noback=.true.
           if(np.gt.0) then
              if(gamma(np-1).lt.nt) noback=.true.
           endif

           if(noback) then               !Can't back up, either
              nt=nt-ndelta               !Relax threshold and look forward again
              if(ii(np).ne.0) then
                 ii(np)=0
                 nstate(np)=ieor(nstate(np),1)
              endif
              exit
           endif

           np=np-1                       !Back up
           if(np.lt.ntail .and. ii(np).ne.1) then
              ii(np)=ii(np)+1            !Search the next best branch
              nstate(np)=ieor(nstate(np),1)
              exit
           endif
        enddo
     endif
  enddo
  i=nbits*maxcycles
  
100 metric=gamma(np)                       !Final path metric
  nbytes=(nbits+7)/8                       !Copy decoded data to user's buffer
  np=7
  do j=1,nbytes-1
     i4a=nstate(np)
     dat(j)=i4a
     np=np+8
  enddo
  dat(nbytes)=0
  ncycles=i+1
  ierr=0
  if(i.ge.maxcycles*nbits) ierr=-1

  return
end subroutine fano232
