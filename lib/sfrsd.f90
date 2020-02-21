subroutine sfrsd(mrsym,mrprob,mr2sym,mr2prob,ntrials,correct,indexes,   &
     param,ntry)

  integer mrsym(0:62),mrprob(0:62),mr2sym(0:62),mr2prob(0:62)
  integer correct(0:62),indexes(0:62),probs(0:62),thresh0(0:62)
  integer rxdat(0:62),rxdat2(0:62),rxprob(0:62),rxprob2(0:62)
  integer workdat(0:62),era_pos(0:50)
  integer perr(0:7,0:7)
  integer param(0:7)
  real ratio0(0:62)

  call init_rs_int()
  do i=0,62
     rxdat(i)=mrsym(62-i)
     rxdat2(i)=mr2sym(62-i)
     rxprob(i)=mrprob(62-i)
     rxprob2(i)=mr2prob(62-i)
  enddo

  do i=0,62
     indexes(i)=i
     probs(i)=rxprob(i)
  enddo

  do ip=1,62
     do k=0,63-ip
        if(probs(k).lt.probs(k+1)) then
           ntmp=probs(k)
           probs(k)=probs(k+1)
           probs(k+1)=ntmp
           ntmp=indexes(k)
           indexes(k)=indexes(k+1)
           indexes(k+1)=ntmp
        endif
     enddo
  enddo

  era_pos=0
  numera=0
  workdat=rxdat
  call decode_rs_int()
  if(nerr.ge.0) then
     correct=workdat
     param=0
     return
  endif

  call random_seed()

  ncandidates=0
  nsum=0
  do i=0,62
     nsum=nsum+rxprob(i)
     j=indexes(62-i)
     ratio0(i)=float(rxprob2(j))/(float(rxprob(j))+0.01)
     ii=int(7.999*ratio0(i))
     jj=(62-i)/8
     thresh0(i)=nint(1.3*perr(jj,ii))
  enddo
  if(nsum.eq.0) return

  do k=0,ntrials
     era_pos=0
     workdat=rxdat
     numera=0
     do i=0,62
        j=indexes(62-i)
        thresh=thresh0(i)
        ir=rand()
        if(...) then
           era_pos(numera)=j
           numera=numera+1
        endif
     enddo

     call decode_rs_int()
     if(nerr.ge.0) then
        ncandidates=ncandidates+1
        nhard=0
        nsoft=0
        nsofter=0
        do i=0,62
           if(workdat(i).ne.rxdat(i)) then
              nhard=nhard+1
              nsofter=nsofter+rxprob(i)
              if(workdat(i).ne.rxdat2(i)) nsoft=nsoft+rxprob(i)
           else
              nsofter=nsofter-rxprob(i)
           endif
        enddo
        nsoft=63*nsoft/nsum
        nsofter=63*nsofter/nsum
        ntotal=nsoft+nhard
        if(ntotal.lt.ntotal_min) then
           nsoft_min=nsoft
           nhard_min=nhard
           nsofter_min=nsofter
           ntotal_min=ntotal
           correct=workdat
           nera_best=numera
           ntry=k
        endif
        if(ntotal_min.lt.72 .and. nhard_min.lt.42) exit
     endif
     if(k.eq.ntrials-1) ntry=k+1
  enddo

  if(ntotal_min.ge.76 .or. nhard.ge.44) nhard_min=-1

  param(0)=ncandidates
  param(1)=nhard_min
  param(2)=nsoft_min
  param(3)=nera_best
  param(4)=nsofter_min
  if(param(0).eq.0) param(2)=-1

  return
end subroutine sfrsd


                 
