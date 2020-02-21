subroutine flat2(s,nz,ref)

  parameter (NSMAX=6827)
  real s(NSMAX)
  real ref(NSMAX)

  nsmo=10
  ia=nsmo+1
  ib=nz-nsmo-1
  do i=ia,ib
     call pctile(s(i-nsmo),2*nsmo+1,5,ref(i))
  enddo

  ref(:ia-1)=ref(ia)
  ref(ib+1:)=ref(ib)

  return
end subroutine flat2
