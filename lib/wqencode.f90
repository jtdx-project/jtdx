subroutine wqencode(msg,ntype,data0)

!  Parse and encode a WSPR message.

  use packjt
  parameter (MASK15=32767)
  character*22 msg
  character*12 call1,call2
  character grid4*4,grid6*6
  logical lbad1,lbad2
  integer*1 data0(11)
  integer nu(0:9)
  data nu/0,-1,1,0,-1,2,1,0,-1,1/

! Standard WSPR message (types 0 3 7 10 13 17 ... 60)
  i1=index(msg,' ')
  i2=index(msg,'/')
  i3=index(msg,'<')
  call1=msg(:i1-1)
  if(i1.lt.3 .or. i1.gt.7 .or. i2.gt.0 .or. i3.gt.0) go to 10
  grid4=msg(i1+1:i1+4)
  call packcall(call1,n1,lbad1)
  call packgrid(grid4,ng,lbad2)
  if(lbad1 .or. lbad2) go to 10
  ndbm=0
  read(msg(i1+5:),*) ndbm
  if(ndbm.lt.0) ndbm=0
  if(ndbm.gt.60) ndbm=60
  ndbm=ndbm+nu(mod(ndbm,10))
  n2=128*ng + (ndbm+64)
  call pack50(n1,n2,data0)
  ntype=ndbm
  go to 900

10 if(i2.ge.2 .and. i3.lt.1) then
     call packpfx(call1,n1,ng,nadd)
     ndbm=0
     read(msg(i1+1:),*) ndbm
     if(ndbm.lt.0) ndbm=0
     if(ndbm.gt.60) ndbm=60
     ndbm=ndbm+nu(mod(ndbm,10))
     ntype=ndbm + 1 + nadd
     n2=128*ng + ntype + 64
     call pack50(n1,n2,data0)
  else if(i3.eq.1) then
     i4=index(msg,'>')
     call1=msg(2:i4-1)
     call hash(call1,i4-2,ih)
     grid6=msg(i1+1:i1+6)
     call2=grid6(2:6)//grid6(1:1)//'      '
     call packcall(call2,n1,lbad1)
     ndbm=0
     read(msg(i1+8:),*) ndbm
     if(ndbm.lt.0) ndbm=0
     if(ndbm.gt.60) ndbm=60
     ndbm=ndbm+nu(mod(ndbm,10))
     ntype=-(ndbm+1)
     n2=128*ih + ntype + 64
     call pack50(n1,n2,data0)
  endif
  go to 900

900 continue
  return
end subroutine wqencode
