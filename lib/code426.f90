program code426

  parameter (MZ=26)                !Number of 4-FSK symbols
  parameter (JZMAX=64)             !Desired number of codewords
  integer ic(MZ,JZMAX),icsave(MZ)
  real c(MZ)
  character*12 arg

  nargs=iargc()
  if(nargs.ne.2) then
     print*,'Usage:   code426 <nmsgs> <iters>'
     print*,'Example: code426   64   10000000'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) nmsgs
  call getarg(2,arg)
  read(arg,*) iters

  call init_random_seed()

  open(13,file='code426.out',status='unknown')

  write(*,1002) nmsgs,iters
  write(13,1002) nmsgs,iters
1002 format('Nmsgs:',i4,'   Iters:',i10/(66('-')))
  
  do i=1,MZ                     !Create 4 mutually orthogonal codewords
     ic(i,1)=mod(i-1,4)
     ic(i,2)=mod(i,4)
     ic(i,3)=mod(i+1,4)
     ic(i,4)=mod(i+2,4)
  enddo

  do j=1,4                      !Write them out
     write(*,1000) j,MZ,ic(1:MZ,j)
     write(13,1000) j,MZ,ic(1:MZ,j)
1000 format(2i5,3x,26i2)
  enddo
 
  do j=5,nmsgs                  !Find codewords up to j=nmsgs with maximum
     npk=0                      !distance from all the rest
     do i=1,iters
        call random_number(c)   !Generate a random codeword candidate
        ic(1:MZ,j)=int(4*c)     !Convert real to integer
!        nd=MZ
!        do k=1,j-1              !Test candidate against all others in list
!           n=count(ic(1:MZ,j).ne.ic(1:MZ,k))
!           nd=min(n,nd)
!        enddo
        call dist426(ic,j,mind)
        if(mind.gt.npk) then
           npk=mind
           icsave=ic(1:MZ,j)    !Best candidate so far, save it
!           if(npk.ge.19) exit   !It won't get any better...
        endif
     enddo
     write(*,1000) j,npk,ic(1:MZ,j)
     write(13,1000) j,npk,ic(1:MZ,j)
  enddo

999 end program code426
