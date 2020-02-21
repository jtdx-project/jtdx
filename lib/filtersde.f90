! This source code file was last time modified by Igor Chernikov UA3DJY on December 14th, 2016.
! All changes are shown in the patch file coming together with the full JTDX source code.

subroutine filtersde(decoded,falsedec)

  integer, parameter :: EGRIDMAX=111
  character grid*4,call2*6,decoded*22
  logical(1) falsedec
    character*2 egrid(EGRIDMAX)&
  /'AA','AB','AC','AD','AF','AM','AN','AR','BA','BB','BC','BD','BE','BF','BM','BN','CA','CB','CC','CD', &
   'CE','CF','CL','CK','CJ','CR','DA','DB','DC','DD','DE','DF','DH','DI','DJ','DR','EA','EB','EC','ED', &
   'EE','EF','EG','EH','ER','FA','FB','GA','GB','GE','GL','GM','GR','HA','HB','HC','HE','HF','HG','HJ', &
   'HL','HN','HO','HR','IA','IB','IC','ID','IE','IG','JA','JB','JC','JE','JR','KA','KB','KC','KD','KR', &
   'LA','LB','LC','LD','LF','LQ','MA','MB','MG','MR','NA','NB','NC','ND','NE','NF','NG','PA','PC','PD', &
   'PE','QA','QB','QC','RA','RC','RL','RM','RN','RQ','RR'/

! check for false "DE " decodes

  i1=index(decoded,' ')
  i2=index(decoded(i1+1:),' ')
  if(i2.lt.4) go to 2
  i2=i2+i1

  call2=decoded(i1+1:i2-1)
  if(i2.gt.18) go to 2 ! avoid getting out of decoded*22
  grid=decoded(i2+1:i2+4)
  
  if(call2(1:1).eq."0" .or. call2(1:1).eq."Q") go to 2
  
  if(call2(1:1).ge.'0' .and. call2(1:1).le.'9' .and. &
     call2(2:2).ge.'0' .and. call2(2:2).le.'9') go to 2

  do i=1,EGRIDMAX
     if(grid(1:2).eq.egrid(i)) go to 2
  enddo

  if((grid(1:2).eq."GK" .and. grid(1:4).ne."GK03") .or. &
     (grid(1:2).eq."IF" .and. grid(1:4).ne."IF32") .or. &
     (grid(1:2).eq."IH" .and. grid(1:4).ne."IH74") .or. &
     (grid(1:2).eq."II" .and. grid(1:4).ne."II22") .or. &
     (grid(1:2).eq."JD" .and. grid(1:4).ne."JD15") .or. &
     (grid(1:2).eq."KE" .and. grid(1:4).ne."KE83") .or. &
      grid(1:3).eq."KF7" .or. &
     (grid(1:2).eq."LE" .and. grid(1:3).ne."LE5" .and. grid(1:3).ne."LE6") .or. &
      grid(1:4).eq."LJ08" .or. &
     (grid(1:2).eq."PB" .and. grid(1:4).ne."PB14") .or. &
      grid(1:4).eq."PI83" .or. &
     (grid(1:3).eq."PO8" .and. grid(1:4).ne."PO80") .or. &
      grid(1:3).eq."PP8" .or. grid(1:3).eq."PQ8" .or. &
     (grid(1:2).eq."QL" .and. grid(1:3).ne."QL0" .and. grid(1:3).ne."QL1") .or. &
     (grid(1:2).eq."RB" .and. grid(1:4).ne."RB25" .and. grid(1:4).ne."RB32") .or. &
     (grid(1:2).eq."RK" .and. grid(1:4).ne."RK39"))  go to 2


  go to 4

2 falsedec=.true.; return

4 return
end subroutine filtersde
