subroutine imopen(plotfile)
  character*(*) plotfile
  common/imcom/ lu,npage

  lu=80
  open(lu,file=plotfile,status='unknown')
  write(lu,1000) 
1000 format('%!PS-Adobe-2.0'/                                            &
          '/rightshow { dup stringwidth pop neg 0 rmoveto show } def'/   &
          '/centershow { dup stringwidth pop neg 2 div ',                &
          '0 rmoveto show } def'/                                        &
          '/lt { lineto } def'/'%%Page: 1 1')
  npage=1

  return
end subroutine imopen

subroutine impalette(palette)
  character*(*) palette
  integer r(0:8),g(0:8),b(0:8)
  integer rr,gg,bb
  common/imcom/ lu,npage
  common/imcom2/rr(0:255),gg(0:255),bb(0:255)

  if(palette.eq.'afmhot') then
     do i=0,255
        j=255-i
        rr(i)=min(255,2*j)
        gg(i)=max(0,min(255,2*j-128))
        bb(i)=max(0,min(255,2*j-256))
     enddo
  else if(palette.eq.'hot') then
     do i=0,255
        j=255-i
        rr(i)=min(255,3*j)
        gg(i)=max(0,min(255,3*j-256))
        bb(i)=max(0,min(255,3*j-512))
     enddo
  else
     open(11,file="Palettes/"//palette,status="old")
     do j=0,8
        read(11,*) r(j),g(j),b(j)
     enddo
     close(11)
     do i=0,255
        j0=i/32
        j1=j0+1
        k=i-32*j0
        rr(i)=r(j0) + int((k*(r(j1)-r(j0)))/31 + 0.5)
        gg(i)=g(j0) + int((k*(g(j1)-g(j0)))/31 + 0.5)
        bb(i)=b(j0) + int((k*(b(j1)-b(j0)))/31 + 0.5)
     enddo

  endif

  return
end subroutine impalette

subroutine imclose
  common/imcom/ lu,npage
  write(lu,1000)
1000 format('showpage'/'%%Trailer')
  close(lu)
  return
end subroutine imclose

subroutine imnewpage
  common/imcom/ lu,npage
  npage=npage+1
  write(lu,1000) npage,npage
1000 format('showpage'/'%%Page:',2i4)
  return
end subroutine imnewpage

subroutine imxline(x,y,dx)
! Draw a line from (x,y) to (x+dx,y)  integer r,g,b
  common/imcom/ lu,npage
  write(lu,1000) 72.0*x,72.0*y,72.0*dx
1000 format('newpath',2f7.1,' moveto',f7.1,' 0 rlineto stroke')
  return
end subroutine imxline

subroutine imyline(x,y,dy)
! Draw a line from (x,y) to (x,y+dy)
  common/imcom/ lu,npage
  write(lu,1000) 72.0*x,72.0*y,72.0*dy
1000 format('newpath',2f7.1,' moveto 0',f7.1,' rlineto stroke')
  return
end subroutine imyline

subroutine imwidth(width)
  common/imcom/ lu,npage
  write(lu,1000) width
1000 format(f7.1,' setlinewidth')
  return
end subroutine imwidth

subroutine imfont(fontname,npoints)
  character*(*) fontname
  common/imcom/ lu,npage
  write(lu,1000) fontname,npoints
1000 format('/',a,' findfont',i4,' scalefont setfont')
  return
end subroutine imfont

subroutine imstring(string,x,y,just,ndeg)
  character*(*) string
  common/imcom/ lu,npage
  write(lu,1000) 72.0*x,72.0*y,ndeg,string
1000 format(2f7.1,' moveto',i4,' rotate'/'(',a,')')
  if(just.eq.1) write(lu,*) 'rightshow'
  if(just.eq.2) write(lu,*) 'centershow'
  if(just.eq.3) write(lu,*) 'show'
  write(lu,1010) -ndeg
1010 format(i4,' rotate'/)
  return
end subroutine imstring

subroutine imr4mat(z,IP,JP,imax,jmax,zz1,zz2,x,y,dx,dy,nbox)
  real z(IP,JP)
  integer idat(2048)
  common/imcom/ lu,npage

  z1=zz1
  z2=zz2
  if(z1.eq.0.0 .and. z2.eq.0.0) then
     z1=z(1,1)
     z2=z1
     do i=1,imax
        do j=1,jmax
           z1=min(z(i,j),z1)
           z2=max(z(i,j),z2)
        enddo
     enddo
  endif
  scale=255.99/(z2-z1)

  write(lu,1002) 72.0*x,72.0*y,72.0*dx,72.0*dy
1002 format(2f7.1,' translate',2f7.1,' scale')
  write(lu,*) imax,jmax,8,' [',imax,0,0,jmax,0,0,']'
  write(lu,*) '{<'

  do j=1,jmax
     do i=1,imax
        idat(i)=scale*(z(i,j)-z1)
        idat(i)=max(idat(i),0)
        idat(i)=min(idat(i),255)
        idat(i)=255-idat(i)
     enddo
     write(lu,1004) (idat(i),i=1,imax)
1004 format(30z2.2)
  enddo
  write(lu,*) '>} image'
  write(lu,1006) 1.0/(72.0*dx),1.0/(72.0*dy),-72.0*x,-72.0*y
1006 format(2f9.6,' scale',2f7.1,' translate')

  if(nbox.ne.0) then
     write(lu,1010) 72.0*x,72.0*y,72.0*dx,72.0*dy,-72*dx
1010 format('newpath',2f7.1,' moveto',f7.1,' 0 rlineto 0',             &
          f7.1,' rlineto',f7.1,' 0 rlineto closepath stroke')
  endif

  return
end subroutine imr4mat

subroutine imr4mat_color(z,IP,JP,imax,jmax,zz1,zz2,x,y,dx,dy,nbox)
  real z(IP,JP)
  integer idat(2048,3)
  integer rr,gg,bb
  common/imcom/ lu,npage
  common/imcom2/rr(0:255),gg(0:255),bb(0:255)

  z1=zz1
  z2=zz2
  if(z1.eq.0.0 .and. z2.eq.0.0) then
     z1=z(1,1)
     z2=z1
     do i=1,imax
        do j=1,jmax
           z1=min(z(i,j),z1)
           z2=max(z(i,j),z2)
        enddo
     enddo
  endif
  scale=255.99/(z2-z1)

  write(lu,1002) 72.0*x,72.0*y,72.0*dx,72.0*dy
1002 format(2f7.1,' translate',2f7.1,' scale')
  write(lu,1003) imax,jmax,8,imax,0,0,jmax,0,0
1003 format(3i5,' [',6i4,']')
  write(lu,1004) imax
1004 format('{currentfile 3',i4,' mul string readhexstring pop} bind'/   &
          'false 3 colorimage')

  do j=1,jmax
     do i=1,imax
        n=scale*(z(i,j)-z1)
        n=max(n,0)
        n=min(n,255)
        idat(i,1)=rr(n)
        idat(i,2)=gg(n)
        idat(i,3)=bb(n)
     enddo
     write(lu,1005) (idat(i,1),idat(i,2),idat(i,3),i=1,imax)
1005 format(30z2.2)
  enddo

  write(lu,1006) 1.0/(72.0*dx),1.0/(72.0*dy),-72.0*x,-72.0*y
1006 format(2f9.6,' scale',2f7.1,' translate')

  if(nbox.ne.0) then
     write(lu,1010) 72.0*x,72.0*y,72.0*dx,72.0*dy,-72*dx
1010 format('newpath',2f7.1,' moveto',f7.1,' 0 rlineto 0',             &
          f7.1,' rlineto',f7.1,' 0 rlineto closepath stroke')
  endif

  return
end subroutine imr4mat_color

subroutine imr4pro(p,imax,yy1,yy2,x,y,dx,dy,nbox)
  real p(imax)
  common/imcom/ lu,npage

  y1=yy1
  y2=yy2
  if(y1.eq.0.0 .and. y2.eq.0.0) then
     y1=p(1)
     y2=y1
     do i=1,imax
        y1=min(p(i),y1)
        y2=max(p(i),y2)
     enddo
  endif

  xscale=72.0*dx/imax
  xoff=72.0*x
  yscale=72.0*dy
  if(y1.ne.y2) yscale=yscale/(y2-y1)
  yoff=72.0*y

  write(lu,*) '1.416 setmiterlimit'
  write(lu,1002) xoff+0.5*xscale,yoff+yscale*(p(1)-y1)
1002 format('newpath',2f7.1,' moveto')

  do i=2,imax
     write(lu,1004) xoff+(i-0.5)*xscale,yoff+yscale*(p(i)-y1)
1004 format(2f6.1,' lt')
  enddo
  write(lu,*) 'stroke'

  if(nbox.ne.0) then
     write(lu,1010) xoff,yoff,72.0*dx,72.0*dy,-72*dx
1010 format('newpath',2f7.1,' moveto',f7.1,' 0 rlineto 0',               &
          f7.1,' rlineto',f7.1,' 0 rlineto closepath stroke')
  endif

  return
end subroutine imr4pro

subroutine imline(x1,y1,x2,y2)
  common/imcom/ lu,npage
  write(lu,1000) 72*x1,72*y1,72*x2,72*y2
1000 format('newpath',2f7.1,' moveto',2f7.1,' lineto stroke')
  return
end subroutine imline

subroutine imcircle(x,y,radius,shade)
  common/imcom/ lu,npage
  write(lu,1000) shade
1000 format(f7.1,' setgray')
  write(lu,1002) 72*x,72*y,72*radius
1002 format('newpath',3f7.1,' 0 360 arc fill')
  write(lu,1000) 0.0
  write(lu,1004) 72*x,72*y,72*radius
1004 format('newpath',3f7.1,' 0 360 arc stroke')
  return
end subroutine imcircle

subroutine imtriangle(x,y,rr,shade)
  common/imcom/ lu,npage
  write(lu,1000) shade
1000 format(f7.1,' setgray')
  write(lu,1002) 72*x,72*(y+rr)
1002 format('newpath',2f7.1,' moveto ')
  write(lu,1004) 72*(x-rr),72*(y-rr)
1004 format(2f7.1,' lineto ')
  write(lu,1004) 72*(x+rr),72*(y-rr)
  write(lu,*) 'closepath fill 0 setgray'
  write(lu,1002) 72*x,72*(y+rr)
  write(lu,1004) 72*(x-rr),72*(y-rr)
  write(lu,1004) 72*(x+rr),72*(y-rr)
  write(lu,*) 'closepath stroke'
  
  return
end subroutine imtriangle

subroutine imr4prov(p,jmax,xx1,xx2,x,y,dx,dy,nbox)
  real p(jmax)
  common/imcom/ lu,npage

  x1=xx1
  x2=xx2
  if(x1.eq.0.0 .and. x2.eq.0.0) then
     x1=p(1)
     x2=x1
     do j=1,jmax
        x1=min(p(j),x1)
        x2=max(p(j),x2)
     enddo
  endif

  xscale=72.0*dx
  xoff=72.0*x
  if(x1.ne.x2) xscale=xscale/(x2-x1)

  yscale=72.0*dy/jmax
  yoff=72.0*y
  
  write(lu,*) '1.416 setmiterlimit'
  write(lu,1002) xoff+xscale*(x2-p(1)),yoff+0.5*yscale
1002 format('newpath',2f7.1,' moveto')
  
  do j=2,jmax
     write(lu,1004) xoff+xscale*(x2-p(j)),yoff+(j-0.5)*yscale
1004 format(2f6.1,' lt')
  enddo
  write(lu,*) 'stroke'

  if(nbox.ne.0) then
     write(lu,1010) xoff,yoff,72.0*dx,72.0*dy,-72*dx
1010 format('newpath',2f7.1,' moveto',f7.1,' 0 rlineto 0',            &
          f7.1,' rlineto',f7.1,' 0 rlineto closepath stroke')
  endif

  return
end subroutine imr4prov
