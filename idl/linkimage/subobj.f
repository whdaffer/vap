
      subroutine subobj( sp,rinfo,ug,vg,l,m,nvect,time,lev,
     1sumx,sumy,a,store,rltmn,rlnmn,rltinc,rlninc,zstd,nvesc,trainf,
     2nop,gamma,denom,ermx,rainf,udev,vdev,alat,work)
c ********************************************************************
c
c whd, 8-jan-1996. Adapted for use in NSCAT Value Added Processing.
c This routine will be called with rinfo(6,*) = u  and rinfo(7,*) = v, the 
c two componenets of the nscat winds. rinfo(5,*) = 1 until we decide to 
c incorporate the time dimension.
c
c Modifications Log:
c
c $Log$
c
c
c $Id$
c
c I   sp:special (bad) value if(rinfo(6,*),(7,*) = sp) vector not used.
c I,O rinfo(1,kk)=1.
c I,O rinfo(2,kk)=latitude    (O: y grid coordinate)
c I,O rinfo(3,kk)=rlon        (O: x grid coordinate)
c I,O rinfo(4,kk)=zstd(lev)   (O: height weight)
c I,O rinfo(5,kk)=time        (O: time weight)
c I,O rinfo(6,kk)=height
c I,O rinfo(7,kk)=height (if(nvesc.eq.1))
c I,O ug, vg:
c I,O l,m:
c I,O nvect: number of points in rinfo.  (O: n.retained vectors).
c I,O time
c I,O lev
c I,O sumx, sumy
c I,O a
c I,O store
c I,O rltmn,rlnmn
c I,O rltinc,rlninc
c I,O zstd
c I,O nvesc
c I,O trainf
c I,O nop
c I,O pres
c I,O gamma
c I,O denom
c I,O ermx
c I,O rainf spatial search radii, one per iteration, in gridstep units.
c I,O udev, vdev
c I,O alat
c I,O lt,mt
c
c 890123, vz copied from drm.
c 900205: vz changed print formats, added comments. no change in logic.
c
c       dimension sumx(l,m),sumy(l,m),denom(3),nel(4),
c     1rinfo(7,nvect),ug(l,m),vg(l,m),ermx(4),gamma(9),
c     2rainf(4),store(l,m),zstd(3),
c     3a(3,l,m),udev(nvect),vdev(nvect),alat(nvect)
      real*4 sumx(l,m),sumy(l,m),denom(3),nel(4),
     1rinfo(7,nvect),ug(l,m),vg(l,m),ermx(4),gamma(9),
     2rainf(4),zstd(3), work(l,m) ,
     3a(3,l,m),udev(nvect),vdev(nvect),alat(nvect)
      integer*4 store(l,m), l,m



cd      type *,'%loc(rinfo) = ',%loc(rinfo)
cd      type *,'%loc(ug) = ',%loc(ug)
cd      type *,'%loc(vg) = ',%loc(vg)
cd      type *,'%loc(udev) = ',%loc(udev)
cd      type *,'%loc(vdev) = ',%loc(vdev)
cd      type *,'%loc(alat) = ',%loc(alat)
cd      type *,'%loc(a) = ',%loc(a)
cd      type *,'%loc(sumx) = ',%loc(sumx)
cd      type *,'%loc(sumy) = ',%loc(sumy)
cd      type *,'%loc(store) = ',%loc(store)

cd      print 115,l,m,nvect,time
  115 format(5x,'l =',i4,5x,'m =',i4,5x,'input vectors =',
     1  i5,2x,'time=',f9.0)
c
cd       type *,"rltmn, rltinc, rlnmn, rlninc= "
cd       type*,rltmn, rltinc, rlnmn ,rlninc
cd       type *," rainf = ",(rainf(i), i=1,4)
c    define constants
c
cd      do j=1,5
cd        do i=1,5
cd          type*,' i,j,%loc(ug(i,j)), ug(i,j) = '
cd          type*,i,j,ug(i,j), %loc(ug(i,j))
cd        enddo
cd      enddo
cd      do j=1,7
cd        do i=1,5
cd          type*,'i,j,%loc(rinfo(i,j)), rinfo(i,j) = '
cd          type*,i,j,rinfo(i,j), %loc(rinfo(i,j))
cd        enddo
cd      enddo
c      stop

      l1=l-1
      m1=m-1
c
c northern most latitude and easternmost longitude
c
       rlnmx=rlnmn+rlninc*float(l1)
       rltmx=rltmn+rltinc*float(m1)
      avect=float(nvect)
      xlm=float(l*m)
      xl1m1=float(l1*m1)
      d=sqrt(xl1m1/avect)
c
cd      print 1010,d
 1010 format(1x,'average station separation is',f4.1,'  grid lengths')
c
c     spatial search radii, rainf(1)-rainf(4), scaled by average station
c     separation.  (commented out)
c	do 1300 i=1,4
c1300   rainf(i)=rainf(i)*d
c
c alpha, zalpha: coefficients for time and height weigting functions.
      alpha=-alog(0.5)/trainf**2
      zalpha=1.
c
c dout: data farther than 'dout' degrees from grid edges rejected offhand.
      dout=rainf(1)*rlninc
cd      type *,"dout = ",dout

      k=0
c
c     Replace time slot  in rinfo by time weighting factor.
c     Replace height slot in rinfo by height weighting factor.
      do 1 i=1,nvect
c         rinfo(5,i)=exp(-alpha*(time-rinfo(5,i))**2)
c         rinfo(4,i)=exp(-zalpha*(zstd(lev)-rinfo(4,i))**2)
         rinfo(5,i)=1.
         rinfo(4,i)=1.
1      continue

c
cd      type*," rltmn, rltmx = ",rltmn, rltmx
cd      type*," rlnmn, rlnmx = ",rlnmn, rlnmx
      do 10 i=1,nvect
c       type*," k = ",k

c       -- finds position of vector relative to grid edges
        d1=rinfo(2,i)-rltmx
        d2=rinfo(2,i)-rltmn
        alat(i)=abs(cos(rinfo(2,i)*3.1416/180.))
        d3=rinfo(3,i)-rlnmx
        d4=rinfo(3,i)-rlnmn
        d2=-d2
        d4=-d4
cd        if (i.eq.31802) then 
cd          type*,'i,rinfo(2:3,i) = ',i,rinfo(2,i),rinfo(3,i)
cd          type*,'d1,d2,d3,d4 = ',d1,d2,d3,d4
cd          mm =  -d2/rltinc+1.
cd          ll = -d4/rlninc+1.
cd          type*, 'rinfo(2:3,i) will equal ', ll,mm
cd        endif 
cd       type*," i,d1,d2,d3,d4 = ", i,d1,d2,d3,d4
c       -- rejects data farther than dout from edges.
        if(d1.gt.dout.or.d2.gt.dout.or.d3.gt.dout.or.d4.gt.dout)then
           goto 10
        endif 
c       -- tags accepted data lying outside grid.
        if(d1.gt.0.or.d2.gt.0.or.d3.gt.0.or.d4.gt.0) 
     >     rinfo(1,i)=-abs(rinfo(1,i))
c       -- replaces latitude, longitude slots with grid coordinates
        rinfo(2,i)=-d2/rltinc+1.
        rinfo(3,i)=-d4/rlninc+1.
c       -- increments 'points accepted' counter k.
        k=k+1
c       -- shifts accepted data vectors up, to save space. 
c       -- Done once per accepted point!!!
        do 9 ii=1,7
          rinfo(ii,k)=rinfo(ii,i)
9       continue
c       -- whd, inserted next line of code (8-jan-1996)
        alat(k) = alat(i)
cd        if (i.eq.31802) then 
cd           type*,' vector 31802  is now in location ',k
cd           kk = k
cd        endif 
10    continue
c
cd      type *,'after 10 continue'
      nn=nvect-k
cd      type *,'nn, nvect = ',nn, nvect
      if(k.ne.nvect)then 
cd        print 100,nn,k
  100 format(5x,i6,' vectors rejected',i6,' vectors remaining')
        nvect=k
        avect=float(nvect)
      else
        type *,'nvect=',nvect
      endif
 
c ********************************
c     find variance of input data
c ***********************************
      usum=0.
      vsum=0.
      xsum=0.
cd      type *,'taking variance of input data '
      do 120 i=1,nvect
        if(rinfo(6,i).eq.sp.or.rinfo(7,i).eq.sp)go to 120
        xsum=xsum+1.
        usum=usum+rinfo(6,i)
        if(nvesc.eq.1)vsum=vsum+rinfo(7,i)
  120 continue
      ubar=usum/xsum
      vbar=vsum/xsum
      uvar=0.
      vvar=0.
      do 125 i=1,nvect
        if(rinfo(6,i).eq.sp.or.rinfo(7,i).eq.sp)go to 125
        uvar=uvar+(ubar-rinfo(6,i))*(ubar-rinfo(6,i))
        if(nvesc.eq.1)vvar=vvar+(vbar-rinfo(7,i))*(vbar-rinfo(7,i))
  125 continue
      uvar=sqrt(uvar/xsum)
      vvar=sqrt(vvar/xsum)
cd      print 2060,xsum,ubar,vbar,uvar,vvar
c
c  ********************      *********************
c variance du guess field initial 
c************************************************
cd      type*,'Taking the average and variance of gues field'
      ugsum=0.
      vgsum=0.
      xgsum=0.
      do 119 i=1,l
        do 119 j=1,m
c          type*,'average: about to access ug(i,j) for i,j = ',i,j
          if(ug(i,j).eq.sp)go to 119
          xgsum=xgsum+1.
          ugsum=ugsum+ug(i,j)
          if(nvesc.eq.1)vgsum=vgsum+vg(i,j)
c          type *,'xgsum ,ugsum,vgsum = ',xgsum,ugsum,vgsum
119   continue
      ugbar=ugsum/xgsum
      if(nvesc.eq.1)vgbar=vgsum/xgsum
      ugvar=0.
      vgvar=0.
      do 124 i=1,l
        do 124 j=1,m
c          type*,'variance: about to access ug(i,j) for i,j = ',i,j
          if(ug(i,j).eq.sp.or.vg(i,j).eq.sp)go to 124
          ugvar=ugvar+(ugbar-ug(i,j))*(ugbar-ug(i,j))
          if(nvesc.eq.1)vgvar=vgvar+(vgbar-vg(i,j))*(vgbar-vg(i,j))
124   continue
      ugvar=sqrt(ugvar/xgsum)
      if(nvesc.eq.1)vgvar=sqrt(vgvar/xgsum)
cd      print 123,xgsum,ugbar,vgbar,ugvar,vgvar
123   format(1x,'nbre de val guess field ',f6.0,' ugbar= ',
     1f8.2,' vgbar= ',f6.2,' u sd=',f8.2,' v sd=',f8.2)
c **************************************************************
c     interpolation of data
c
cd      type*,' just after avg/var of guess field'

      do 2000 ipass=1,nop
cd        print 1990,ipass
 1990   format(' ************* pass number ',i3,'**************' )
        cressman = 0
        nel(ipass)=0
        r=rainf(ipass)
        r2=r*r
        ermax=ermx(ipass)
c
c       -- first find deviations at stations from guess field
c
cd        type *,'debut ao   nvesc=',nvesc
        do 40 ist=1,nvect
          udev(ist)=0.
          vdev(ist)=0.
          x=rinfo(3,ist)
          y=rinfo(2,ist)
c
c         -- calculate deviation at station inside grid : lagrangian interpolation
c
          if(rinfo(1,ist).le.0.) goto40
15        continue
          ix=x+.5
          iy=y+.5
c          type *,' x,y,ix,iy = ',x,y,ix,iy
          if(ix.eq.1) ix=2
          if(ix.eq.l) ix=l1
          if(iy.eq.1) iy=2
          if(iy.eq.m) iy=m1
          xp=x-float(ix)
          yp=y-float(iy)
          dx=1.
          dy=1.
          
          yk1=yp*(yp-dy)
          yk2=(yp+dy)*(yp-dy)
          yk3=(yp+dy)*yp
          xk1=xp*(xp-dx)
          xk2=(xp+dx)*(xp-dx)
          xk3=(xp+dx)*xp
          g1=xk1*yk1*denom(1)
          g2=xk2*yk1*denom(2)
c
          g3=xk3*yk1*denom(1)
          g4=xk1*yk2*denom(2)
          g5=xk2*yk2*denom(3)
          g6=xk3*yk2*denom(2)
          g7=xk1*yk3*denom(1)
          g8=xk2*yk3*denom(2)
          g9=xk3*yk3*denom(1)
          ixm=ix-1
          ixp=ix+1
          iym=iy-1
          iyp=iy+1
          uval=g1*ug(ixm,iym)+g2*ug(ix,iym)+g3*ug(ixp,iym)
     1      +g4*ug(ixm,iy)+g5*ug(ix,iy)+g6*ug(ixp,iy)
     2      +g7*ug(ixm,iyp)+g8*ug(ix,iyp)+g9*ug(ixp,iyp)
          if(nvesc.eq.1)vval=g1*vg(ixm,iym)+g2*vg(ix,iym)+g3*vg(ixp,iym)
     1      +g4*vg(ixm,iy)+g5*vg(ix,iy)+g6*vg(ixp,iy)
     2      +g7*vg(ixm,iyp)+g8*vg(ix,iyp)+g9*vg(ixp,iyp)
          udev(ist)=rinfo(6,ist)-uval
          if(nvesc.eq.1)vdev(ist)=rinfo(7,ist)-vval
cd          if (ist.eq.kk) then 
cd            type *,' (31802=k) lagrangian interpolation,'
cd            type *,' kk, x,y,float(ix),float(iy),xp,yp'
cd           type *,kk, x,y,float(ix),float(iy),xp,yp
cd            type *,' xp,yp,xk1,xk2,xk3,yk1,yk2,yk3,='
cd            type *,xp,yp,xk1,xk2,xk3,yk1,yk2,yk3
cd            type*,'g1,2,3,4,5,6,7,8,9= ', g1,g2,g3,g4,g5,g6,g7,g8,g9
cd            type *,'ug = ',ug(ixm,iym),ug(ix,iym),ug(ixp,iym)
cd            type *,'     ',ug(ixm,iy),ug(ix,iy),ug(ixp,iy)
cd            type *,'     ',ug(ixm,iyp),ug(ix,iyp)
cd            type *,'     ', ug(ixp,iyp)
cd            type *,'vg = ',vg(ixm,iym),vg(ix,iym),vg(ixp,iym)
cd            type *,'     ',vg(ixm,iy),vg(ix,iy),vg(ixp,iy)
cd            type *,'     ', vg(ixm,iyp),vg(ix,iyp)
cd            type *,'     ',vg(ixp,iyp)
cd            type*,' uval, vval = ', uval, vval
cd            print *,' nvesc=',nvesc
cd          endif 
          if(abs(udev(ist)).gt.ermax) go to 39
          if(nvesc.eq.0)goto40
          if(abs(vdev(ist)).gt.ermax) goto39
          goto40
 39       continue
          xlat=(rinfo(2,ist)-1.)*rltinc+rltmn
          xlon=(rinfo(3,ist)-1.)*rlninc+rlnmn
cd          if(ipass.eq.1)then 
cd            type *,'bad piece of data inside grid, vector # ='
cd            type *, ist
cd            type *, 'lat = ',xlat
cd            type *,' lon = ',xlon
cd            type *,' u = ',rinfo(6,ist)
cd            type *,' v = ',rinfo(7,ist)
cd            type *,'u/vguess = ',uval,vval
cd            type*,'u/vdev = ',udev(ist),vdev(ist)
cd          endif 
          
c         write(21,168)ist,xlat,xlon,rinfo(6,ist),rinfo(7,ist),
c    1    uval,vval
 168  format(i8,6f7.2)
          nel(ipass)=nel(ipass)+1
          udev(ist)=999.
          if(nvesc.eq.1)vdev(ist)=999.
 40     continue
cd        print *,'nbre de valeurs inside elim',nel(ipass)
c
c     -- calculate deviation at station outside grid : cressman weightind function
c
        do 45 ist=1,nvect
          id=ifix(abs(rinfo(1,ist))+0.01)
          if(rinfo(1,ist).gt.0.) go to 45
          cressman = cressman+1
          udev(ist)=0.
          vdev(ist)=0.
          x=rinfo(3,ist)
          y=rinfo(2,ist)
          a1=0.
          a2=0.
          a3=0.
          jj=y
          j1=jj-ifix(r)-1
          if(j1.gt.m) go to 44
          j2=jj+ifix(r)+1
          if(j2.lt.1) go to 44
          if(j1.lt.1) j1=1
          if(j2.gt.m) j2=m
          ii=x
c          i1=ii-ifix(r)-1
c          i2=ii+ifix(r)+1

          i1=ii-ifix(r/alat(ist))-1
          i2=ii+ifix(r/alat(ist))+1
c
          if(i1.gt.l) go to 44
          if(i2.lt.1) go to 44
          if(i1.lt.1) i1=1
          if(i2.gt.l) i2=l
          do 34 i=i1,i2
c           added alat correction, whd
            xdist=abs(x-float(i))*alat(ist)
c            xdist=abs(x-float(i))
            do 33 j=j1,j2
c              ydist=abs(y-float(j))*alat(ist)
c             deleted alat correction
              ydist=abs(y-float(j))
              dist2=xdist*xdist+ydist*ydist
              p1=r2-dist2
              if(p1) 33,33,31
 31           p2=r2+dist2
              weight=p1/p2
              a1=a1+weight*ug(i,j)
              if(nvesc.eq.1)a2=a2+weight*vg(i,j)
              a3=a3+weight
 33         continue
 34       continue
          if(a3.lt.0.0000000001) go to 44
          udev(ist)=rinfo(6,ist)-a1/a3
          if(nvesc.eq.1)vdev(ist)=rinfo(7,ist)-a2/a3
          uval=a1/a3
          if(nvesc.eq.1)vval=a2/a3
          if(abs(udev(ist)).gt.ermax) go to 46
          if(nvesc.eq.0)goto45
          if(abs(vdev(ist)).gt.ermax) goto46
          goto45
46        continue
          xlat=(rinfo(2,ist)-1.)*rltinc+rltmn
          xlon=(rinfo(3,ist)-1.)*rlninc+rlnmn
cd          if(ipass.eq.1)then 
cd            type *,'bad piece of data outside grid, vector # ='
cd            type *, ist
cd            type *, 'lat = ',xlat
cd            type *,' lon = ',xlon
cd            type *,' u = ',rinfo(6,ist)
cd            type *,' v = ',rinfo(7,ist)
cd            type *,'u/vguess = ',uval,vval
cd            type*,'u/vdev = ',udev(ist),vdev(ist)
cd          endif 

c          if(ipass.eq.1)print 167,ist,xlat,xlon,rinfo(6,ist),
c     1      rinfo(7,ist),uval,vval,udev(ist),vdev(ist)
c167        format(5x,'bad piece of data outside grid    vector number',
c     1    i5,'  lat=',f6.1,'  long=',f6.1,
c     2    '  u=',g10.3,'  v=',g10.3,/,10x,'  uguess=',g10.3,'  vguess=',
c     3    g10.3,'  udev=',g10.3,'  vdev=',g10.3)
c         write(22,168)ist,xlat,xlon,rinfo(6,ist),rinfo(7,ist),
c      1  uval,vval
          nel(ipass)=nel(ipass)+1
44        continue
          udev(ist)=999.
          if(nvesc.eq.1)vdev(ist)=999.
45      continue
cd        print *,'nbre total de valeurs elim',nel(ipass)
cd        print *,' total # of point in cressman section', cressman
c
c       -- calculation of correction to grid point value.
        cgmax=0.5*r
        do 69 i=1,l
          do 69 j=1,m
            a(1,i,j)=0.0
            a(2,i,j)=0.0
            a(3,i,j)=0.0
            sumx(i,j)=0.
            sumy(i,j)=0.
            store(i,j)=0.
69      continue
        do 70 ist=1,nvect
          id=ifix(abs(rinfo(1,ist))+0.01)
          if (udev(ist).eq.999.) go to 70
          if (vdev(ist).eq.999.) go to 70
          x=rinfo(3,ist)
          y=rinfo(2,ist)
          jj=y
c         -- added alat(i) factor to increase number of grid points included
c         -- in calculation of grid correction
          j1=jj-ifix(r)-1
          if(j1.gt.m) go to 70
          j2=jj+ifix(r)+1
          if(j2.lt.1) go to 70
          if(j1.lt.1) j1=1
          if(j2.gt.m) j2=m
          ii=x
          i1=ii-ifix(r/alat(i))-1
          i2=ii+ifix(r/alat(i))+1
c          i1=ii-ifix(r)-1
c          i2=ii+ifix(r)+1
          if(i1.gt.l) go to 70
          if(i2.lt.1) go to 70
          if(i2.gt.l) i2=l
          if(i1.lt.1) i1=1
          if (ist .eq. kk) then
cd            type *,' 31802=kk, x,y,i1,i2,j1,j2,jj = '
cd            type*,kk,x,y,i1,i2,j1,j2,jj
          endif 

          do 60 i=i1,i2
            xx=i
c           -- added alat(ist) correction, whd 29-jan-1996
            xdist=abs(x-xx)*alat(ist)
c            xdist=abs(x-xx)
            do 59 j=j1,j2
              yy=j
c             ydist=abs(y-yy)*alat(ist)
c             deleted alat(ist) correction, whd 29-jan-1996
              ydist=abs(y-yy)
              dist2=xdist*xdist+ydist*ydist
              p1=r2-dist2
cd              if (ist .eq.kk) then
cd                type *,' (31802) i,j,r,r2,x,xx,y,yy,xdist,ydist,p1='
cd                type*,i,j,r,r2,x,xx,y,yy,xdist,ydist,p1
cd              endif 
              if(p1) 59,59,55
 55           p2=r2+dist2
              weight=p1/p2*rinfo(5,ist)*gamma(id)*rinfo(4,ist)
              a(1,i,j)=a(1,i,j)+udev(ist)*weight
              if(nvesc.eq.1)a(2,i,j)=a(2,i,j)+vdev(ist)*weight
              a(3,i,j)=a(3,i,j)+weight
              store(i,j)=store(i,j)+1
              sumx(i,j)=sumx(i,j)+x-xx
              sumy(i,j)=sumy(i,j)+y-yy
cd              if (ist .eq. kk) then
cd                type *,' (31802) i,j, p2,weight,a(1,i,j),a(3,i,j) = '
cd                type*,i,j, p2,weight,a(1,i,j),a(3,i,j)
cd                type *,' (31802) sumx(i,j),sumy(i,j) = '
cd                type*, sumx(i,j),sumy(i,j)
cd              endif 
 59         continue
 60       continue
 70     continue
        do 63 i=1,l
          do 63 j=1,m
            if(store(i,j).eq.0) go to 63
            xc=sumx(i,j)/store(i,j)
            yc=sumy(i,j)/store(i,j)
            sumx(i,j)=sqrt(xc*xc+yc*yc)
c  
c          -- calculate the corrected fields
c  
            p1dp2=1.
            if  (sumx(i,j).lt.cgmax) go to 62
c            p1dp2=(r-sumx(i,j))/(0.5*r)
c            removed by whd, 29-jan-96
             p1dp2 = 1 
 62         if(a(3,i,j)) 63,63,68
 68         continue
cd            if (i. eq. 195 .and. j .eq. 54) then
cd               type*,'Before correction '
cd               type*,'i,j,',i,j
cd               type*,'ug(i,j),vg(i,j)=',ug(i,j),vg(i,j)
cd               type*,'i,j, ', i,j
cd               type*,'a(1,i,j),a(3,i,j),p1dp2=',a(1,i,j),a(3,i,j),p1dp2
cd             endif 

            ug(i,j)=ug(i,j)+a(1,i,j)/a(3,i,j)*p1dp2
            if(nvesc.eq.1)vg(i,j)=vg(i,j)+a(2,i,j)/a(3,i,j)*p1dp2
cd            if (i.eq.195.and.j.eq.54) then 
cd               type *,'after: '
cd               type*,'i,j,ug(i,j),vg(i,j)=',i,j,ug(i,j),vg(i,j)
cd            endif 
            

 63     continue
c  
c       -- find root mean square error in u and v
c  
        k=0
        ve=0.
        ue=0.
        do 90 ist=1,nvect
          if(rinfo(1,ist).le.0.) go to 90
 85       x=rinfo(3,ist)
          y=rinfo(2,ist)
          ix=x+.000001
          iy=y+.000001
          if(ix.eq.l) ix=l1
          if(iy.eq.m) iy=m1
          ixp1=ix+1
          iyp1=iy+1
          u1=ug(ix,iy)
          if(nvesc.eq.1)v1=vg(ix,iy)
          u2=ug(ix,iyp1)
          if(nvesc.eq.1)v2=vg(ix,iyp1)
          u3=ug(ixp1,iyp1)
          if(nvesc.eq.1)v3=vg(ixp1,iyp1)
          u4=ug(ixp1,iy)
          if(nvesc.eq.1)v4=vg(ixp1,iy)
          d1=x-float(ix)
          d2=y-float(iy)
          uu  =u1*(1.-d1)*(1.-d2)+u4*d1*(1.-d2)+u3*d1*d2+u2*d2*(1.-d1)
          if(nvesc.eq.1)then
            vv  =v1*(1.-d1)*(1.-d2)+v4*d1*(1.-d2)+v3*d1*d2+v2*d2*(1.-d1)
          endif
          if(rinfo(6,ist).eq.sp)go to 90
          if(nvesc.eq.1)then
            if(rinfo(7,ist).eq.sp)go to 90
          endif
          u1=(rinfo(6,ist)-uu)
          ue=ue+u1*u1
          if(nvesc.eq.1)v1=rinfo(7,ist)-vv
          if(nvesc.eq.1)ve=ve+v1*v1
          k=k+1
 90     continue
        ue=ue/float(k)
        if(nvesc.eq.1)ve=ve/float(k)
        urms=sqrt(ue)
        if(nvesc.eq.1)then
          vrms=sqrt(ve)
          vec=sqrt(ue+ve)
        else
          vrms=0.
          vec=0.	
        endif
cd        print 200,k,urms,vrms,vec
 200   format(1x,'nbre de pts',i6,'rms u error=',f8.3,1x,
     1  'rms v error=',f8.3,1x,
     2  'rms vector error=',f8.3,1x,'para unit')
 2000 continue
      if(nvesc.eq.1)then 
cd        type*,' calling smooth for vg'
        call smooth(sp,vg,work,l,m)
      endif 
cd      type*,' calling smooth for ug'
      call smooth(sp,ug,work,l,m)
c **********************************************
c     find variance of analyzed field
c     xgri,usum,vsum,ubar2etvbar2 concernent champ total sur l indien
c **********************************************
c      do 777 j=1,m
c      do 777 i=1,l
c      if(nvesc.eq.1.and.xl(i,j).eq.0.)vg(i,j)=sp
c      if(xl(i,j).eq.0.)ug(i,j)=sp
c777   continue
cd     type *,'1'
      xgri=0.
      usum=0.
      vsum=0.
      do 2050 i=1,l
      do 2050 j=1,m
        if(ug(i,j).eq.sp)go to 2050
        if(nvesc.eq.1.and.vg(i,j).eq.sp)go to 2050
        xgri=xgri+1.
        usum=usum+ug(i,j)
        if(nvesc.eq.1)vsum=vsum+vg(i,j)
 2050 continue
	ubar2=usum/xgri
	if(nvesc.eq.1)vbar2=vsum/xgri
	uvar2=0.
	if(nvesc.eq.1)vvar2=0.
	do 2055 i=1,l
	do 2055 j=1,m
	  if(ug(i,j).eq.sp)go to 2055
  	  uvar2=uvar2+(ug(i,j)-ubar2)*(ug(i,j)-ubar2)
	  if(nvesc.eq.1)then
	  if(vg(i,j).eq.sp)go to 2055
	  vvar2=vvar2+(vg(i,j)-vbar2)*(vg(i,j)-vbar2)
	endif
2055	continue
      uvar2=sqrt(uvar2/xgri)
      upctv=-1.
      if(nvesc.eq.1)vvar2=sqrt(vvar2/xgri)
      if (uvar .gt. 1.0e-10) upctv=(uvar2/uvar)*100.*(uvar2/uvar)
      vpctv=-1.
      if(nvesc.eq.1.and.vvar.gt.1.0e-10) 
     1    vpctv=(vvar2/vvar)*100.*(vvar2/vvar)
 2060 format(10x,'initial data nbre=',f7.0,'  ubar=',f8.2,'  vbar=',
     1f8.2,' u std=',f8.2,'  v std=',f8.2)
cd      print 2064,xgri,ubar2,vbar2,uvar2,vvar2
 2064 format(10x,'analyzed field nbre=',f7.0,' ubar=',f8.2,'  vbar=',
     1f8.2,' u std=',f8.2,'  v std=',f8.2)
cd      print 2066,upctv,vpctv
 2066 format(10x,'u % variance',f10.1,3x,'v % variance',f10.1)
cd      type *, ' SUBOBJ: About to return'
      return
      end
c=====================================================================
      subroutine smooth(sp,a,b,l,m)
      dimension a(l,m),b(l,m)
cd      type *,' in smooth '
cd      type *,' l = ',l
cd      type *,' m = ',m
      l1=l-1
      m1=m-1
      rnu=0.1
      rnu1=0.5*rnu*(1.-rnu)
      rnu2=0.25*rnu*rnu
cd     type *,'l1,m1 = ',l1,m1
      do 10 i=2,l1
        ip=i+1
        im=i-1
        do 10 j=2,m1
           jp = j+1
           jm = j-1
c           type *,'3, i,j,ip,jp,im,jm = ',i,j,ip,jp,im,jm
          if(a(i,j).eq.sp.or.a(ip,j).eq.sp)go to 10
c           type *,'4'
          if(a(im,j).eq.sp.or.a(i,jp).eq.sp)go to 10
c           type *,'5'
          if(a(i,jm).eq.sp.or.a(im,jp).eq.sp)go to 10
c           type *,'6'
          if(a(im,jm).eq.sp.or.a(ip,jp).eq.sp)go to 10
c           type *,'7'
          if(a(ip,jm).eq.sp)go to 10
c           type *,'8'
          b(i,j)=a(i,j)+rnu1*(a(im,j)+a(ip,j)+a(i,j+1)+a(i,j-1)-
     ^4.*a(i,j))+rnu2*(a(im,j+1)+a(im,j-1)+a(ip,j+1)+a(ip,j-1)-
     ^4.*a(i,j))
c          type *,'b = ',b(i,j)
   10 continue
cd     type *,'3'
      do 11 i=2,l1
        do 11 j=2,m1
          a(i,j)=b(i,j)
   11 continue
cd     type *,' (smooth): about to return  '
      return
      end
