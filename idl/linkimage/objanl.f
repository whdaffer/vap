      SUBROUTINE objanl( sp,rinfo,ug,vg,l,m,nvect,nreps,time,
     1 sumx,sumy,a,store,rltmn,rlnmn,rltinc,rlninc,nvesc,trainf,
     2 gamma,denom,ermx,rainf,udev,vdev,alat,work)
c ********************************************************************
C
C Adapted from 'subobj.f' which I got from Victor Zlotniki.
C This subroutine is based (loosely) on the paper 
C
C  "Low-Level Flows of the GATE area during Summer 1972"
C   Gregory j. Tripoli and T.N.Krishnamurti
C   Dept of Meteorology, Florida State University, Tallahassee Fla
C   
C  Monthly Weather Review, Volume 103
C  pp 197-216 
C  March 1975.
C 
C $Id:
C 
c Modifications Log:
c
c $Log$
c
c
c $Id$
c
c I   sp:special (bad) value if(rinfo(5,*),(6,*) = sp) vector not used.
c I,O rinfo(1,kk)=1.
c I,O rinfo(2,kk)=latitude    (O: y grid coordinate)
c I,O rinfo(3,kk)=longitude   (O: x grid coordinate)
c I,O rinfo(4,kk)=time        (O: time weight)
c I,O rinfo(5,kk)=1st dimension of data.
c I,O rinfo(6,kk)=2nd dimension of data.  (if (nvesc.eq.1))
c I,O ug, vg: Guess fields.
c I,O l,m: 
c I,O nvect: number of points in rinfo.  (O: n.retained vectors).
C I   nreps: number of iterations throught the *big* loop.
c I,O time
c I,O sumx, sumy
c I,O a
c I,O store
c I,O rltmn,rlnmn
c I,O rltinc,rlninc
c I,O nvesc : Flag, if data is 2-dimensional, rinfo(6,*) has the second dimension.
c I,O trainf
c I,O pres
c I,O gamma
c I,O denom
c I,O ermx:  Error for each iteration of big loop. Data larger than
C            this is rejected.
c I,O rainf: spatial search radii, one per iteration, in gridstep units.
c I,O udev, vdev
c I,O alat
c I,O lt,mt
c

      implicit none

C ---------- Arguments -----------------

      integer*4 l,m,nreps, nvect,nvesc,store(l,m)

      real*4 sp,sumx(l,m),sumy(l,m),denom(3),nel(10),
     1 rinfo(6,nvect),ug(l,m),vg(l,m),ermx(10),gamma(9),
     2 rainf(10), work(l,m),time(nvect),
     3 a(3,l,m),udev(nvect),vdev(nvect),alat(nvect), 
     4 trainf


C -------------- Local Variables -----------------

      real*4 d,d1,d2,d3,d4, rltmn,rltmx,rlnmn, rlnmx, rltinc, 
     1 rlninc, alpha, zalpha, dout, usum, vsum, xsum, ubar, 
     2 vbar, uvar, vvar, ugsum, vgsum, xgsum, ugbar, vgbar, 
     3 r,r2,x, y,xp,yp,dx,dy,ermax,crsmn,xk1,xk2,xk3,
     4 yk1,yk2,yk3,g1,g2,g3,g4,g5,g6,g7,g8,g9,a1,a2,a3,
     5 uval,vval,xlon,ylon,xdis,ydis,dis2,p1,p2,weight,cgmax,
     6 xx,yy,xc,p1dp2,ve,ue,u1,v1, ugvar, vgvar, xlat, xdist, 
     7 ydist, dist2, yc, u2, v2, u3, v3, u4, v4, uu, vv, 
     8 urms, vrms, vec, xgri, ubar2, uvar2, vbar2, vvar2,
     9 upctv, vpctv, aa,bb, cc,dd,ee,uval1, vval1


      integer*4 i,j,k,l1,m1,ii,jj,kk,nn,ix,iy,ixm,iym,ixp,
     1 iyp,ipass,id,ist,j1,j2,i1,i2,ixp1,iyp1,badi,badone, 
     2 dummy, first
       

      l1=l-1
      m1=m-1
c
c northern most latitude and easternmost longitude
c
      rlnmx=rlnmn+rlninc*float(l1)
      rltmx=rltmn+rltinc*float(m1)
d      d=sqrt(float(l1*m1)/float(nvect))
c
d      print 1010,d
 1010 format(1x,'average station separation is',f7.2,'  grid lengths')

d     print 1011,ug(6,1), vg(7,1)
d     print 1012,ug(1,6),vg(1,7)
d1011  format(1x,'Ug(6,1)= ',f7.2,', vg(7,1)= ',f7.2)
d1012  format(1x,'Ug(1,6)= ',f7.2,', vg(1,7)= ',f7.2)
c
c     spatial search radii, rainf(1)-rainf(4), scaled by average station
c     separation.  (commented out)
c	do 1300 i=1,4
c1300   rainf(i)=rainf(i)*d
c
c alpha, zalpha: coefficients for time and height weigting functions.

      alpha=-alog(0.5)/trainf**2
c
c
c     Replace time slot  in rinfo by time weighting factor.
      do 1 i=1,nvect
c         rinfo(4,i)=exp(-alpha*(time-rinfo(4,i))**2)
         rinfo(4,i)=1.
         rinfo(4,i)=1.
1      continue

c dout: data farther than 'dout' degrees from grid edges rejected out of hand.

      dout=rainf(1)*rlninc
      k=0

c
c     type *,'rltmn,rltmx,rlnmn,rlnmx=',rltmn,rltmx,rlnmn,rlnmx
      do 10 i=1,nvect

C        alat(i)=abs(cos(rinfo(2,i)*3.1416/180.))
        alat(i)=1.

c       -- finds position of vector relative to grid edges
        d1=rinfo(2,i)-rltmx
        d2=rinfo(2,i)-rltmn
        d3=rinfo(3,i)-rlnmx
        d4=rinfo(3,i)-rlnmn
        d2=-d2
        d4=-d4

        

c       -- rejects data farther than dout from edges.
        if(d1.gt.dout.or.d2.gt.dout.or.d3.gt.dout.or.d4.gt.dout)then
           goto 10
        endif 

c       -- tags accepted data lying outside grid.
        if(d1.gt.0.or.d2.gt.0.or.d3.gt.0.or.d4.gt.0) then 
          rinfo(1,i)=-abs(rinfo(1,i))
        endif 

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
        alat(k) = alat(i)
10    continue

      nn=nvect-k
      if(k.ne.nvect)then 
d        print 100,nn,k
  100 format(5x,i6,' vectors rejected',i6,' vectors remaining')
        nvect=k
      else
d        type *,'nvect=',nvect
      endif
 
c ********************************
c     find variance of input data
c ***********************************

      usum=0.
      vsum=0.
      xsum=0.
      ubar=0.
      vbar=0.
      uvar=0.
      vvar=0.

      do 120 i=1,nvect
        if(rinfo(5,i).eq.sp.or.rinfo(6,i).eq.sp)go to 120
        xsum=xsum+1.
        usum=usum+rinfo(5,i)
        if(nvesc.eq.1)vsum=vsum+rinfo(6,i)
 120  continue
      if (xsum.gt.1.e-10) then 
         ubar=usum/xsum
         vbar=vsum/xsum
      else
d         type*,"(120) xsum <= 0."
      endif  

      uvar=0.
      vvar=0.
      do 125 i=1,nvect
        if(rinfo(5,i).eq.sp.or.rinfo(6,i).eq.sp)go to 125
        uvar=uvar+(ubar-rinfo(5,i))*(ubar-rinfo(5,i))
        if(nvesc.eq.1)vvar=vvar+(vbar-rinfo(6,i))*(vbar-rinfo(6,i))
  125 continue
      if (xsum .gt. 1.e-10) then 
         uvar=sqrt(uvar/xsum)
         vvar=sqrt(vvar/xsum)
       else 
d          type *,'(125) xsum<=0.0.'
       endif 
d      print 2060,xsum,ubar,vbar,uvar,vvar
 2060 format(10x,'initial data nbre=',f7.0,'  ubar=',f8.2,'  vbar=',
     1     f8.2,' u std=',f8.2,'  v std=',f8.2)

c


c ******************************************
c variance of initial guess field 
c *******************************************

      ugsum=0.
      vgsum=0.
      xgsum=0.
      ugbar=0.
      vgbar=0.
      ugvar=0.
      vgvar=0.

C      ====== Average ==========      
      do 119 i=1,l
        do 119 j=1,m
           if(nvesc.eq.1) then 
              if(ug(i,j).eq.sp.or. vg(i,j).eq.sp)go to 119
              xgsum=xgsum+1.
              ugsum=ugsum+ug(i,j)
              vgsum=vgsum+vg(i,j)
           else
              if(ug(i,j).eq.sp)go to 119
              xgsum=xgsum+1.
              ugsum=ugsum+ug(i,j)
           endif 
119   continue
      if (xgsum .gt. 1.e-10) then 
         ugbar=ugsum/xgsum
         if(nvesc.eq.1)vgbar=vgsum/xgsum
      else
d         type *,'(119) xgsum=0.0'
      endif 

C      ====== Variance ==========      

      ugvar=0.
      vgvar=0.
      do 124 i=1,l
        do 124 j=1,m
          if (nvesc .eq. 1) then 
             if(ug(i,j).eq.sp.or.vg(i,j).eq.sp)go to 124
             ugvar=ugvar+(ugbar-ug(i,j))*(ugbar-ug(i,j))
             vgvar=vgvar+(vgbar-vg(i,j))*(vgbar-vg(i,j))
          else 
             if(ug(i,j).eq.sp)go to 124
             ugvar=ugvar+(ugbar-ug(i,j))*(ugbar-ug(i,j))
          endif 
124   continue
      if (xgsum .gt. 1.e-10) then 
         ugvar=sqrt(ugvar/xgsum)
         if(nvesc.eq.1)vgvar=sqrt(vgvar/xgsum)
      else
d        type *,'(124) xgsum=0.0'
      endif 

d      print 123,xgsum,ugbar,vgbar,ugvar,vgvar
123   format(1x,'nbre de val guess field ',f6.0,' ugbar= ',
     1f8.2,' vgbar= ',f6.2,' u sd=',f8.2,' v sd=',f8.2)

c **************************************************************
c     interpolation of data
c **************************************************************


d     type *,'Nreps = ',nreps      
      do 2000 ipass=1,nreps
d        print 1990,ipass
 1990   format(' ************* pass number ',i3,' **************' )
        crsmn = 0
        nel(ipass)=0
        r=rainf(ipass)
        r2=r*r
        ermax=ermx(ipass)
d       print 1991, r, ermax
d1991   format ('Rainf= ',f6.2,', ermax= ',f6.2)
c
c       -- first find deviations at stations from guess field
c
        badi=1

        do 40 ist=1,nvect
          badone=0
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

c          uval=0.
c          vval=0.
c          do 3000 i=ix-1,ix+1
c            do 3000 j=iy-1,iy+1
c               aa=1.
c               bb=1.
c               do 3001 ii=ix-1,ix+1
c                 if (ii.eq.i) goto 3001
c                 aa=aa*(x-ii)/(i-ii)
c 3001          continue
c               do 3002 jj=iy-1,iy+1
c                 if (jj.eq.j) goto 3002
c                 bb=bb*(y-jj)/(j-jj)
c 3002          continue
c                 uval=uval + ug(i,j)*aa*bb
c                 if (nvesc.eq.1) vval=vval + vg(i,j)*aa*bb
c 3000       continue
c
c          if (abs(uval-uval1).gt.0.1.or.
c     1        abs(vval-vval1).gt.0.1) then
c             type*,"u/vval diff!",uval,uval1,vval,vval1
c          endif 
          udev(ist)=rinfo(5,ist)-uval
          if(nvesc.eq.1)vdev(ist)=rinfo(6,ist)-vval

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
cd          if (abs(udev(ist)).gt.ermax)badone=1
cd          if (nvesc.eq.1.and.abs(vdev(ist)).gt.ermax)badone=1
cd          if(ipass.eq.1.and.badi.lt.20.and.badone)then 
cd            type *,'bad piece of data inside grid, vector # =',ist
cd            type *, '  lon/lat = ',xlon,xlat
cd            type *,'   u/v = ',rinfo(5,ist),rinfo(6,ist)
cd            type *,'  u/v guess = ',uval,vval
cd            type *,'  u/v dev = ',udev(ist),vdev(ist)
cd            badi=badi+1
cd          endif 
          if(abs(udev(ist)).gt.ermax) go to 39
          if(nvesc.eq.0)goto40
          if(abs(vdev(ist)).gt.ermax) goto39
          goto40
 39       continue
d          xlat=(rinfo(2,ist)-1.)*rltinc+rltmn
d          xlon=(rinfo(3,ist)-1.)*rlninc+rlnmn
d         
d         write(21,168)ist,xlat,xlon,rinfo(5,ist),rinfo(6,ist),
d    1    uval,vval
 168  format(i8,6f7.2)
          nel(ipass)=nel(ipass)+1
          udev(ist)=999.
          if(nvesc.eq.1)vdev(ist)=999.
 40     continue

d        print *,'nbre de valeurs inside elim',nel(ipass)


c
c     -- calculate deviation at station OUTSIDE GRID : 
c          cressman weighting function
c
        do 45 ist=1,nvect

          id=ifix(abs(rinfo(1,ist))+0.01)
          if(rinfo(1,ist).gt.0.) go to 45
          crsmn = crsmn+1
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

          if (alat(ist).gt.1.e-10) then 
             i1=ii-ifix(r/alat(ist))-1
             i2=ii+ifix(r/alat(ist))+1
          else 
             i1=ii-ifix(r)-1
             i2=ii+ifix(r)+1
d           type *,"(i1/i2), alat(ist) <= 0"
          endif 
c
          if(i1.gt.l) go to 44
          if(i2.lt.1) go to 44
          if(i1.lt.1) i1=1
          if(i2.gt.l) i2=l
          do 34 i=i1,i2
            xdist=abs(x-float(i))*alat(ist)
            do 33 j=j1,j2
              ydist=abs(y-float(j))
              dist2=xdist*xdist+ydist*ydist
              p1=r2-dist2
              if(p1) 33,33,31
 31           p2=r2+dist2
              if (p2.gt.1.e-10) then 
                weight=p1/p2
              else 
                weight=0.
d               type *,'(31) p2<=0.'
              endif 
C              weight=1.
              a1=a1+weight*ug(i,j)
              if(nvesc.eq.1)a2=a2+weight*vg(i,j)
              a3=a3+weight
 33         continue
 34       continue
          if(a3.lt.0.0000000001) go to 44
          udev(ist)=rinfo(5,ist)-a1/a3
          if(nvesc.eq.1)vdev(ist)=rinfo(6,ist)-a2/a3
          uval=a1/a3
          if(nvesc.eq.1)vval=a2/a3
          if(abs(udev(ist)).gt.ermax) go to 46
          if(nvesc.eq.0)goto45
          if(abs(vdev(ist)).gt.ermax) goto46
          goto45
46        continue
d          xlat=(rinfo(2,ist)-1.)*rltinc+rltmn
d          xlon=(rinfo(3,ist)-1.)*rlninc+rlnmn

d         if(ipass.eq.1)then 

d           print 167,ist,xlat,xlon,rinfo(5,ist),
d    1        rinfo(6,ist),uval,vval,udev(ist),vdev(ist)
d         endif 
d167      format(5x,'bad piece of data outside grid    vector number',
d    1     i5,'  lat=',f6.1,'  long=',f6.1,
d    2      '  u=',g10.3,'  v=',g10.3,/,10x,'  uguess=',g10.3,' 
d    3      vguess=',g10.3,'  udev=',g10.3,'  vdev=',g10.3)
c         write(22,168)ist,xlat,xlon,rinfo(5,ist),rinfo(6,ist),
c    1      uval,vval
          nel(ipass)=nel(ipass)+1
44        continue
          udev(ist)=999.
          if(nvesc.eq.1)vdev(ist)=999.
45      continue
d        print *,'nbre total de valeurs elim',nel(ipass)
d        print *,' total # of point in cressman section', crsmn
c



c       -- calculation of correction to grid point value ------

C       Initialize Arrays.
        cgmax=0.6*r
        do 69 i=1,l
          do 69 j=1,m
            a(1,i,j)=0.0
            a(2,i,j)=0.0
            a(3,i,j)=0.0
            sumx(i,j)=0.
            sumy(i,j)=0.
            store(i,j)=0.
69      continue


C       -- For each 'station' find the guess field locations within the current radius
C          of influence,   
        first=1
        do 70 ist=1,nvect
          id=ifix(abs(rinfo(1,ist))+0.01)
          if (udev(ist).eq.999.) go to 70
          if (vdev(ist).eq.999.) go to 70
          x=rinfo(3,ist)
          y=rinfo(2,ist)
          ii=x
          jj=y


c          -- j1,j2 are the Y limits of the grid points within 
C             this radius of influence          
          j1=jj-ifix(r)-1
          if(j1.gt.m) go to 70
          j2=jj+ifix(r)+1
          if(j2.lt.1) go to 70
          if(j1.lt.1) j1=1
          if(j2.gt.m) j2=m


c         -- added alat(i) factor to increase number of grid points included
c         -- in calculation of grid correction

c          -- i1,i2 are the X limits of the grid points within 
C             this radius of influence, corrected for latitude.

          if (alat(i).gt.1.e-10) then 
             i1=ii-ifix(r/alat(i))-1
             i2=ii+ifix(r/alat(i))+1
          else
            i1=ii-ifix(r)-1
            i2=ii+ifix(r)+1
d            type *,"(i1/2, 2) alat(i) <=0."
          endif 
c          i1=ii-ifix(r)-1
c          i2=ii+ifix(r)+1
          if(i1.gt.l) go to 70
          if(i2.lt.1) go to 70
          if(i2.gt.l) i2=l
          if(i1.lt.1) i1=1
          if (ist .eq. kk) then
          endif 

          do 60 i=i1,i2
            xx=i
            xdist=abs(x-xx)*alat(ist)
            do 59 j=j1,j2
              yy=j
              ydist=abs(y-yy)
              dist2=xdist*xdist+ydist*ydist
              p1=r2-dist2
              if(p1) 59,59,55
 55           p2=r2+dist2
              if (p2 .gt. 1.e-10) then 
                 weight=p1/p2*rinfo(4,ist)*gamma(id)
              else 
d                 type *,"(55) p2<=0"
                 weight=rinfo(4,ist)*gamma(id)
              endif 
              a(1,i,j)=a(1,i,j)+udev(ist)*weight
              if(nvesc.eq.1)a(2,i,j)=a(2,i,j)+vdev(ist)*weight
              a(3,i,j)=a(3,i,j)+weight
              store(i,j)=store(i,j)+1
              sumx(i,j)=sumx(i,j)+x-xx
              sumy(i,j)=sumy(i,j)+y-yy
d            if (i.eq.10 .and. j.eq.15) then 
d              aa=a(1,i,j)
d              bb=a(2,i,j)
d              cc=a(3,i,j)
d              if (first.eq.1) then 
d                print *, 
d    1 ' Lon,Lat,u,v,udev,vdev,a(1:3,i,j),w(s),sumx,sumy,num '
d                 first=0
d               endif 
d              xlon=(x-1)*rlninc+rlnmn
d              xlat=(y-1)*rltinc+rltmn
d              dd=rinfo(5,ist)
d              ee=rinfo(6,ist)
d              print 170, xlon,xlat,dd,ee,
d    1             udev(ist),vdev(ist),aa,bb, 
d    2             cc,weight,sumx(i,j),sumy(i,j), 
d    3             dist2,store(i,j)
d
cd170            format( f7.2, f7.2,',a(1.)= ', f8.2, ' ,a(2.)= ', f8.2,
cd    2          ',a(3.)= ',f8.2,',weight= ', f6.4,', sumx= ', f8.1, 
cd    3              ',sumy= ',f8.1, ',store= ', i4)
d170            format( 2(f7.2,1x),7(f8.3,1x),f6.4,1x,f8.3,1x,
d    1                  f10.3,1x,f8.3,1x,i4)
cd170            format( f7.2,1x,f7.2,1x,f8.3,1x,f8.3,,1x,f8.3, 1x,
cd    2           f8.3, 1x,f8.3,1x,f8.3,1x,f8.3,1x,f6.4, 1x,f8.3,1x,
cd    3            f10.3,1x,f8.3,1x,i4)
d             endif 
d             if (weight.ge.1.and.
d    1              (xdist.gt.0.or.ydist.gt.0)) then 
d                print 171, i,j,weight
d171              format('i= ',i3,',j= ',i3,',weight= ',f6.4)
d             endif 
 59         continue
 60       continue
 70     continue


c  
c          -- calculate the corrected fields
c  

        do 63 i=1,l
          do 63 j=1,m
            if(store(i,j).eq.0) go to 63
            xc=sumx(i,j)/store(i,j)
            yc=sumy(i,j)/store(i,j)
            sumx(i,j)=sqrt(xc*xc+yc*yc)

            p1dp2=1.
C            if  (sumx(i,j).lt.cgmax) go to 62
C            if (ipass.gt.1) then 
C               aa=(0.4*r)**2
C               bb=(r-abs(sumx(i,j)))**2
C               p1dp2=(aa-bb)/(aa+bb)
C               
C               aa=(r2- sumx(i,j))
C               if (aa .lt.0) aa=0.
C               bb=(r2+sumx(i,j))
C               p1dp2=aa/bb
C
C            endif 

 62         if(a(3,i,j)) 63,63,68
 68         continue

            ug(i,j)=ug(i,j)+a(1,i,j)/a(3,i,j)*p1dp2
            if(nvesc.eq.1)vg(i,j)=vg(i,j)+a(2,i,j)/a(3,i,j)*p1dp2
d
d           if (i.eq.10 .and. j.eq.15) then 
d             print *,'i,j,ug,vi,a(1-3),p1dp2,xc,yc,sumx'
d             print 169,i,j,ug(i,j),vg(i,j),a(1,i,j),a(2,i,j),a(3,i,j),
d    1           p1dp2,xc,yc,sumx(i,j)
d169          format(5x,i2,1x,i2,1x,f8.3,1x,f8.3,1x,f8.3,1x,f8.3,1x,f8.3, 
d    3                 f8.3,1x,f8.3,1x,f8.3,1x,f8.3)
d           endif 
d
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
          if(rinfo(5,ist).eq.sp)go to 90
          if(nvesc.eq.1)then
            if(rinfo(6,ist).eq.sp)go to 90
          endif
          u1=(rinfo(5,ist)-uu)
          ue=ue+u1*u1
          if(nvesc.eq.1)then 
             v1=rinfo(6,ist)-vv
             ve=ve+v1*v1
          endif 
          k=k+1
 90     continue
        if (k .gt. 1.e-10) then 
          ue=ue/float(k)
          if(nvesc.eq.1)ve=ve/float(k)
        else 
d           type *,"(90) k == 0"
           goto 2000
        endif
        urms=sqrt(ue)
        if(nvesc.eq.1)then
          vrms=sqrt(ve)
          vec=sqrt(ue+ve)
        else
          vrms=0.
          vec=0.
        endif
d        print 200,k,urms,vrms,vec
 200   format(1x,'nbre de pts',i6,'rms u error=',f8.3,1x,
     1  'rms v error=',f8.3,1x,
     2  'rms vector error=',f8.3,1x,'para unit')

      dummy=1
 2000 continue

C     ===== Done with the big loop !=============


c      if(nvesc.eq.1)then 
cd        type*,' calling smooth for vg'
c        call smooth(sp,vg,work,l,m)
c      endif 
cd      type*,' calling smooth for ug'
c      call smooth(sp,ug,work,l,m)

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
      if (xgri.gt.1.e-10) then 
        ubar2=usum/xgri
        if(nvesc.eq.1)vbar2=vsum/xgri
      else 
d         type *,"(2050) xgri <= 0."
         ubar2=0.
      endif 
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
 2055 continue
      if (xgri.gt. 1.e-10) then 
        uvar2=sqrt(uvar2/xgri)
        if(nvesc.eq.1)vvar2=sqrt(vvar2/xgri)
      else 
d         type *,"(2055) xgri <= 0."
      endif 
      upctv=-1.
      if (uvar .gt. 1.0e-10) upctv=(uvar2/uvar)*100.*(uvar2/uvar)
      vpctv=-1.
      if(nvesc.eq.1.and.vvar.gt.1.0e-10) 
     1     vpctv=(vvar2/vvar)*100.*(vvar2/vvar)
d      print 2064,xgri,ubar2,vbar2,uvar2,vvar2
 2064 format(10x,'analyzed field nbre=',f7.0,' ubar=',f8.2,'  vbar=',
     1f8.2,' u std=',f8.2,'  v std=',f8.2)
d      print 2066,upctv,vpctv
 2066 format(10x,'u % variance',f10.1,3x,'v % variance',f10.1)
d      type *, ' OBJANALYSIS: About to return'
cd     do 2067 i=1,10
cd       ug(i,1)=i*2+1
cd       vg(1,i)=i*2
cd2067 continue
      return
      end


C ================== End objanalysis ==========================


      subroutine smooth(sp,a,b,l,m)
      dimension a(l,m),b(l,m)
      l1=l-1
      m1=m-1
      rnu=0.1
      rnu1=0.5*rnu*(1.-rnu)
      rnu2=0.25*rnu*rnu
      do 10 i=2,l1
        ip=i+1
        im=i-1
        do 10 j=2,m1
           jp = j+1
           jm = j-1
          if(a(i,j).eq.sp.or.a(ip,j).eq.sp)go to 10
          if(a(im,j).eq.sp.or.a(i,jp).eq.sp)go to 10
          if(a(i,jm).eq.sp.or.a(im,jp).eq.sp)go to 10
          if(a(im,jm).eq.sp.or.a(ip,jp).eq.sp)go to 10
          if(a(ip,jm).eq.sp)go to 10
          b(i,j)=a(i,j)+
     ^ rnu1*(
     ^     a(im,j)+a(ip,j)+a(i,jp)+a(i,jm)-4.*a(i,j)
     ^      )+
     ^ rnu2*(
     ^     a(im,jp)+a(im,jm)+a(ip,jp)+a(ip,jm)-4.*a(i,j)
     ^      )
   10 continue
      do 11 i=2,l1
        do 11 j=2,m1
          a(i,j)=b(i,j)
   11 continue
      return
      end

