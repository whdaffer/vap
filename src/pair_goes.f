c pair_goes.f
c $Id$
c
c Modification History:
c $Log$
c Revision 1.1  1998/10/22 21:43:34  vapuser
c Initial revision
c
c
c
c
c
c 8/8/96 pair_goes() -- two pairs of lat/lon => lin/ele 
c   if minlat=maxlat(for WHOLE case) return minlat/maxlat (lin_start,lin_end)  
c   if minlat<maxlat(for SUB case) return min/max lin/ele                 
c Only for saving time to scan the whole file -- narrow down the lines/eles.   

c note lon0 lon1 are inner values! (west +)
c call NVXEASGVAR(LAT,LON,Z,LIN,ELE,DUM) 
c called by main_goes.c
c----------------------------------------------------------------

      subroutine pair_goes(IPARMS,paras,nret)
      integer paras(*)
c  1.LIN0  2. ELE0  3. NLIN  4. NELE  5. RESL   6. RESE  
c  9. minlat/(ret)minLIN 10. minlon/(ret)minELE  
c  11.maxlat/(ret)maxLIN 12. maxlon/(ret)maxELE
c  note : 9,10,11,12 will be re-set  

      INTEGER IPARMS(640)
      INTEGER LIN0,ELE0
      INTEGER RESL,RESE
      integer minlat,lon0,maxlat,lon1 

      REAL LIN,ELE,DUM,LAT,LON,Z
      INTEGER F,IFUNC

      LIN0=paras(1)
      ELE0=paras(2)
      NLIN=paras(3)
      NELE=paras(4)
      RESL=paras(5)
      RESE=paras(6)

      minlat=paras(9)
      lon0=paras(10)
      maxlat=paras(11)
      lon1=paras(12)

c navigation initialization:
      IFUNC=1 
      F=NVXINIGVAR(IFUNC,IPARMS)    
      if (F.NE.0)  then
        nret=1
        print 888 
        goto 999 
      endif

      if (minlat.ne.maxlat) then
c --------------------------- for SUB case, convert min/max lat/lon:
        LAT=minlat
        LON=lon0
C       write(*,*) 'now convert MIN lat/lon: ',LAT,LON
        F=NVXEASGVAR(LAT,LON,Z,LIN,ELE,DUM)
        if (F.ne.0) then 
          write(*,*) 'ERROR: minimum lat/lon navigation failed'
          nret=1
        goto 999
        endif 
        paras(11)=LIN
        paras(12)=ELE
c  max lat/lon --- min lin/ele !!         

c convert max pair:
        LAT=maxlat
        LON=lon1
C       write(*,*) 'now convert MAX lat/lon: ',LAT,LON
        F=NVXEASGVAR(LAT,LON,Z,LIN,ELE,DUM)
        if (F.ne.0) then
          write(*,*) 'ERROR: maximum lat/lon navigation failed'
          nret=1
          goto 999
        endif
        paras(9)=LIN
        paras(10)=ELE
        goto 999
      endif

      if (minlat.eq.maxlat) then
c -------------------------- for WHOLE case convert min/max lin
c i,j -- image lin/ele
c ii,jj -- area lin/ele
         jj=NELE/2
         j=ELE0+(jj-1)*RESE
         i=LIN0
C        type *, 'jj,j,i=',jj,j,i
         if (i.eq.0) then
            write(*,*) 'ERROR: ilin_start cannot be zero'
            nret=1
            goto 999 
         endif
         
         LIN=i
         ELE=j
C        write(*,*) 'check lin0/ele0:',LIN,ELE
         F=NVXSAEGVAR(LIN,ELE,DUM,LAT,LON,Z)
C        write(*,*) 'check (Min?) Lon/Lat, F:',LON,LAT, F
         if (F.eq.0) then
            paras(11)=LAT*100
c         else 
c             goto 999
         endif
         

c  keep same ELE:
         j=ELE0+NELE*RESE
         i=LIN0+NLIN*RESL
         LIN=i
C        write(*,*) 'check lin1/ele1:',LIN,ELE
         F=NVXSAEGVAR(LIN,ELE,DUM,LAT,LON,Z)
C        write(*,*) 'check (Max?) Lon/Lat,F :',LON,LAT,F
         if (F.eq.0) then
            paras(9)=LAT*100
c         else 
c           goto 999
         endif
c---------------------------------------
      endif
      
         
 888  format('ERROR: call NVXINIGVAR failed')
 999  continue
      return
      END


