c navig_goes.f
c $Id$
c Modification History:
c $Log$
c Revision 1.1  1998/10/22 21:43:43  vapuser
c Initial revision
c
c
c
c
c 8/8/96  navig_goes.f ( navigation between  lin/ele -- lat/lon for GOES)
c
c  Output GOESx_xxxxxxx.dat 
c        (head lines + data (did lon switch only, 16-bit integers!))   
c
c  passed in : SLIN0,SELE0,SLIN,SELE,RESL,RESE,SCALA,SCALO,minlat,minlon,
c              maxlat,maxlon,goes_type,jday,time,ima_l,ima_e
c
c  GOESx_xxxxxxx.dat content: (open/write unformatted)
c     1. Head line: goes_type, array lin/ele size, jday, time, minlat,lon0,
c                maxlat,lon1, and lat/lon precision
c     2. image data (ima_e,ima_l)
c----------------------------------------------------------------------- 
c ** NOTE: F77 array(i) start from 1  while C and IDL start from 0 !!
c          F77 and IDL array(col,row) while C array(row,col) !! 

c  . do interpolate when fill in?

c  .called by main_goes.c
c  .lon0 & lon1 are inner values (west +) here !!
c----------------------------------------------------------------

      subroutine navig_goes(IPARMS,IVAL,paras,ima,iima)
      character*3 cf1
      character*3 cf2
      character*4 cf3
      character*80 outfilenm
      integer paras(*)
c     1: SLIN0
c     2: SELE0
c     3:SLIN
c     4:SELE
c     5:RESL
c     6:RESE
c     7:SCALA
c     8:SCALO
c     9:minlat
c     10:lon0
c     11:maxlat        
c     12:lon1        
c     13: goes_type.
c     14: jday
c     15: time 
c     16: ima_l
c     17: ima_e 

c   ** IVAL,ima (col,row), col/row start from 1 
      INTEGER IPARMS(640)
      INTEGER*2 IVAL(paras(4),paras(3))
      INTEGER*2 ima(paras(17),paras(16))
      INTEGER*2 iima(paras(17),paras(16))

      INTEGER scala,scalo,SLIN0,SELE0,SLIN,SELE
      INTEGER RESL,RESE,ima_l,ima_e,filln,goes_type
      REAL preci_l,preci_e,rlat,rlon

      REAL LIN,ELE,DUM,LAT,LON,Z
      INTEGER F,IFUNC,i,j,ii,jj,tot
c      write(*,*) (paras(i),i=1,17)

      goes_type=paras(13) 
      if (paras(13).lt.100) then 
         write(cf1,'(i2)') paras(13) 
         cf1='0'//cf1
      endif
      if (paras(13).ge.100) then 
         write(cf1,'(i3)') paras(13) 
      endif

c ----------------------------------------------------------- 
c  Get output file name with goes_type+Jday+hhmm:

      if (paras(14).lt.10) then
         write(cf2,'(i1)') paras(14)
         cf2='00'//cf2
      endif
      if ((paras(14).lt.100).and.(paras(14).ge.10)) then
         write(cf2,'(i2)') paras(14)
         cf2='0'//cf2
      endif
      if (paras(14).ge.100) then 
         write(cf2,'(i3)') paras(14)
      endif 
   
      if (paras(15).lt.10) then
         write(cf3,'(i1)') paras(15)
         cf3='000'//cf3
      endif
      if (paras(15).lt.100) then
         write(cf3,'(i2)') paras(15)
         cf3='00'//cf3
      endif
      if ((paras(15).lt.1000).and.(paras(15).ge.100)) then 
         write(cf3,'(i3)') paras(15)
         cf3='0'//cf3
      endif
      if (paras(15).ge.1000) then
         write(cf3,'(i4)') paras(15) 
      endif

      
      outfilenm='GOES'//cf1//'_'//cf2//cf3//'.dat'  
c-------------------------------------------
      
      SLIN0=paras(1)
      SELE0=paras(2)
      SLIN=paras(3)
      SELE=paras(4)
      RESL=paras(5)
      RESE=paras(6)
      scala=paras(7)
      scalo=paras(8) 
      minlat=paras(9)
      lon0=paras(10)
      maxlat=paras(11)
      lon1=paras(12) 
      ima_l=paras(16)
      ima_e=paras(17)
c ima=scal x deg range    scal=1/precision
c precision .02 deg -- scale 50  

      preci_l=1./scala
      preci_e=1./scalo
      
      lla0=minlat*scala
      llo0=lon0*scalo

      filln=0
c****** fill value to the near +-n spots
      if (((RESL.le.2).and.(RESE.le.2)).or.
     +   ((preci_e.gt.0.02).and.(preci_e.gt.0.02))) then
         filln=1
      elseif (((RESL.gt.2).or.(RESE.gt.2)).and. 
     +       ((preci_e.gt.0.02).and.(preci_e.gt.0.02))) then
         filln=2
      elseif (((RESL.gt.2).or.(RESE.gt.2)).and.
     +        ((preci_e.eq.0.02).and.(preci_e.eq.0.02))) then
         filln=3
      elseif ((preci_e.lt.0.02).or.(preci_l.lt.0.02)) then 
c         write(*,*) 'The best (smallest) precision for GOES8/9 is 0.02'
      endif

c      write(*,126) filln 
126   format( ':: Range filled in blank neighbours +-',i2)
c      write(*,*)
c      write(*,121) 
121   format(':: Output file header :')
c      write(*,122) goes_type
122   format('  GOES type: ',i4)
c      write(*,123) ima_l,ima_e 
123   format('  array size: (ima_l,ima_e) ',2(2x,i6))
c      write(*,125) paras(14),paras(15) 
125   format('  jday, time(hhmm) ',2(2x,i6))
c      write(*,127) minlat,-lon1
127   format('  minlat minlon (deg) ',2(2x,i6)) 
c      write(*,129) maxlat,-lon0
129   format('  maxlat maxlon (deg) ',2(2x,i6))
c      write(*,131) preci_l,preci_e 
131   format('  precision (deg) ',2(2x,f4.2))
c      write(*,*)  

c navigation initialization:
      IFUNC=1 
      F=NVXINIGVAR(IFUNC,IPARMS)    
      if (F.NE.0)  then
         print 888 
         goto 999 
      endif

      tot=0

c i,j -- image lin/ele
c ii,jj -- area lin/ele
clllllllllllllllllllllllllllllllllllllllllllllllllllllllll
      DO 444 i=1,SLIN         
         ii=SLIN0+(i-1)*RESL       
c 4 line one value, so pick mid-line 
         DO 555 j=1,SELE
            jj=SELE0+(j-1)*RESE

            LIN=ii
            ELE=jj
            F=NVXSAEGVAR(LIN,ELE,DUM,LAT,LON,Z)
c-----------------------------
            if (F.eq.0) then 
c     start from indx 1 .... 
               lla1=(LAT*scala)-lla0+1    
c.............................
c     if  LON=-160  save it to 200  
c     (i.e.  -200 to -70 for 160E to 70W) 
c.............................
               if ((lon0.gt.lon1).and.(LON.lt.lon1)) then 
                  llo1=(360+LON)*scalo-llo0+1
               else
                  llo1=(LON*scalo)-llo0+1
               endif
               
               if ((lla1.gt.0).and.(llo1.gt.0).and.    
     +              (lla1.le.ima_l).and.(llo1.le.ima_e)) then 
c........................
c     fill in the lat/lon grids:          
                  ima(llo1,lla1)=IVAL(j,i)     
                  
                  if ((filln.gt.0).and.
     +                 ((lla1-filln).gt.0).and.
     +                 ((llo1-filln).gt.0).and.
     +                 ((lla1+filln).le.ima_l).and.
     +                 ((llo1+filln).le.ima_e)) then
                     do 44 nj=llo1-filln,llo1+filln
                        do 55 ni=lla1-filln,lla1+filln 
                           if (ima(nj,ni).eq.0) then 
                              ima(nj,ni)=IVAL(j,i)
                              tot=tot+1 
                           endif
55                      continue
44                   continue
                  endif
               endif
c.....................
            endif
c----  if F==0 end --------------
555      continue
444   continue
cpppppppppppppppppppppppppppppppppppppppppppppppppppp

666   len=ima_l*ima_e
c      write(*,*) 'filled in blank grids (+-1 or 2)',tot,' of',len 

c********** prepare for idl display *******:
c i.e., swap west with east?
c
      do 77 j=1,ima_e
         do 88 i=1,ima_l
            iima(j,i)=ima(ima_e-j+1,i)
88       continue
77    continue

c ** Head line: array size (lin,ele), jday, time, lat/lon boundary,
c    and  lat/lon precision:
c
c      OPEN(UNIT=60,FILE=outfilenm,
c     +      FORM='UNFORMATTED')
c      write(60) goes_type,ima_l,ima_e, paras(14),paras(15), 
c     +           minlat,-lon1,maxlat,-lon0,preci_l,preci_e 
c      write(60) ((iima(j,i),j=1,ima_e),i=1,ima_l) 
c      close(60)
c
c      write(*,133) outfilenm
c
c
133   format(':: output to  ',60a)
888   format('ERROR: call NVXINIGVAR failed')
999   continue

      return
      END




c================================================ NAVIGATION CODE:




      INTEGER FUNCTION NVXINIGVAR(IFUNC,IPARMS)                             
                                                                        
C *** McIDAS Revision History ***      
C 1 NVXGVAR.DLM 5-Jul-94,13:01:32,`KENB' Initial release of GVAR navigation
C      module. (4663)
C 2 NVXGVAR.DLM 25-Aug-94,13:30:14,`USER' Released
C 3 NVXGVAR.DLM 29-Aug-94,10:56:58,`RUSSN' Corrected revision history cards
C 4 NVXGVAR.DLM 13-Sep-94,12:09:06,`USER' Released
C 5 NVXGVAR.DLM 28-Oct-94,15:34:28,`KENB' adopted fix from m/f for changes
C 6 NVXGVAR.DLM 31-Oct-94,12:12:46,`USER' Released
C *** McIDAS Revision History ***      

      IMPLICIT NONE
                                                                        
C     INCLUDE 'ELCONS.INC'
C=========================== DIELCONS =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCONS = INCLUDE                                                
                                                                        
      REAL*8 PI                                                         
           PARAMETER (PI=3.141592653589793D0)                           
      REAL*8 DEG                                                        
           PARAMETER (DEG=180.D0/PI)                                    
      REAL*8 RAD                                                        
           PARAMETER (RAD=PI/180.D0)                                    
C                    DEGREES TO RADIANS CONVERSION PI/180               
      REAL*8 NOMORB                                                     
           PARAMETER (NOMORB=42164.365D0)                               
C                    NOMINAL RADIAL DISTANCE OF SATELLITE (KM)          
      REAL*8 AE                                                         
           PARAMETER (AE=6378.137D0)                                    
C                    EARTH EQUATORIAL RADIUS (KM)                       
      REAL*8 FER                                                        
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                        
C                    EARTH FLATTENING COEFFICIENT = 1-(BE/AE)           
      REAL*4 AEBE2                                                      
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                         
      REAL*4 AEBE3                                                      
           PARAMETER (AEBE3=AEBE2-1.)                                   
      REAL*4 AEBE4                                                      
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                           
C========================= END DIELCONS ===========================     
C     INCLUDE 'ELCOMM.INC'
C=========================== DIELCOMM =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCOMM = INCLUDE                                                
                                                                        
C     COMMON VARIABLES                                                  
C                                                                       
      REAL*8 XS(3)                                                      
C                      NORMALIZED S/C POSITION IN ECEF COORDINATES      
      REAL*8 BT(3,3)                                                    
C                      ECEF TO INSTRUMENT COORDINATES TRANSFORMATION    
      REAL*8  Q3                                                        
C                      USED IN SUBRTN LPOINT                        
      REAL*8 PITCH,ROLL,YAW                                             
C                          PITCH,ROLL,YAW ANGLES OF INSTRUMENT (RAD)    
      REAL*4 PMA,RMA                                                    
C                          PITCH,ROLL MISALIGNMENTS OF INSTRUMENT (RAD) 
         COMMON /ELCOMM/ XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                
C========================= END DIELCOMM ===========================     
C     INCLUDE 'INSTCO.INC'
C=========================== DIINSTCO =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIINSTCO = INCLUDE                                                
                                                                        
      INTEGER*4 INCMAX(2)                                               
C                       NUMBER OF INCREMENTS PER CYCLE                  
      REAL*4 ELVMAX(2)                                                  
C                       BOUNDS IN ELEVATION (RADIANS)                   
      REAL*4 SCNMAX(2)                                                  
C                       BOUNDS IN SCAN ANGLE (RADIANS)                  
      REAL*4 ELVINC(2)                                                  
C                       CHANGE IN ELEVATION ANGLE PER INCREMENT (RAD)   
      REAL*4 SCNINC(2)                                                  
C                       CHANGE IN SCAN ANGLE PER INCREMENT (RADIANS)    
      REAL*4 ELVLN(2)                                                   
C                       ELEVATION ANGLE PER DETECTOR LINE (RADIANS)     
      REAL*4 SCNPX(2)                                                   
C                       SCAN ANGLE PER PIXEL (RADIANS)                  
                                                                        
      COMMON /INSTCO/ INCMAX,ELVMAX,SCNMAX,                             
     1   ELVINC,SCNINC,ELVLN,SCNPX                                      
C========================= END DIINSTCO ===========================     
C     INCLUDE 'GVRCOD.INC'
C=========================== DIGVRCOD =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE GVAR NAVIGATION SUBSYSTEM.       
C $                                                                     
C $   DESCRIPTION:                                                      
C $      THESE ARE CONSTANTS USED TO DESCRIBE THE GVAR CODICIL STRUCTURE
C $                                                                     
C $$  DIGVRCOD = INCLUDE, NAVIGATION, GVAR                              
                                                                        
C DEFINITIONS OF PARAMETERS                                             
C STTYPE   : POSITION OF SATELLITE TYPE                                 
C ETIME    : POSITION OF EPOCH TIME                                     
C IMCACT   : POSITION OF IMC ACTIVE FLAG                                
C IMGDAY   : POSITION OF IMAGE DAY VALUE  (YYDDD)                       
C IMGTM    : POSITION OF IMAGE TIME VALUE (HHMMSS)                      
C REFLON   : POSITION OF REFERENCE LONGITUDE                            
C REFDIS   : POSITION OF REFERENCE DISTANCE FROM NOMINAL                
C REFLAT   : POSITION OF REFERENCE LATITUDE                             
C REFYAW   : POSITION OF REFERENCE YAW                                  
C RATROL   : POSITION OF REFERENCE ATTITUDE ROLL                        
C RATPTC   : POSITION OF REFERENCE ATTITUDE PITCH                       
C RATYAW   : POSITION OF REFERENCE ATTITUDE YAW                         
C LDR1-13  : LOCATION OF LONGITUDE DELTA FROM REFERENCE PARAMETERS      
C RDDR1-11 : LOCATION OF RADIAL DISTANCE DELTA FROM REFERENCE PARAMETERS
C DGL1-9   : LOCATION OF GEOCENTRIC LATITUDE DELTA PARAMETERS           
C DOY1-9   : LOCATION OF ORBIT YAW DELTA PARAMETERS                     
C EXPTIM   : EXPONENTIAL START TIME FROM EPOCH                          
C RAAWDS   : LOCATION OF START OF ROLL ATTITUDE ANGLE WORDS             
C PAAWDS   : LOCATION OF START OF PITCH ATTITUDE ANGLE WORDS            
C YAAWDS   : LOCATION OF START OF YAW ATTITUDE ANGLE WORDS              
C RMAWDS   : LOCATION OF START OF ROLL MISALIGNMENT ANGLE WORDS         
C PMAWDS   : LOCATION OF START OF PITCH MISALIGNMENT ANGLE WORDS        
C EDTIME   : LOCATION OF DELTA FROM EPOCH TIME                          
C IMCROL   : LOCATION OF IMAGE MOTION COMPENSATION ROLL                 
C IMCPTC   : LOCATION OF IMAGE MOTION COMPENSATION PITCH                
C IMCYAW   : LOCATION OF IMAGE MOTION COMPENSATION YAW                  
C IMGSND   : LOCATION OF IMAGER/SOUNDER INSTRUMENT FLAG                 
                                                                        
      INTEGER      ETIME , IMCACT, IMGDAY, IMGTM , RATROL, RATPTC       
      INTEGER      RATYAW, LDR1  , LDR2  , LDR3  , LDR4  , LDR5         
      INTEGER      LDR6  , LDR7  , LDR8  , LDR9  , LDR10 , LDR11        
      INTEGER      LDR12 , LDR13 , RDDR1 , RDDR2 , RDDR3 , RDDR4        
      INTEGER      RDDR5 , RDDR6 , RDDR7 , RDDR8 , RDDR9 , RDDR10       
      INTEGER      RDDR11, DGL1  , DGL2  , DGL3  , DGL4  , DGL5         
      INTEGER      DGL6  , DGL7  , DGL8  , DGL9  , DOY1  , DOY2         
      INTEGER      DOY3  , DOY4  , DOY5  , DOY6  , DOY7  , DOY8         
      INTEGER      DOY9  , EXPTIM, RAAWDS, PAAWDS, YAAWDS, RMAWDS       
      INTEGER      PMAWDS, EDTIME, IMCROL, IMCPTC, IMCYAW, IMGSND       
      INTEGER      REFLON, REFDIS, REFLAT, REFYAW, STTYPE, IDNTFR
      INTEGER      IOFNC , IOFNI , IOFEC , IOFEI
                                                                        
      PARAMETER (STTYPE = 1)                                            
      PARAMETER (IDNTFR = 2)
      PARAMETER (IMCACT = 3)                                            
      PARAMETER (REFLON = 6)                                            
      PARAMETER (REFDIS = 7)                                            
      PARAMETER (REFLAT = 8)                                            
      PARAMETER (REFYAW = 9)                                            
      PARAMETER (RATROL = 10)                                           
      PARAMETER (RATPTC = 11)                                           
      PARAMETER (RATYAW = 12)                                           
      PARAMETER (ETIME  = 13)                                           
      PARAMETER (EDTIME = 15)                                           
      PARAMETER (IMCROL = 16)                                           
      PARAMETER (IMCPTC = 17)                                           
      PARAMETER (IMCYAW = 18)                                           
      PARAMETER (LDR1   = 19)                                           
      PARAMETER (LDR2   = 20)                                           
      PARAMETER (LDR3   = 21)                                           
      PARAMETER (LDR4   = 22)                                           
      PARAMETER (LDR5   = 23)                                           
      PARAMETER (LDR6   = 24)                                           
      PARAMETER (LDR7   = 25)                                           
      PARAMETER (LDR8   = 26)                                           
      PARAMETER (LDR9   = 27)                                           
      PARAMETER (LDR10  = 28)                                           
      PARAMETER (LDR11  = 29)                                           
      PARAMETER (LDR12  = 30)                                           
      PARAMETER (LDR13  = 31)                                           
      PARAMETER (RDDR1  = 32)                                           
      PARAMETER (RDDR2  = 33)                                           
      PARAMETER (RDDR3  = 34)                                           
      PARAMETER (RDDR4  = 35)                                           
      PARAMETER (RDDR5  = 36)                                           
      PARAMETER (RDDR6  = 37)                                           
      PARAMETER (RDDR7  = 38)                                           
      PARAMETER (RDDR8  = 39)                                           
      PARAMETER (RDDR9  = 40)                                           
      PARAMETER (RDDR10 = 41)                                           
      PARAMETER (RDDR11 = 42)                                           
      PARAMETER (DGL1   = 43)                                           
      PARAMETER (DGL2   = 44)                                           
      PARAMETER (DGL3   = 45)                                           
      PARAMETER (DGL4   = 46)                                           
      PARAMETER (DGL5   = 47)                                           
      PARAMETER (DGL6   = 48)                                           
      PARAMETER (DGL7   = 49)                                           
      PARAMETER (DGL8   = 50)                                           
      PARAMETER (DGL9   = 51)                                           
      PARAMETER (DOY1   = 52)                                           
      PARAMETER (DOY2   = 53)                                           
      PARAMETER (DOY3   = 54)                                           
      PARAMETER (DOY4   = 55)                                           
      PARAMETER (DOY5   = 56)                                           
      PARAMETER (DOY6   = 57)                                           
      PARAMETER (DOY7   = 58)                                           
      PARAMETER (DOY8   = 59)                                           
      PARAMETER (DOY9   = 60)                                           
      PARAMETER (EXPTIM = 62)                                           
      PARAMETER (RAAWDS = 63)                                           
      PARAMETER (PAAWDS = 130)                                          
      PARAMETER (YAAWDS = 185)                                          
      PARAMETER (RMAWDS = 258)                                          
      PARAMETER (PMAWDS = 313)                                          
                                                                        
      PARAMETER (IMGDAY = 368)                                          
      PARAMETER (IMGTM  = 369)                                          
      PARAMETER (IMGSND = 370)                                          
                                                                        
C THESE FOUR WORDS WERE ADDED 5-26-94 TO COMPLY W/ THE NEW ELUG         
C NUMBERING STARTED AT 380 BECAUSE THESE SAME PARAMETERS ARE USED       
C IN THE NAV MESSAGE SENT FROM THE INGESTOR TO EVX, AND WE HAD TO       
C START SOMEWHERE AFTER THE 378 NAV PARAMETERS                          
      PARAMETER (IOFNC = 380)                                           
      PARAMETER (IOFEC = 381)                                           
      PARAMETER (IOFNI = 382)                                           
      PARAMETER (IOFEI = 383)
                                                                        
C========================= END DIGVRCOD ===========================     
C     INCLUDE 'MXCDSZ.INC'
C=========================== DIMXCDSZ =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE NAVIGATION SUBSYSTEM             
C $                                                                     
C $   DESCRIPTION:                                                      
C $      THIS IS THE VALUE OF LARGEST NAVIGATION CODICIL SIZE ALLOWED   
C $                                                                     
C $$  DIMXCDSZ = INCLUDE, NAVIGATION                                    
                                                                        
C DEFINITIONS OF PARAMETERS                                             
C MXCDSZ   : LARGEST ALLOWED CODICIL SIZE                               
                                                                        
      INTEGER      MXCDSZ                                               
                                                                        
      PARAMETER (MXCDSZ = 5*128)                                        
                                                                        
C========================= END DIMXCDSZ ===========================     
C     INCLUDE 'OASET.INC'
C=========================== DIOASET =============================      
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE GVAR NAVIGATION SUBSYSTEM.       
C $                                                                     
C $   DESCRIPTION:                                                      
C $      THESE ARE CONSTANTS USED TO DESCRIBE THE GVAR O&A SET STRUCTURE
C $                                                                     
C $$  DIOASET = INCLUDE, NAVIGATION, GVAR                               
                                                                        
C DEFINITIONS OF PARAMETERS                                             
C OASIZE   : SIZE OF GVAR ORBIT AND ATTITUDE SET                        
C PCOEFS   : START OF PITCH COEFFICIENTS                                
C RMACFS   : START OF RMA COEFFICIENTS                                  
C CUTOF1   : FIRST DIVIDING POINT IN O&A SET (WHERE DIVIDED INTO 128    
C            WORD SETS)                                                 
C CUTOF2   : SECOND DIVIDING POINT IN O&A SET (WHERE DIVIDED INTO 128   
C            WORD SETS)                                                 
                                                                        
      INTEGER      OASIZE, PCOEFS, RMACFS, CUTOF1, CUTOF2               
                                                                        
      PARAMETER (OASIZE = 336)                                          
      PARAMETER (PCOEFS = 117)                                          
      PARAMETER (RMACFS = 227)                                          
      PARAMETER (CUTOF1 = 116)                                          
      PARAMETER (CUTOF2 = 226)                                          
                                                                        
C========================= END DIOASET ===========================      
C     INCLUDE 'B11DOC.INC'
C=========================== DIB11DOC =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE DECODER SCHEDULER SUBSYSTEM.     
C $                                                                     
C $   DESCRIPTION:                                                      
C $      THESE ARE INDICES INTO THE SOUNDER DOCUMENTATION BLOCK         
C $      (INDICES ARE 1-BASED FROM START OF SAD ID)                     
C $                                                                     
C $$  DIB11DOC = INCLUDE, DECODER                                       
                                                                        
C DEFINITIONS OF PARAMETERS                                             
C     SSCAN   SOUNDER SCAN STATUS IN TWO BYTES                          
C     SGBCT   TOTAL NUMBER OF BLOCK 11'S IN THIS SCAN (INCLUDES DOC)    
C     TSCLS   TIME OF SCAN LINE START IN 8 BYTES                        
C     NEWIMG  BIT # IN SSCAN FOR NEW IMAGE FLAG                         
C     ENDIMG  BIT # IN SSCAN FOR END OF IMAGE FLAG                      
C     WARN    BIT # IN SSCAN FOR WARNING MODE FLAG                      
C     DWELL1  BIT # IN SSCAN FOR DWELL MODE 1 FLAG                      
C     DWELL2  BIT # IN SSCAN FOR DWELL MODE 2 FLAG                      
C     DWELL4  BIT # IN SSCAN FOR DWELL MODE 4 FLAG                      
C     IMCFLG  BIT # IN SSCAN FOR IMC ACTIVE FLAG                        
C     STPLOC  BIT # IN SSCAN FOR STEP MODE FLAG                         
C     TIMSIZ  NUMBER OF BYTES THAT THE TIME OCCUPIES                    
C     SNSLN   NORTHERN MOST SCAN LINE OF CURRENT SCAN                   
C     SWFPX   WESTERNMOST PIXEL # OF CURRENT FRAME                      
C     SEFPX   EASTERNMOST PIXEL # OF CURRENT FRAME                      
C     SNFLN   NORTHERNMOST SCAN LINE # OF CURRENT FRAME                 
C     SSFLN   SOUTHERNMOST SCAN LINE # OF CURRENT FRAME                 
C     NAVLOC  START OF O&A PARAMETERS                                   
C     NAVEND  END OF O&A PARAMETERS                                     
                                                                        
      INTEGER      SSCAN , SGBCT , TSCLS , NEWIMG, ENDIMG, TIMSIZ       
      INTEGER      SWFPX , SEFPX , SNFLN , SSFLN , SDSTA , DWELL1       
      INTEGER      NAVLOC, DWELL2, DWELL4, SNSLN , WARN  , STPLOC       
      INTEGER      NAVEND, IMCFLG                                       
                                                                        
C LOCATION OF FIELDS WITHIN DOCUMENTATION BLOCK                         
      PARAMETER (SSCAN  = 31)                                           
      PARAMETER (SDSTA  = 33)                                           
      PARAMETER (SGBCT  = 45)                                           
      PARAMETER (TSCLS  = 67)                                           
      PARAMETER (NEWIMG = 15)                                           
      PARAMETER (ENDIMG = 14)                                           
      PARAMETER (WARN   = 11)                                           
      PARAMETER (STPLOC = 3)                                            
      PARAMETER (DWELL1 = 4)                                            
      PARAMETER (DWELL2 = 5)                                            
      PARAMETER (DWELL4 = 6)                                            
      PARAMETER (IMCFLG = 7)                                            
      PARAMETER (TIMSIZ = 8)                                            
      PARAMETER (SNSLN  = 185)                                          
      PARAMETER (SWFPX  = 187)                                          
      PARAMETER (SEFPX  = 189)                                          
      PARAMETER (SNFLN  = 191)                                          
      PARAMETER (SSFLN  = 193)                                          
      PARAMETER (NAVLOC = 307)                                          
      PARAMETER (NAVEND = 1718)                                         
                                                                        
C========================= END DIB11DOC ===========================     
                                                                        
      REAL*8       TE    , PHI   , PSI   , U     , SINU  , COSU         
      REAL*8       SINOI , COSOI , SLAT  , ASC   , SINASC, COSASC       
      REAL*8       SYAW  , W     , SW    , CW    , S2W   , C2W          
      REAL*8       SW1   , CW1   , SW3   , CW3   , GATT
      REAL*8       SUBLAT, SUBLON, SECS  , WA
      REAL         RPARMS(MXCDSZ)
      REAL*8       IMGTIM, EPOCH , TIMEX , TIME50, R     , B(3,3)       
      REAL*8       AEC   , TS    , DR    , LAM   , DLAT  , DYAW         
      REAL*8       AEBE2C, AEBE3C, AEBE4C, FERC
      INTEGER      IFUNC , LIT   , ITYPE , IMC   , YEAR  , DAY          
      INTEGER      IFTOK , HOUR  , MINUTS, INSTR , IPARMS(MXCDSZ)
      INTEGER      COUNT , OFFSET, LOOP  , IPARM2(MXCDSZ), TIME(2)      
      LOGICAL      BTEST                                                
      CHARACTER*4  CLIT
      CHARACTER*12 CFZ
                                                                        
      EQUIVALENCE(IPARM2,RPARMS)
                                                                        
      COMMON /GVRCOM/ ITYPE , INSTR , SUBLAT, SUBLON                    
      COMMON /RADCOM/ AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C            
      COMMON /SAVCOM/ B     , DR    , PHI                               
                                                                        
C     INCLUDE 'GVRDAT.INC'
C=========================== DIGVRDAT =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE GVAR NAVIGATION SUBSYSTEM.       
C $                                                                     
C $   DESCRIPTION:                                                      
C $      THESE ARE DATA STATEMENTS USED IN THE GVAR NAVIGATION          
C $                                                                     
C $$  DIGVRDAT = INCLUDE, NAVIGATION, GVAR                              
                                                                        
C DEFINITIONS OF PARAMETERS                                             
C RELLST   : TWO DIMENSIONAL ARRAY OF LOCATION OF REALS IN O&A SET      
C INTLST   : ARRAY OF LOCATION OF INTEGERS IN O&A SET                   
                                                                        
      INTEGER      I     , J     , RELLST(40,2), INTLST(55)             
                                                                        
      DATA ((RELLST(I,J),J=1,2),I=1,31) / 5,11,14,64,66,95,99,101,104,  
     &        106,109,111,114,116,117,119,121,150,154,156,159,161,164,  
     &        166,169,171,172,174,176,205,209,211,214,216,219,221,224,  
     &        226,227,229,231,260,264,266,269,271,274,276,279,284,286,  
     &        315,319,321,324,326,329,331,334,336,-1,-1 /               
                                                                        
      DATA (INTLST(I),I=1,55) / 1,2,12,13,65,96,97,98,102,103,107,108,
     &    112,113,120,151,152,153,157,158,162,163,167,168,175,206,207,  
     &    208,212,213,217,218,222,223,230,261,262,263,267,268,272,273,  
     &    277,278,285,316,317,318,322,323,327,328,332,333,-1 /          
                                                                        
C========================= END DIGVRDAT ===========================     
                                                                        
C--------------------------- NVXGVR ------------------------------      

C INITIALIZE THE EARTH RADIUS CONSTANTS                                 
      AEC    = AE                                                       
      FERC   = FER                                                      
      AEBE2C = DBLE(AEBE2)                                              
      AEBE3C = DBLE(AEBE3)                                              
      AEBE4C = DBLE(AEBE4)                                              
                                                                        
C IFUNC = 1                                                             
      IF (IFUNC.EQ.1) THEN                                              
        IF (IPARMS(STTYPE).NE.LIT('GVAR')) THEN                         
          NVXINIGVAR = -1                                                   
          RETURN                                                        
        ENDIF                                                           
        ITYPE = 1                                                       
                                                                        
C COPY CODICIL SO WE CAN EQUIVALENCE IT TO REAL ARRAY                   
        DO 1 LOOP = 1, MXCDSZ                                           
 1        IPARM2(LOOP) = IPARMS(LOOP)                                   
                                                                        
C SCALE DOWN REAL CODICIL ELEMENTS                                      
        COUNT = 1                                                       
        RPARMS(IMGTM) = REAL(IPARM2(IMGTM))/1000.                       
 2      IF (RELLST(COUNT,1).EQ.-1) GOTO 4                               
          OFFSET = 1                                                    
          IF (RELLST(COUNT,1).GT.CUTOF1) OFFSET = 13                    
          IF (RELLST(COUNT,1).GT.CUTOF2) OFFSET = 31                    
          DO 3 LOOP=RELLST(COUNT,1),RELLST(COUNT,2)                     
            IF (LOOP.EQ.14.OR.LOOP.EQ.61.OR.(MOD(LOOP-8,55).EQ.0.AND.   
     &                                               LOOP.NE.8)) THEN   
              RPARMS(LOOP+OFFSET) = REAL(IPARM2(LOOP+OFFSET))/100.      
            ELSE                                                        
              RPARMS(LOOP+OFFSET) = REAL(IPARM2(LOOP+OFFSET))/10000000. 
            ENDIF                                                       
 3        CONTINUE                                                      
          COUNT = COUNT + 1                                             
        GOTO 2                                                          
                                                                        
C SEE IF THIS CODICIL IS FOR IMAGER OR SOUNDER                          
 4      INSTR = IPARMS(IMGSND)                                          

C INITIALIZE CONSTANTS IN COMMON INSTCO                                 
        CALL SETCON(INSTR, IPARMS(IOFNC), IPARMS(IOFNI), IPARMS(IOFEC),
     &                                                  IPARMS(IOFEI))
                                                                        
C GET CONTROL INFO FROM CODICIL                                         
        YEAR    = 1900 + IPARMS(IMGDAY) / 1000                          
        DAY     = IPARMS(IMGDAY) - IPARMS(IMGDAY) / 1000 * 1000         
        HOUR    = RPARMS(IMGTM) / 10000                                 
        MINUTS  = RPARMS(IMGTM) / 100 - HOUR * 100                      
        SECS    = RPARMS(IMGTM) - REAL(100*MINUTS) - REAL(10000*HOUR)   
        IMGTIM  = TIMEX(YEAR, DAY, HOUR, MINUTS, SECS)                  
        TIME(1) = IFTOK(CFZ(IPARMS(ETIME)))
        TIME(2) = IFTOK(CFZ(IPARMS(ETIME+1)))
        EPOCH   = TIME50(TIME)                                          
        IMC     = 1
        IF (BTEST(IPARMS(IMCACT),IMCFLG)) IMC = 0
                                                                        
C ASSIGN REFERENCE VALUES TO THE SUBSATELLITE LONGITUDE AND             
C LATITUDE, THE RADIAL DISTANCE AND THE ORBIT YAW.
        LAM = RPARMS(REFLON)                                            
        DR  = RPARMS(REFDIS)                                            
        PHI = RPARMS(REFLAT)                                            
        PSI = RPARMS(REFYAW)                                            
                                                                        
C ASSIGN REFERENCE VALUES TO THE ATTITUDES AND MISALIGNMENTS            
        ROLL  = RPARMS(RATROL)                                          
        PITCH = RPARMS(RATPTC)                                          
        YAW   = RPARMS(RATYAW)                                          
        RMA   = 0.                                                      
        PMA   = 0.                                                      
                                                                        
C IF IMC IS OFF, COMPUTE CHANGES IN THE SATELLITE ORBIT                 
        IF (IMC.NE.0) THEN                                              

C SET REFERENCE RADIAL DISTANCE, LATITUDE AND ORBIT YAW TO ZERO
          DR  = 0.
          PHI = 0.
          PSI = 0.

C COMPUTE TIME SINCE EPOCH (IN MINUTES)                                 
          TS = IMGTIM - EPOCH                                           
                                                                        
C COMPUTES ORBIT ANGLE AND THE RELATED TRIGONOMETRIC FUNKTIONS.         
C EARTH ROTATIONAL RATE=.729115E-4 (RAD/S)                              
          W   = 0.729115D - 4 * 60.0D0 * TS                             
          SW  = DSIN(W)                                                  
          CW  = DCOS(W)                                                  
          SW1 = DSIN(0.927*W)                                            
          CW1 = DCOS(0.927*W)                                            
          S2W = DSIN(2.*W)                                               
          C2W = DCOS(2.*W)                                               
          SW3 = DSIN(1.9268*W)                                           
          CW3 = DCOS(1.9268*W)                                           

C COMPUTES CHANGE IN THE IMC LONGITUDE FROM THE REFERENCE               
          LAM = LAM + RPARMS(LDR1) + (RPARMS(LDR2) + RPARMS(LDR3)*W) * W
     &        + (RPARMS(LDR10)*SW1 + RPARMS(LDR11)*CW1 + RPARMS(LDR4)*SW
     &        + RPARMS(LDR5)*CW + RPARMS(LDR6)*S2W + RPARMS(LDR7)*C2W   
     &        + RPARMS(LDR8)*SW3+RPARMS(LDR9)*CW3 + W*(RPARMS(LDR12)*SW 
     &        + RPARMS(LDR13)*CW))*2.                                   
                                                                        
C COMPUTES CHANGE IN RADIAL DISTANCE FROM THE REFERENCE (KM)            
          DR = DR + RPARMS(RDDR1) + RPARMS(RDDR2)*CW + RPARMS(RDDR3)*SW 
     &          + RPARMS(RDDR4)*C2W + RPARMS(RDDR5)*S2W + RPARMS(RDDR6) 
     &          * CW3+RPARMS(RDDR7)*SW3 + RPARMS(RDDR8)*CW1             
     &          + RPARMS(RDDR9)*SW1 + W*(RPARMS(RDDR10)*CW              
     &          + RPARMS(RDDR11)*SW)                                    
                                                                        
C COMPUTES THE SINE OF THE CHANGE IN THE GEOCENTRIC LATITUDE            
          DLAT = RPARMS(DGL1) + RPARMS(DGL2)*CW + RPARMS(DGL3)*SW       
     &        + RPARMS(DGL4)*C2W + RPARMS(DGL5)*S2W + W*(RPARMS(DGL6)*CW
     &        + RPARMS(DGL7)*SW) + RPARMS(DGL8)*CW1+RPARMS(DGL9)*SW1    
                                                                        
C COMPUTES GEOCENTRIC LATITUDE BY USING AN EXPANSION FOR ARCSINE        
          PHI = PHI + DLAT * (1. + DLAT * DLAT / 6.)                    
                                                                        
C COMPUTES SINE OF THE CHANGE IN THE ORBIT YAW                          
          DYAW = RPARMS(DOY1) + RPARMS(DOY2)*SW + RPARMS(DOY3)*CW       
     &         + RPARMS(DOY4)*S2W + RPARMS(DOY5)*C2W                    
     &         + W*(RPARMS(DOY6)*SW + RPARMS(DOY7)*CW)                  
     &         + RPARMS(DOY8)*SW1 + RPARMS(DOY9)*CW1                    
                                                                        
C COMPUTES THE ORBIT YAW BY USING AN EXPANSION FOR ARCSINE.             
          PSI = PSI + DYAW * (1. + DYAW * DYAW / 6.)                    
                                                                        
C CALCULATION OF CHANGES IN THE SATELLITE ORBIT ENDS HERE               
        ENDIF                                                           

C CONVERSION OF THE IMC LONGITUDE AND ORBIT YAW TO THE SUBSATELLITE     
C LONGITUDE AND THE ORBIT INCLINATION (REF: GOES-PCC-TM-2473, INPUTS    
C REQUIRED FOR EARTH LOCATION AND GRIDDING BY SPS, JUNE 6, 1988)
        SLAT  = DSIN(PHI)
        SYAW  = DSIN(PSI)                                                
        SINOI = SLAT**2 + SYAW**2                                       
        COSOI = DSQRT(1.-SINOI)                                          
        SINOI = DSQRT(SINOI)

        IF (SLAT .EQ. 0.0D0 .AND. SYAW .EQ. 0.0D0) THEN
          U = 0.0D0
        ELSE
          U = DATAN2(SLAT,SYAW)
        ENDIF

        SINU  = DSIN(U)                                                  
        COSU  = DCOS(U)                                                  
                                                                        
C COMPUTES LONGITUDE OF THE ASCENDING NODE                              
        ASC    = LAM-U                                                  
        SINASC = DSIN(ASC)                                               
        COSASC = DCOS(ASC)                                               
                                                                        
C COMPUTES THE SUBSATELLITE GEOGRAPHIC LATITUDE                         
        SUBLAT = DATAN(AEBE2C*DTAN(PHI))                                  
                                                                        
C COMPUTES THE SUBSATELLITE LONGITUDE                                   
        SUBLON = ASC+DATAN2(COSOI*SINU,COSU)                             
                                                                        
C COMPUTES THE SPACECRAFT TO EARTH FIXED COORDINATES TRANSFORMATION     
C MATRIX:                                                               
C     (VECTOR IN ECEF COORDINATES) = B * (VECTOR IN S/C COORDINATES)    
        B(1,2) = -SINASC*SINOI                                          
        B(2,2) =  COSASC*SINOI                                          
        B(3,2) = -COSOI                                                 
        B(1,3) = -COSASC*COSU+SINASC*SINU*COSOI                         
        B(2,3) = -SINASC*COSU-COSASC*SINU*COSOI                         
        B(3,3) = -SLAT                                                  
        B(1,1) = -COSASC*SINU-SINASC*COSU*COSOI                         
        B(2,1) = -SINASC*SINU+COSASC*COSU*COSOI                         
        B(3,1) =  COSU*SINOI                                            
                                                                        
C COMPUTES THE NORMALIZED SPACECRAFT POSITION VECTOR IN EARTH FIXED     
C COORDINATES - XS.                                                     
        R     = (NOMORB+DR)/AEC                                         
        XS(1) = -B(1,3)*R                                               
        XS(2) = -B(2,3)*R                                               
        XS(3) = -B(3,3)*R                                               
                                                                        
C PRECOMPUTES Q3 (USED IN LPOINT)                                       
        Q3 = XS(1)**2 + XS(2)**2 + AEBE2C * XS(3)**2 - 1.0              
                                                                        
C COMPUTES THE ATTITUDES AND MISALIGNMENTS IF IMC IS OFF                
        IF (IMC.NE.0) THEN                                              
                                                                        
C COMPUTES THE SOLAR ORBIT ANGLE                                        
          WA = RPARMS(61)*TS                                            
                                                                        
C COMPUTES THE DIFFERENCE BETWEEN CURRENT TIME, TS, AND THE             
C EXPONENTIAL TIME, IPARMS(62). NOTE THAT BOTH TIMES ARE SINCE EPOCH.   
          TE = TS - RPARMS(EXPTIM)

C COMPUTES ROLL + ROLL MISALIGNMENT                                     
          ROLL = ROLL + GATT(RAAWDS,RPARMS,IPARMS,WA,TE)
                                                                        
C COMPUTES PITCH + PITCH MISALIGNMENT                                   
          PITCH = PITCH + GATT(PAAWDS,RPARMS,IPARMS,WA,TE)              
                                                                        
C COMPUTES YAW                                                          
          YAW = YAW + GATT(YAAWDS,RPARMS,IPARMS,WA,TE)                  
                                                                        
C COMPUTES ROLL MISALIGNMENT                                            
          RMA = GATT(RMAWDS,RPARMS,IPARMS,WA,TE)                        
                                                                        
C COMPUTES PITCH MISALIGNMENT                                           
          PMA = GATT(PMAWDS,RPARMS,IPARMS,WA,TE)                        
                                                                        
C APPLY THE EARTH SENSOR COMPENSATION IF NEEDED                         
          ROLL   = ROLL + RPARMS(IMCROL)
          PITCH  = PITCH + RPARMS(IMCPTC)
          YAW    = YAW + RPARMS(IMCYAW)
        ENDIF                                                           
                                                                        
C COMPUTES THE INSTRUMENT TO EARTH FIXED COORDINATES TRANSFORMATION     
C MATRIX - BT                                                           
        CALL INST2E(ROLL,PITCH,YAW,B,BT)                                
                                                                        
C IFUNC = 2                                                             
      ELSEIF (IFUNC.EQ.2) THEN                                          
c        IF (INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=1                   
c        IF (INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=2                   
      ENDIF                                                             
      NVXINIGVAR = 0                                                          
      RETURN                                                            
      END                                                               

                                                                
                                                                        
                                                                        
      INTEGER FUNCTION NVXSAEGVAR(XLIN,XELE,XDUM,XLAT,XLON,Z)               
                                                                        
      IMPLICIT NONE
                                                                        
      REAL         XLIN  , XELE  , XDUM  , XLAT  , XLON  , Z            
      REAL*8       RL    , RP    , E     , S
      REAL         YLAT  , YLON
      REAL*8       SUBLAT, SUBLON, EVLN  , SCPX  , TMPLAT, TMPLON
      INTEGER      INSTR , ITYPE , STAT                                 
                                                                        
C     INCLUDE 'ELCONS.INC'
C=========================== DIELCONS =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCONS = INCLUDE                                                
                                                                        
      REAL*8 PI                                                         
           PARAMETER (PI=3.141592653589793D0)                           
      REAL*8 DEG                                                        
           PARAMETER (DEG=180.D0/PI)                                    
      REAL*8 RAD                                                        
           PARAMETER (RAD=PI/180.D0)                                    
C                    DEGREES TO RADIANS CONVERSION PI/180               
      REAL*8 NOMORB                                                     
           PARAMETER (NOMORB=42164.365D0)                               
C                    NOMINAL RADIAL DISTANCE OF SATELLITE (KM)          
      REAL*8 AE                                                         
           PARAMETER (AE=6378.137D0)                                    
C                    EARTH EQUATORIAL RADIUS (KM)                       
      REAL*8 FER                                                        
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                        
C                    EARTH FLATTENING COEFFICIENT = 1-(BE/AE)           
      REAL*4 AEBE2                                                      
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                         
      REAL*4 AEBE3                                                      
           PARAMETER (AEBE3=AEBE2-1.)                                   
      REAL*4 AEBE4                                                      
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                           
C========================= END DIELCONS ===========================     
C     INCLUDE 'ELCOMM.INC'
C=========================== DIELCOMM =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCOMM = INCLUDE                                                
                                                                        
C     COMMON VARIABLES                                                  
C                                                                       
      REAL*8 XS(3)                                                      
C                      NORMALIZED S/C POSITION IN ECEF COORDINATES      
      REAL*8 BT(3,3)                                                    
C                      ECEF TO INSTRUMENT COORDINATES TRANSFORMATION    
      REAL*8  Q3                                                        
C                      USED IN SUBRTN LPOINT                        
      REAL*8 PITCH,ROLL,YAW                                             
C                          PITCH,ROLL,YAW ANGLES OF INSTRUMENT (RAD)    
      REAL*4 PMA,RMA                                                    
C                          PITCH,ROLL MISALIGNMENTS OF INSTRUMENT (RAD) 
         COMMON /ELCOMM/ XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                
C========================= END DIELCOMM ===========================     
C     INCLUDE 'OASET.INC'
C=========================== DIOASET =============================      
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE GVAR NAVIGATION SUBSYSTEM.       
C $                                                                     
C $   DESCRIPTION:                                                      
C $      THESE ARE CONSTANTS USED TO DESCRIBE THE GVAR O&A SET STRUCTURE
C $                                                                     
C $$  DIOASET = INCLUDE, NAVIGATION, GVAR                               
                                                                        
C DEFINITIONS OF PARAMETERS                                             
C OASIZE   : SIZE OF GVAR ORBIT AND ATTITUDE SET                        
C PCOEFS   : START OF PITCH COEFFICIENTS                                
C RMACFS   : START OF RMA COEFFICIENTS                                  
C CUTOF1   : FIRST DIVIDING POINT IN O&A SET (WHERE DIVIDED INTO 128    
C            WORD SETS)                                                 
C CUTOF2   : SECOND DIVIDING POINT IN O&A SET (WHERE DIVIDED INTO 128   
C            WORD SETS)                                                 
                                                                        
      INTEGER      OASIZE, PCOEFS, RMACFS, CUTOF1, CUTOF2               
                                                                        
      PARAMETER (OASIZE = 336)                                          
      PARAMETER (PCOEFS = 117)                                          
      PARAMETER (RMACFS = 227)                                          
      PARAMETER (CUTOF1 = 116)                                          
      PARAMETER (CUTOF2 = 226)                                          
                                                                        
C========================= END DIOASET ===========================      
C     INCLUDE 'INSTCO.INC'
C=========================== DIINSTCO =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIINSTCO = INCLUDE                                                
                                                                        
      INTEGER*4 INCMAX(2)                                               
C                       NUMBER OF INCREMENTS PER CYCLE                  
      REAL*4 ELVMAX(2)                                                  
C                       BOUNDS IN ELEVATION (RADIANS)                   
      REAL*4 SCNMAX(2)                                                  
C                       BOUNDS IN SCAN ANGLE (RADIANS)                  
      REAL*4 ELVINC(2)                                                  
C                       CHANGE IN ELEVATION ANGLE PER INCREMENT (RAD)   
      REAL*4 SCNINC(2)                                                  
C                       CHANGE IN SCAN ANGLE PER INCREMENT (RADIANS)    
      REAL*4 ELVLN(2)                                                   
C                       ELEVATION ANGLE PER DETECTOR LINE (RADIANS)     
      REAL*4 SCNPX(2)                                                   
C                       SCAN ANGLE PER PIXEL (RADIANS)                  
                                                                        
      COMMON /INSTCO/ INCMAX,ELVMAX,SCNMAX,                             
     1   ELVINC,SCNINC,ELVLN,SCNPX                                      
C========================= END DIINSTCO ===========================     
                                                                        
      COMMON /GVRCOM/ ITYPE, INSTR , SUBLAT, SUBLON                     
                                                                        
C--------------------------------------------------------------         
                                                                        
C TRANSFORM LINE/PIXEL TO GEOGRAPHIC COORDINATES:                       
C SET INPUT LINE/PIXEL NUMBERS                                          
      RL = XLIN                                                         
      RP = XELE

C IF DOING SOUNDER NAV, HAVE TO TRICK ROUTINES INTO THINKING IMAGE IS
C AT RES 1, BECAUSE NAV ROUTINES TAKE SOUNDER RES INTO ACCOUNT
      IF (INSTR.EQ.2) THEN
        RL = (RL+9.)/10.
        RP = (RP+9.)/10.
      ENDIF
                                                                        
C COMPUTE ELEVATION AND SCAN ANGLES (E,S) RELATED TO INPUT              
C LINE AND PIXEL NUMBERS                                                
      E = EVLN(INSTR,RL)                                                
      S = SCPX(INSTR,RP)                                                

C TRANSFORM ELEVATION AND SCAN ANGLES TO GEOGRAPHIC COORDINATES         
c     write(*,*), 'Before LPOINT: Rl, Rp, E, X = ',RL,RP,E,S
      CALL LPOINT(E,S,TMPLAT,TMPLON,STAT)                                   
      IF (STAT.NE.0) GOTO 900                                           
      TMPLAT = TMPLAT * DEG                                                 
      TMPLON = TMPLON * DEG                                                 
                                                                        
C PUT LONGITUDE INTO MCIDAS FORM                                        
      TMPLON = -TMPLON                                                      
                                                                        
C SEE IF WE HAVE TO CONVERT TO X Y Z COORDINATES                        
      IF (ITYPE.EQ.2) THEN                                              
        YLAT = TMPLAT                                                     
        YLON = TMPLON                                                     
c        CALL LLCART(YLAT,YLON,XLAT,XLON,Z)                              
      ELSE
	XLAT = TMPLAT
	XLON = TMPLON
      ENDIF                                                             
                                                                        
      NVXSAEGVAR = 0                                                          
      RETURN                                                            
                                                                        
 900  CONTINUE                                                          
      XLAT = TMPLAT
      XLON = -TMPLON
      NVXSAEGVAR = -1                                                         
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
                                                                        
      INTEGER FUNCTION NVXEASGVAR(ZLAT,ZLON,Z,XLIN,XELE,XDUM)               
                                                                        
      IMPLICIT NONE
                                                                        
      REAL         ZLAT  , ZLON  , Z     , XLIN  , XELE  , XDUM         
      REAL         X     , Y            
      REAL*8       TMPLIN, TMPELE, E     , S     , TMPLAT, TMPLON
      REAL*8       SUBLAT, SUBLON
      INTEGER      INSTR , ITYPE , IER                                  
                                                                        
C     INCLUDE 'ELCONS.INC'
C=========================== DIELCONS =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCONS = INCLUDE                                                
                                                                        
      REAL*8 PI                                                         
           PARAMETER (PI=3.141592653589793D0)                           
      REAL*8 DEG                                                        
           PARAMETER (DEG=180.D0/PI)                                    
      REAL*8 RAD                                                        
           PARAMETER (RAD=PI/180.D0)                                    
C                    DEGREES TO RADIANS CONVERSION PI/180               
      REAL*8 NOMORB                                                     
           PARAMETER (NOMORB=42164.365D0)                               
C                    NOMINAL RADIAL DISTANCE OF SATELLITE (KM)          
      REAL*8 AE                                                         
           PARAMETER (AE=6378.137D0)                                    
C                    EARTH EQUATORIAL RADIUS (KM)                       
      REAL*8 FER                                                        
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                        
C                    EARTH FLATTENING COEFFICIENT = 1-(BE/AE)           
      REAL*4 AEBE2                                                      
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                         
      REAL*4 AEBE3                                                      
           PARAMETER (AEBE3=AEBE2-1.)                                   
      REAL*4 AEBE4                                                      
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                           
C========================= END DIELCONS ===========================     
C     INCLUDE 'ELCOMM.INC'
C=========================== DIELCOMM =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCOMM = INCLUDE                                                
                                                                        
C     COMMON VARIABLES                                                  
C                                                                       
      REAL*8 XS(3)                                                      
C                      NORMALIZED S/C POSITION IN ECEF COORDINATES      
      REAL*8 BT(3,3)                                                    
C                      ECEF TO INSTRUMENT COORDINATES TRANSFORMATION    
      REAL*8  Q3                                                        
C                      USED IN SUBRTN LPOINT                        
      REAL*8 PITCH,ROLL,YAW                                             
C                          PITCH,ROLL,YAW ANGLES OF INSTRUMENT (RAD)    
      REAL*4 PMA,RMA                                                    
C                          PITCH,ROLL MISALIGNMENTS OF INSTRUMENT (RAD) 
         COMMON /ELCOMM/ XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                
C========================= END DIELCOMM ===========================     
C     INCLUDE 'OASET.INC'
C=========================== DIOASET =============================      
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE GVAR NAVIGATION SUBSYSTEM.       
C $                                                                     
C $   DESCRIPTION:                                                      
C $      THESE ARE CONSTANTS USED TO DESCRIBE THE GVAR O&A SET STRUCTURE
C $                                                                     
C $$  DIOASET = INCLUDE, NAVIGATION, GVAR                               
                                                                        
C DEFINITIONS OF PARAMETERS                                             
C OASIZE   : SIZE OF GVAR ORBIT AND ATTITUDE SET                        
C PCOEFS   : START OF PITCH COEFFICIENTS                                
C RMACFS   : START OF RMA COEFFICIENTS                                  
C CUTOF1   : FIRST DIVIDING POINT IN O&A SET (WHERE DIVIDED INTO 128    
C            WORD SETS)                                                 
C CUTOF2   : SECOND DIVIDING POINT IN O&A SET (WHERE DIVIDED INTO 128   
C            WORD SETS)                                                 
                                                                        
      INTEGER      OASIZE, PCOEFS, RMACFS, CUTOF1, CUTOF2               
                                                                        
      PARAMETER (OASIZE = 336)                                          
      PARAMETER (PCOEFS = 117)                                          
      PARAMETER (RMACFS = 227)                                          
      PARAMETER (CUTOF1 = 116)                                          
      PARAMETER (CUTOF2 = 226)                                          
                                                                        
C========================= END DIOASET ===========================      
C     INCLUDE 'INSTCO.INC'
C=========================== DIINSTCO =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIINSTCO = INCLUDE                                                
                                                                        
      INTEGER*4 INCMAX(2)                                               
C                       NUMBER OF INCREMENTS PER CYCLE                  
      REAL*4 ELVMAX(2)                                                  
C                       BOUNDS IN ELEVATION (RADIANS)                   
      REAL*4 SCNMAX(2)                                                  
C                       BOUNDS IN SCAN ANGLE (RADIANS)                  
      REAL*4 ELVINC(2)                                                  
C                       CHANGE IN ELEVATION ANGLE PER INCREMENT (RAD)   
      REAL*4 SCNINC(2)                                                  
C                       CHANGE IN SCAN ANGLE PER INCREMENT (RADIANS)    
      REAL*4 ELVLN(2)                                                   
C                       ELEVATION ANGLE PER DETECTOR LINE (RADIANS)     
      REAL*4 SCNPX(2)                                                   
C                       SCAN ANGLE PER PIXEL (RADIANS)                  
                                                                        
      COMMON /INSTCO/ INCMAX,ELVMAX,SCNMAX,                             
     1   ELVINC,SCNINC,ELVLN,SCNPX                                      
C========================= END DIINSTCO ===========================     
                                                                        
      COMMON /GVRCOM/ ITYPE, INSTR, SUBLAT, SUBLON                      
                                                                        
C--------------------------------------------------------------         
                                                                        
      NVXEASGVAR = 0                                                        
                                                                        
C IF IN CARTESIAN COORDS, TRANSFORM TO LAT/LON                          
      IF (ITYPE.EQ.2) THEN                                              
        X = ZLAT                                                        
        Y = ZLON                                                        
c        CALL CARTLL(X,Y,Z,ZLAT,ZLON)                                    
      ENDIF                                                             
                                                                        
      IF (ABS(ZLAT).GT.90.) THEN                                        
        NVXEASGVAR = -1                                                     
        RETURN                                                          
      ENDIF                                                             

C ELUG ROUTINES USE REAL*8 VARS; USE TEMP VARIABLES 
      TMPLAT = ZLAT 
      TMPLON = ZLON 
                                                                        
C TRANSFORM LAT/LON TO ELEVATION AND SCAN ANGLES                        
      CALL GPOINT(TMPLAT*RAD,-TMPLON*RAD,E,S,IER)
                                                                        
C SEE IF THE LAT/LON GIVEN IS ABLE TO BE SEEN                           
      IF (IER.NE.0) THEN                                                
        NVXEASGVAR = -1                                                     
        RETURN                                                          
      ENDIF                                                             
                                                                        
C CONVERT ELEVATION AND SCAN ANGLES TO LINE/PIXEL COORDINATES           
      CALL EVSC2L(INSTR,E,S,TMPLIN,TMPELE)

C CONVERT INTERNAL 8 BYTE VALUES TO 4 BYTES 
      XLIN = TMPLIN 
      XELE = TMPELE

C IF DOING SOUNDER NAV, CHANGE LIN & ELE RETURNED TO RES 10 VALUES
      IF (INSTR.EQ.2) THEN
        XLIN = XLIN*10.-9.
        XELE = XELE*10.-9.
      ENDIF
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
                                                                        
      INTEGER FUNCTION NVXOPT(IFUNC,XIN,XOUT)                           
                                                                        
      IMPLICIT NONE
                                                                        
C     INCLUDE 'ELCONS.INC'
C=========================== DIELCONS =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCONS = INCLUDE                                                
                                                                        
      REAL*8 PI                                                         
           PARAMETER (PI=3.141592653589793D0)                           
      REAL*8 DEG                                                        
           PARAMETER (DEG=180.D0/PI)                                    
      REAL*8 RAD                                                        
           PARAMETER (RAD=PI/180.D0)                                    
C                    DEGREES TO RADIANS CONVERSION PI/180               
      REAL*8 NOMORB                                                     
           PARAMETER (NOMORB=42164.365D0)                               
C                    NOMINAL RADIAL DISTANCE OF SATELLITE (KM)          
      REAL*8 AE                                                         
           PARAMETER (AE=6378.137D0)                                    
C                    EARTH EQUATORIAL RADIUS (KM)                       
      REAL*8 FER                                                        
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                        
C                    EARTH FLATTENING COEFFICIENT = 1-(BE/AE)           
      REAL*4 AEBE2                                                      
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                         
      REAL*4 AEBE3                                                      
           PARAMETER (AEBE3=AEBE2-1.)                                   
      REAL*4 AEBE4                                                      
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                           
C========================= END DIELCONS ===========================     
C     INCLUDE 'ELCOMM.INC'
C=========================== DIELCOMM =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCOMM = INCLUDE                                                
                                                                        
C     COMMON VARIABLES                                                  
C                                                                       
      REAL*8 XS(3)                                                      
C                      NORMALIZED S/C POSITION IN ECEF COORDINATES      
      REAL*8 BT(3,3)                                                    
C                      ECEF TO INSTRUMENT COORDINATES TRANSFORMATION    
      REAL*8  Q3                                                        
C                      USED IN SUBRTN LPOINT                        
      REAL*8 PITCH,ROLL,YAW                                             
C                          PITCH,ROLL,YAW ANGLES OF INSTRUMENT (RAD)    
      REAL*4 PMA,RMA                                                    
C                          PITCH,ROLL MISALIGNMENTS OF INSTRUMENT (RAD) 
         COMMON /ELCOMM/ XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                
C========================= END DIELCOMM ===========================     
                                                                        
      INTEGER      IFUNC , LIT   , ITYPE , INSTR , JDAY  , JTIME        
      INTEGER      IROUND, ITIME , LASDAY, LASTIM
      REAL         FLAT  , FLON  , GHA   , XIN(*)       
      REAL         DEC   , XLAT  , XLON  , XOUT(*)              
      REAL*8       AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C, B(3,3)       
      REAL*8       SUBLAT, SUBLON, R     , DR    , PHI
                                                                        
      COMMON /GVRCOM/ ITYPE , INSTR , SUBLAT, SUBLON                    
      COMMON /RADCOM/ AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C            
      COMMON /SAVCOM/ B     , DR    , PHI                               
                                                                        
      DATA LASDAY/-1/,LASTIM/-1/                                        
                                                                        
C-----------------------------------------------------------------      
                                                                        
      NVXOPT = -1                                                       
                                                                        
      IF (IFUNC.EQ.LIT('SPOS')) THEN                                    
        XOUT(1) = SUBLAT * DEG                                          
        XOUT(2) = -SUBLON * DEG                                         
        XOUT(2) = AMOD(XOUT(2),360.)                                    
        NVXOPT = 0                                                      
                                                                        
      ELSEIF (IFUNC.EQ.LIT('HGT ')) THEN                                
        AEC    = AE + DBLE(XIN(1))                                      
        FERC   = 1.D0 - ((6356.7533D0 + DBLE(XIN(1))) / AEC)            
        AEBE2C = 1.D0 / ((1.D0 - FERC)*(1.D0 - FERC))                   
        AEBE3C = AEBE2C - 1.D0                                          
        AEBE4C = (1.D0-FERC)*(1.D0-FERC)*(1.D0-FERC)*(1.D0-FERC) - 1.D0 
                                                                        
C RECOMPUTE VALUES THAT DEPEND ON EARTH RADIUS                          
        SUBLAT = DATAN(AEBE2C*DTAN(PHI))
        R      = (NOMORB+DR)/AEC                                        
        XS(1)  = -B(1,3)*R                                              
        XS(2)  = -B(2,3)*R                                              
        XS(3)  = -B(3,3)*R                                              
        Q3     = XS(1)**2 + XS(2)**2 + AEBE2C * XS(3)**2 - 1.0          
        NVXOPT = 0                                                      
                                                                        
      ELSEIF (IFUNC.EQ.LIT('ANG ')) THEN                                
        JDAY  = IROUND(XIN(1))                                          
        JTIME = ITIME(XIN(2))                                           
        FLAT  = XIN(3)                                                  
        FLON  = XIN(4)                                                  
        IF (JDAY.NE.LASDAY.OR.JTIME.NE.LASTIM) THEN                     
          CALL SOLARP(JDAY,JTIME,GHA,DEC,XLAT,XLON)                     
          LASDAY = JDAY                                                 
          LASTIM = JTIME
        ENDIF
        CALL GVRANG(JDAY, JTIME, FLAT, FLON, GHA, DEC, XOUT(1), XOUT(2),
     &                                                          XOUT(3))
        NVXOPT = 0                                                      
      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
C THE FOLLOWING ARE SUBSIDIARY ROUTINES                                 
                                                                        
                                                                        
      SUBROUTINE SETCON(INSTR, NADNSC, NADNSI, NADEWC, NADEWI)
                                                                        
      IMPLICIT NONE

      INTEGER INSTR, NADNSC, NADNSI, NADEWC, NADEWI
      character*12 cfg
                                                                        
C     INCLUDE 'ELCONS.INC'
C=========================== DIELCONS =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCONS = INCLUDE                                                
                                                                        
      REAL*8 PI                                                         
           PARAMETER (PI=3.141592653589793D0)                           
      REAL*8 DEG                                                        
           PARAMETER (DEG=180.D0/PI)                                    
      REAL*8 RAD                                                        
           PARAMETER (RAD=PI/180.D0)                                    
C                    DEGREES TO RADIANS CONVERSION PI/180               
      REAL*8 NOMORB                                                     
           PARAMETER (NOMORB=42164.365D0)                               
C                    NOMINAL RADIAL DISTANCE OF SATELLITE (KM)          
      REAL*8 AE                                                         
           PARAMETER (AE=6378.137D0)                                    
C                    EARTH EQUATORIAL RADIUS (KM)                       
      REAL*8 FER                                                        
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                        
C                    EARTH FLATTENING COEFFICIENT = 1-(BE/AE)           
      REAL*4 AEBE2                                                      
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                         
      REAL*4 AEBE3                                                      
           PARAMETER (AEBE3=AEBE2-1.)                                   
      REAL*4 AEBE4                                                      
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                           
C========================= END DIELCONS ===========================     
C     INCLUDE 'INSTCO.INC'
C=========================== DIINSTCO =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIINSTCO = INCLUDE                                                
                                                                        
      INTEGER*4 INCMAX(2)                                               
C                       NUMBER OF INCREMENTS PER CYCLE                  
      REAL*4 ELVMAX(2)                                                  
C                       BOUNDS IN ELEVATION (RADIANS)                   
      REAL*4 SCNMAX(2)                                                  
C                       BOUNDS IN SCAN ANGLE (RADIANS)                  
      REAL*4 ELVINC(2)                                                  
C                       CHANGE IN ELEVATION ANGLE PER INCREMENT (RAD)   
      REAL*4 SCNINC(2)                                                  
C                       CHANGE IN SCAN ANGLE PER INCREMENT (RADIANS)    
      REAL*4 ELVLN(2)                                                   
C                       ELEVATION ANGLE PER DETECTOR LINE (RADIANS)     
      REAL*4 SCNPX(2)                                                   
C                       SCAN ANGLE PER PIXEL (RADIANS)                  
                                                                        
      COMMON /INSTCO/ INCMAX,ELVMAX,SCNMAX,                             
     1   ELVINC,SCNINC,ELVLN,SCNPX                                      
C========================= END DIINSTCO ===========================     
                                                                        
      INCMAX(1)  = 6136                                                 
      INCMAX(2)  = 2805                                                 
      ELVINC(1)  = 8.E-6                                                
      ELVINC(2)  = 17.5E-6                                              
      SCNINC(1)  = 16.E-6                                               
      SCNINC(2)  = 35.E-6                                               
      ELVLN(1)   = 28.E-6                                               
      ELVLN(2)   = 280.E-6                                              
      SCNPX(1)   = 16.E-6                                               
      SCNPX(2)   = 280.E-6                                              
      ELVMAX(1)  = 0.220896                                             
      ELVMAX(2)  = 0.22089375                                           
      SCNMAX(1)  = 0.24544                                              
      SCNMAX(2)  = 0.2454375                                            

C new code because of change to elug; nadir position is available
C in the signal, so should compute 4 values above using them 
C instead of having them hardwired
c      call ddest('nadnsc: ', nadnsc)
c      call ddest('nadnsi: ', nadnsi)
c      call ddest('nadewc: ', nadewc)
c      call ddest('nadewi: ', nadewi)
c
c--- new code from Kathy Kelly for sounder nav - 10/27
c
      if (nadnsc .ne. 0 .and. nadnsi .ne. 0 .and. nadewc .ne. 0
     &                                 .and. nadewi .ne. 0) then
	IF (INSTR .EQ. 1) THEN

           ELVMAX(INSTR) = (NADNSC*INCMAX(INSTR)+NADNSI)
     &                                             *ELVINC(INSTR)

        ELSE

           ELVMAX(INSTR) = ((9-NADNSC)*INCMAX(INSTR)-NADNSI)
     &                                             *ELVINC(INSTR)

	ENDIF

        SCNMAX(INSTR) = (NADEWC*INCMAX(INSTR)+NADEWI)
     &                                            *SCNINC(INSTR)
      endif
c
c--- end of new code from Kathy Kelly for sounder nav - 10/27
c
c       print *,elvmax(1),elvmax(2),scnmax(1),scnmax(2)
c      call ddest('elvmax(1): '//cfg(elvmax(1)), 0)
c      call ddest('elvmax(2): '//cfg(elvmax(2)), 0)
c      call ddest('scnmax(1): '//cfg(scnmax(1)), 0)
c      call ddest('scnmax(2): '//cfg(scnmax(2)), 0)
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
      FUNCTION TIMEX(NY,ND,NH,NM,S)                                     
                                                                        
      IMPLICIT NONE
                                                                        
      INTEGER      ND    , NH    , NM    , J     , NY                   
      REAL*8       TIMEX , S                                            
                                                                        
C-----------------------------------------------------------------------
                                                                        
      J = ND + 1461 * (NY + 4799) / 4 - 3 * ((NY + 4899) / 100) / 4     
     &                                                        - 2465022 
                                                                        
C COMPUTE ACTUAL TIME IN MINUTES FROM JANUARY 1.0, 1950                 
      TIMEX = J * 1440.D0 + NH * 60.D0 + NM + S / 60.D0                 
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
      FUNCTION TIME50(I)                                                
                                                                        
      IMPLICIT NONE
                                                                        
      INTEGER      NY    , ND    , NH    , NM    , J     , I(2)         
      INTEGER      IAA   , IAB   , NBC   , IAC   , DEF                  
      REAL*8       TIME50, S                                            
                                                                        
C-----------------------------------------------------------------      
                                                                        
      NY  = I(1) / 10000                                                
      IAA = I(1) - (NY*10000)                                           
                                                                        
      ND  = (I(1) - (NY*10000)) * .1                                    
      IAB = (IAA - (ND * 10)) * 10                                      
      NBC = I(2) / 10000000.                                            
      IAC = I(2) - (NBC * 10000000)                                     
      NH  = IAB + NBC                                                   
      DEF = I(2) - IAC                                                  
      NM  = IAC * .00001                                                
      S   = (I(2) - (DEF + (NM * 100000))) * .001                       
                                                                        
C HERE WE CONVERT INTEGER YEAR AND DAY OF YEAR TO NUMBER OF             
C DAYS FROM 0 HOUR UT, 1950 JAN. 1.0                                    
C THIS CONVERTION IS BASED ON AN ALGORITHM BY FLIEGEL AND VAN           
C FLANDERN, COMM. OF ACM, VOL.11, NO. 10, OCT. 1968 (P.657)             
                                                                        
      J = ND + 1461 * (NY + 4799) / 4 - 3 * ((NY + 4899) / 100) / 4     
     &                                                       - 2465022  
                                                                        
C COMPUTE TIME IN MINUTES FROM JANUARY 1.0, 1950                        
      TIME50 = J * 1440.D0 + NH * 60.D0 + NM + S / 60.D0                
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
                                                                        
      REAL*8 FUNCTION GATT(K0,RPARMS,IPARMS,WA,TE)                        
                                                                        
      IMPLICIT NONE

C     INCLUDE 'MXCDSZ.INC'
C=========================== DIMXCDSZ =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE NAVIGATION SUBSYSTEM             
C $                                                                     
C $   DESCRIPTION:                                                      
C $      THIS IS THE VALUE OF LARGEST NAVIGATION CODICIL SIZE ALLOWED   
C $                                                                     
C $$  DIMXCDSZ = INCLUDE, NAVIGATION                                    
                                                                        
C DEFINITIONS OF PARAMETERS                                             
C MXCDSZ   : LARGEST ALLOWED CODICIL SIZE                               
                                                                        
      INTEGER      MXCDSZ                                               
                                                                        
      PARAMETER (MXCDSZ = 5*128)                                        
                                                                        
C========================= END DIMXCDSZ ===========================     
                                                                        
C CALLING PARAMETERS                                                    
      INTEGER K0                                                        
C STARTING POSITION OF A PARAMETER SUBSET IN THE REAL O&A SET           
      REAL*4 RPARMS(MXCDSZ)
C INPUT O&A PARAMETER SET                                               
      INTEGER IPARMS(MXCDSZ)
C INPUT O&A PARAMETER SET                                               
      REAL*8 WA                                                           
C INPUT SOLAR ORBIT ANGLE IN RADIANS                                    
      REAL*8 TE                                                           
C INPUT EXPONENTIAL TIME DELAY FROM EPOCH (MINUTES)                     
                                                                        
C LOCAL VARIABLES                                                       
      INTEGER      KKK   , I     , J     , M     , L     , LL    , K    
      REAL*8       IR    , JR    , MR    , ATT                          
                                                                        
C------------------------------------------------------------------     
                                                                        
C CONSTANT COMPONENT                                                    
      K   = K0                                                          
      ATT = RPARMS(K+2)                                                 
                                                                        
C COMPUTES THE EXPONENTIAL TERM                                         
      IF (TE.GE.0) ATT = ATT + RPARMS(K) * DEXP(-TE / RPARMS(K+1))       
                                                                        
C EXTRACTS THE NUMBER OF SINUSOIDS                                      
      IR = REAL(IPARMS(K+3))
      I  = NINT(IR)                                                     
                                                                        
C CALCULATION OF SINUSOIDS                                              
      DO 10 L=1,I                                                       
        ATT = ATT + RPARMS(K+2*L+2) * DCOS(WA*L+RPARMS(K+2*L+3))         
 10   CONTINUE                                                          
                                                                        
C POINTER TO THE NUMBER OF MONOMIAL SINUSOIDS                           
      K = K + 34                                                        

C EXTACTS NUMBER OF MONOMIAL SINUSOIDS                                  
      IR  = REAL(IPARMS(K))                                             
      KKK = IPARMS(K)                                                   
                                                                        
C COMPUTES MONOMIAL SINUSOIDS                                           
      DO 20 L=1,KKK                                                     
        LL = K + 5 * L                                                  
                                                                        
C ORDER OF SINUSOID                                                     
        JR = REAL(IPARMS(LL-4))                                         
                                                                        
C ORDER OF MONOMIAL SINUSOID                                            
        MR  = REAL(IPARMS(LL-3))                                        
        J   = NINT(JR)                                                  
        M   = NINT(MR)                                                  
        ATT = ATT + RPARMS(LL-2) * ((WA - RPARMS(LL))**MR)              
     &                                      * DCOS(JR*WA+RPARMS(LL-1))  
 20   CONTINUE                                                          
                                                                        
      GATT = ATT                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
                                                                        
      SUBROUTINE EVSC2L(INSTR,ELEV,SCAN,RL,RP)                          
                                                                        
      IMPLICIT NONE
                                                                        
C CALLING PARAMETERS                                                    
      INTEGER INSTR                                                     
C INSTRUMENT CODE (1-IMAGER, 2-SOUNDER)                                 
      REAL*8 ELEV                                                         
C ELEVATION ANGLE IN RADIANS                                            
      REAL*8 SCAN                                                         
C SCAN ANGLE IN RADIANS                                                 
      REAL*8 RL                                                           
C LINE NUMBER                                                           
      REAL*8 RP                                                           
C PIXEL NUMBER                                                          
                                                                        
C     INCLUDE 'INSTCO.INC'
C=========================== DIINSTCO =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIINSTCO = INCLUDE                                                
                                                                        
      INTEGER*4 INCMAX(2)                                               
C                       NUMBER OF INCREMENTS PER CYCLE                  
      REAL*4 ELVMAX(2)                                                  
C                       BOUNDS IN ELEVATION (RADIANS)                   
      REAL*4 SCNMAX(2)                                                  
C                       BOUNDS IN SCAN ANGLE (RADIANS)                  
      REAL*4 ELVINC(2)                                                  
C                       CHANGE IN ELEVATION ANGLE PER INCREMENT (RAD)   
      REAL*4 SCNINC(2)                                                  
C                       CHANGE IN SCAN ANGLE PER INCREMENT (RADIANS)    
      REAL*4 ELVLN(2)                                                   
C                       ELEVATION ANGLE PER DETECTOR LINE (RADIANS)     
      REAL*4 SCNPX(2)                                                   
C                       SCAN ANGLE PER PIXEL (RADIANS)                  
                                                                        
      COMMON /INSTCO/ INCMAX,ELVMAX,SCNMAX,                             
     1   ELVINC,SCNINC,ELVLN,SCNPX                                      
C========================= END DIINSTCO ===========================     
                                                                        
C--------------------------------------------------------------         
                                                                        
C COMPUTE FRACTIONAL LINE NUMBER                                        
      RL = (ELVMAX(INSTR) - ELEV) / ELVLN(INSTR)                        
      IF (INSTR.EQ.1) THEN                                              
        RL = RL + 4.5                                                   
      ELSE                                                              
        RL = RL + 2.5                                                   
      ENDIF                                                             
                                                                        
C COMPUTE FRACTIONAL PIXEL NUMBER                                       
      RP = (SCNMAX(INSTR) + SCAN) / SCNPX(INSTR) + 1.                   
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
      REAL*8 FUNCTION EVLN(INSTR,RLINE)                                   
                                                                        
      IMPLICIT NONE
                                                                        
C CALLING PARAMETERS                                                    
      INTEGER INSTR                                                     
C INSTRUMENT CODE (1-IMAGER, 2-SOUNDER)                                 
      REAL*8  RLINE                                                     
C FRACTIONAL LINE  NUMBER                                               
                                                                        
C     INCLUDE 'INSTCO.INC'
C=========================== DIINSTCO =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIINSTCO = INCLUDE                                                
                                                                        
      INTEGER*4 INCMAX(2)                                               
C                       NUMBER OF INCREMENTS PER CYCLE                  
      REAL*4 ELVMAX(2)                                                  
C                       BOUNDS IN ELEVATION (RADIANS)                   
      REAL*4 SCNMAX(2)                                                  
C                       BOUNDS IN SCAN ANGLE (RADIANS)                  
      REAL*4 ELVINC(2)                                                  
C                       CHANGE IN ELEVATION ANGLE PER INCREMENT (RAD)   
      REAL*4 SCNINC(2)                                                  
C                       CHANGE IN SCAN ANGLE PER INCREMENT (RADIANS)    
      REAL*4 ELVLN(2)                                                   
C                       ELEVATION ANGLE PER DETECTOR LINE (RADIANS)     
      REAL*4 SCNPX(2)                                                   
C                       SCAN ANGLE PER PIXEL (RADIANS)                  
                                                                        
      COMMON /INSTCO/ INCMAX,ELVMAX,SCNMAX,                             
     1   ELVINC,SCNINC,ELVLN,SCNPX                                      
C========================= END DIINSTCO ===========================     
                                                                        
C----------------------------------------------------------------       
                                                                        
      IF (INSTR.EQ.1) THEN                                              
        EVLN = ELVMAX(INSTR) - (RLINE - 4.5) * ELVLN(INSTR)             
      ELSE                                                              
        EVLN = ELVMAX(INSTR) - (RLINE - 2.5) * ELVLN(INSTR)             
      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
      REAL*8 FUNCTION SCPX(INSTR,PIX)                                     
                                                                        
      IMPLICIT NONE
                                                                        
C CALLING PARAMETERS                                                    
      INTEGER INSTR                                                     
C INSTRUMENT CODE (1-IMAGER, 2-SOUNDER)                                 
      REAL*8 PIX                                                          
C FRACTIONAL PIXEL NUMBER                                               
                                                                        
C     INCLUDE 'INSTCO.INC'
C=========================== DIINSTCO =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIINSTCO = INCLUDE                                                
                                                                        
      INTEGER*4 INCMAX(2)                                               
C                       NUMBER OF INCREMENTS PER CYCLE                  
      REAL*4 ELVMAX(2)                                                  
C                       BOUNDS IN ELEVATION (RADIANS)                   
      REAL*4 SCNMAX(2)                                                  
C                       BOUNDS IN SCAN ANGLE (RADIANS)                  
      REAL*4 ELVINC(2)                                                  
C                       CHANGE IN ELEVATION ANGLE PER INCREMENT (RAD)   
      REAL*4 SCNINC(2)                                                  
C                       CHANGE IN SCAN ANGLE PER INCREMENT (RADIANS)    
      REAL*4 ELVLN(2)                                                   
C                       ELEVATION ANGLE PER DETECTOR LINE (RADIANS)     
      REAL*4 SCNPX(2)                                                   
C                       SCAN ANGLE PER PIXEL (RADIANS)                  
                                                                        
      COMMON /INSTCO/ INCMAX,ELVMAX,SCNMAX,                             
     1   ELVINC,SCNINC,ELVLN,SCNPX                                      
C========================= END DIINSTCO ===========================     
                                                                        
C--------------------------------------------------------------------   
                                                                        
      SCPX = (PIX - 1.) * SCNPX(INSTR) - SCNMAX(INSTR)                  
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
                                                                        
      SUBROUTINE INST2E(R,P,Y,A,AT)                                     
                                                                        
      IMPLICIT NONE
                                                                        
C CALLING PARAMETERS                                                    
      REAL*8 R                                                          
C ROLL ANGLE IN RADIANS                                                 
      REAL*8 P                                                          
C PITCH ANGLE IN RADIANS                                                
      REAL*8 Y                                                          
C YAW ANGLE IN RADIANS                                                  
      REAL*8 A(3,3)                                                     
C SPACECRAFT TO ECEF COORDINATES TRANSFORMATION MATRIX                  
      REAL*8 AT(3,3)                                                    
C INSTRUMENT TO ECEF COORDINATES TRANSFORMATION MATRIX                  
                                                                        
      REAL*8       RPY(3,3)                                             
      INTEGER*4    I     , J                                            
                                                                        
C-----------------------------------------------------------------------
                                                                        
C WE COMPUTE INSTRUMENT TO BODY COORDINATES TRANSFORMATION              
C MATRIX BY USING A SMALL ANGLE APPROXIMATION OF TRIGONOMETRIC          
C FUNKTIONS OF THE ROLL, PITCH AND YAW.                                 
      RPY(1,1) = 1. - 0.5 * (P * P + Y * Y)                             
      RPY(1,2) = -Y                                                     
      RPY(1,3) = P                                                      
      RPY(2,1) = Y + P * R                                              
      RPY(2,2) = 1. - 0.5 * (Y * Y + R * R)                             
      RPY(2,3) = -R                                                     
      RPY(3,1) = -P + R * Y                                             
      RPY(3,2) = R + P * Y                                              
      RPY(3,3) = 1. - 0.5 * (P * P + R * R)                             
C MULTIPLICATION OF MATRICES A AND RPY                                  
      DO 20 I=1,3                                                       
        DO 10 J=1,3                                                     
          AT(I,J) = A(I,1) * RPY(1,J) + A(I,2) * RPY(2,J) + A(I,3)      
     &                                                    * RPY(3,J)    
 10     CONTINUE                                                        
 20   CONTINUE                                                          
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
                                                                        
      SUBROUTINE LPOINT(ALPHA,ZETA,RLAT,RLON,IERR)                      
                                                                        
      IMPLICIT NONE
                                                                        
C CALLING PARAMETERS                                                    
      REAL*8   ALPHA                                                    
C ELEVATION ANGLE (RAD)                                                 
      REAL*8   ZETA                                                     
C SCAN ANGLE (RAD)                                                      
      REAL*8   RLAT                                                     
C LATITUDE IN RADIANS (OUTPUT)                                          
      REAL*8   RLON                                                     
C LONGITUDE IN RADIANS (OUTPUT)                                         
      INTEGER IERR                                                      
C OUTPUT STATUS; 0 - POINT ON THE EARTH FOUND,                          
C                1 - INSTRUMENT POINTS OFF EARTH                        
                                                                        
      REAL*8       AEC   , FERC  , Q1    , Q2    , D     , H     , G1(3)
      REAL*8       SA    , CA    , DA    , DZ    , D1    , CZ    , G(3) 
      REAL*8       U(3)                                                 
      REAL*8       AEBE2C, AEBE3C, AEBE4C                               
                                                                        
C     INCLUDE 'ELCONS.INC'
C=========================== DIELCONS =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCONS = INCLUDE                                                
                                                                        
      REAL*8 PI                                                         
           PARAMETER (PI=3.141592653589793D0)                           
      REAL*8 DEG                                                        
           PARAMETER (DEG=180.D0/PI)                                    
      REAL*8 RAD                                                        
           PARAMETER (RAD=PI/180.D0)                                    
C                    DEGREES TO RADIANS CONVERSION PI/180               
      REAL*8 NOMORB                                                     
           PARAMETER (NOMORB=42164.365D0)                               
C                    NOMINAL RADIAL DISTANCE OF SATELLITE (KM)          
      REAL*8 AE                                                         
           PARAMETER (AE=6378.137D0)                                    
C                    EARTH EQUATORIAL RADIUS (KM)                       
      REAL*8 FER                                                        
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                        
C                    EARTH FLATTENING COEFFICIENT = 1-(BE/AE)           
      REAL*4 AEBE2                                                      
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                         
      REAL*4 AEBE3                                                      
           PARAMETER (AEBE3=AEBE2-1.)                                   
      REAL*4 AEBE4                                                      
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                           
C========================= END DIELCONS ===========================     
C     INCLUDE 'ELCOMM.INC'
C=========================== DIELCOMM =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCOMM = INCLUDE                                                
                                                                        
C     COMMON VARIABLES                                                  
C                                                                       
      REAL*8 XS(3)                                                      
C                      NORMALIZED S/C POSITION IN ECEF COORDINATES      
      REAL*8 BT(3,3)                                                    
C                      ECEF TO INSTRUMENT COORDINATES TRANSFORMATION    
      REAL*8  Q3                                                        
C                      USED IN SUBRTN LPOINT                        
      REAL*8 PITCH,ROLL,YAW                                             
C                          PITCH,ROLL,YAW ANGLES OF INSTRUMENT (RAD)    
      REAL*4 PMA,RMA                                                    
C                          PITCH,ROLL MISALIGNMENTS OF INSTRUMENT (RAD) 
         COMMON /ELCOMM/ XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                
C========================= END DIELCOMM ===========================     
                                                                        
      COMMON /RADCOM/ AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C            
                                                                        
C-----------------------------------------------------------------------
                                                                        
      IERR=1                                                            
                                                                        
C COMPUTES TRIGONOMETRIC FUNKTIONS OF THE SCAN AND ELEVATION            
C ANGLES CORRECTED FOR THE ROLL AND PITCH MISALIGNMENTS                 
      CA = DCOS(ALPHA)                                                   
      SA = DSIN(ALPHA)                                                   
      CZ = DCOS(ZETA)
      DA = ALPHA-PMA*SA*(1.0D0/CZ+DTAN(ZETA))-RMA*(1.0D0-CA/CZ)        
      DZ = ZETA + RMA * SA                                              
                                                                        
C CORRECTED SCAN ANGLE                                                  
      CZ = DCOS(DZ)                                                      
                                                                        
C COMPUTES POINTING VECTOR IN INSTRUMENT COORDINATES                    
      G(1) = DSIN(DZ)                                                    
      G(2) = -CZ * DSIN(DA)                                              
      G(3) = CZ * DCOS(DA)                                               
                                                                        
C TRANSFORMS THE POINTING VECTOR TO EARTH FIXED COORDINATES             
      G1(1) = BT(1,1) * G(1) + BT(1,2) * G(2) + BT(1,3) * G(3)          
      G1(2) = BT(2,1) * G(1) + BT(2,2) * G(2) + BT(2,3) * G(3)          
      G1(3) = BT(3,1) * G(1) + BT(3,2) * G(2) + BT(3,3) * G(3)          
C COMPUTES COEFFICIENTS AND SOLVES A QUADRATIC EQUATION TO              
C FIND THE INTERSECT OF THE POINTING VECTOR WITH THE EARTH              
C SURFACE                                                               
      Q1 = G1(1)**2 + G1(2)**2 + AEBE2C * G1(3)**2                      
      Q2 = XS(1) * G1(1) + XS(2) * G1(2) + AEBE2C * XS(3) * G1(3)       
      D  = Q2 * Q2 - Q1 * Q3                                            
      IF (DABS(D).LT.1.D-9) D=0.                                        

                                                                        
C IF THE DISCRIMINANT OF THE EQUATION, D, IS NEGATIVE, THE              
C INSTRUMENT POINTS OFF THE EARTH                                       
      IF (D.LT.0) THEN                                                  
        RLAT = 999999.                                                  
        RLON = 999999.                                                  
        RETURN                                                          
      ENDIF                                                             
      D = DSQRT(D)                                                      
                                                                        
C SLANT DISTANCE FROM THE SATELLITE TO THE EARTH POINT                  
      H = -(Q2 + D) / Q1                                                
                                                                        
C CARTESIAN COORDINATES OF THE EARTH POINT                              
      U(1) = XS(1) + H * G1(1)                                          
      U(2) = XS(2) + H * G1(2)                                          
      U(3) = XS(3) + H * G1(3)                                          
                                                                        
C SINUS OF GEOCENTRIC LATITUDE                                          
      D1 = U(3) / DSQRT(U(1)**2 + U(2)**2 + U(3)**2)                     
                                                                        
C GEOGRAPHIC (GEODETIC) COORDINATES OF THE POINT                        
      RLAT = DATAN(AEBE2C * D1 / DSQRT(1. - D1 * D1))                     
      RLON = DATAN2(U(2),U(1))                                           
      IERR = 0                                                          
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
                                                                        
                                                                        
      SUBROUTINE GPOINT(RLAT,RLON,ALF,GAM,IERR)                         
                                                                        
      IMPLICIT NONE
                                                                        
C CALLING PARAMETERS                                                    
      REAL*8   RLAT
C GEOGRAPHIC LATITUDE IN RADIANS (INPUT)                                
      REAL*8   RLON
C GEOGRAPHIC LONGITUDE IN RADIANS (INPUT)                               
      REAL*8   ALF
C ELEVATION ANGLE IN RADIANS (OUTPUT)                                   
      REAL*8   GAM
C SCAN ANGLE IN RADIANS (OUTPUT)                                        
      INTEGER IERR                                                      
C OUTPUT STATUS; 0 - SUCCESSFUL COMPLETION,                             
C                1 - POINT WITH GIVEN LAT/LON IS INVISIBLE              
                                                                        
      REAL*8       F(3)  , AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C
      REAL*8       FT(3) , U(3)  , SING  , SLAT  , W1    , W2
                                                                        
C     INCLUDE 'ELCONS.INC'
C=========================== DIELCONS =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCONS = INCLUDE                                                
                                                                        
      REAL*8 PI                                                         
           PARAMETER (PI=3.141592653589793D0)                           
      REAL*8 DEG                                                        
           PARAMETER (DEG=180.D0/PI)                                    
      REAL*8 RAD                                                        
           PARAMETER (RAD=PI/180.D0)                                    
C                    DEGREES TO RADIANS CONVERSION PI/180               
      REAL*8 NOMORB                                                     
           PARAMETER (NOMORB=42164.365D0)                               
C                    NOMINAL RADIAL DISTANCE OF SATELLITE (KM)          
      REAL*8 AE                                                         
           PARAMETER (AE=6378.137D0)                                    
C                    EARTH EQUATORIAL RADIUS (KM)                       
      REAL*8 FER                                                        
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                        
C                    EARTH FLATTENING COEFFICIENT = 1-(BE/AE)           
      REAL*4 AEBE2                                                      
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                         
      REAL*4 AEBE3                                                      
           PARAMETER (AEBE3=AEBE2-1.)                                   
      REAL*4 AEBE4                                                      
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                           
C========================= END DIELCONS ===========================     
C     INCLUDE 'ELCOMM.INC'
C=========================== DIELCOMM =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCOMM = INCLUDE                                                
                                                                        
C     COMMON VARIABLES                                                  
C                                                                       
      REAL*8 XS(3)                                                      
C                      NORMALIZED S/C POSITION IN ECEF COORDINATES      
      REAL*8 BT(3,3)                                                    
C                      ECEF TO INSTRUMENT COORDINATES TRANSFORMATION    
      REAL*8  Q3                                                        
C                      USED IN SUBRTN LPOINT                        
      REAL*8 PITCH,ROLL,YAW                                             
C                          PITCH,ROLL,YAW ANGLES OF INSTRUMENT (RAD)    
      REAL*4 PMA,RMA                                                    
C                          PITCH,ROLL MISALIGNMENTS OF INSTRUMENT (RAD) 
         COMMON /ELCOMM/ XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                
C========================= END DIELCOMM ===========================     
                                                                        
      COMMON /RADCOM/ AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C            
                                                                        
C-----------------------------------------------------------------------
                                                                        
C COMPUTES SINUS OF GEOGRAPHIC (GEODETIC) LATITUDE                      
      SING = DSIN(RLAT)                                                  
      W1   = AEBE4C * SING * SING                                       
                                                                        
C SINUS OF THE GEOCENTRIC LATITUDE                                      
      SLAT = ((0.375 * W1 - 0.5) * W1 + 1.) * SING / AEBE2C             
                                                                        
C COMPUTES LOCAL EARTH RADIUS AT SPECIFIED POINT                        
      W2 = SLAT * SLAT                                                  
      W1 = AEBE3C * W2                                                  
      W1 = (0.375 * W1 - 0.5) * W1 + 1.                                 
                                                                        
C COMPUTES CARTESIAN COORDINATES OF THE POINT                           
      U(3) = SLAT * W1                                                  
      W2   = W1 * DSQRT(1. - W2)                                         
      U(1) = W2 * DCOS(RLON)                                             
      U(2) = W2 * DSIN(RLON)                                             
                                                                        
C POINTING VECTOR FROM SATELLITE TO THE EARTH POINT                     
      F(1) = U(1) - XS(1)                                               
      F(2) = U(2) - XS(2)                                               
      F(3) = U(3) - XS(3)                                               
      W2   = U(1) * SNGL(F(1)) + U(2) * SNGL(F(2))+ U(3) * SNGL(F(3))   
     &                                                   * AEBE2C       
                                                                        
C VERIFIES VISIBILITY OF THE POINT                                      
      IF (W2.GT.0.) THEN                                                
                                                                        
C INVISIBLE POINT ON THE EARTH                                          
        IERR = 1                                                        
        ALF  = 99999.                                                   
        GAM  = 99999.                                                   
        RETURN                                                          
      ENDIF                                                             
                                                                        
C CONVERTS POINTING VECTOR TO INSTRUMENT COORDINATES                    
      FT(1) = BT(1,1) * F(1) + BT(2,1) * F(2) + BT(3,1) * F(3)          
      FT(2) = BT(1,2) * F(1) + BT(2,2) * F(2) + BT(3,2) * F(3)          
      FT(3) = BT(1,3) * F(1) + BT(2,3) * F(2) + BT(3,3) * F(3)          
                                                                        
C CONVERTS POINTING VECTOR TO SCAN AND ELEVATION ANGLES AND             
C CORRECTS FOR THE ROLL AND PITCH MISALIGNMENTS                         
      GAM  = DATAN(FT(1) / DSQRT(FT(2)**2 + FT(3)**2))                    
      ALF  = -DATAN(FT(2) / FT(3))                                       
      W1   = DSIN(ALF)                                                   
      W2   = DCOS(GAM)                                                   
      ALF  = ALF + RMA * (1. - DCOS(ALF) / W2) + PMA * W1 * 
     &                                   (1. / W2 + DTAN(GAM))         
      GAM  = GAM - RMA * W1                                             
      IERR = 0                                                          
                                                                        
      RETURN                                                            
      END
c
c--- an in-line version of gvrang (originally angles.for) must be
c--- included because of the common block, elcomm.
c
c--- this is required because the preprocessor, convdlm, renames
c--- all of the common blocks, appending the nav type.  the common
c--- block in angles.for was not being renamed, and therefor not
c--- being accessed.  one of the big problems with this solution
c--- is that applications cannot currently call angles directly
c--- for goes-8 navigation.  applications should use nvxopt('ANG '...)
c--- instead.
c

      SUBROUTINE GVRANG(JDAY,JTIME,XLAT,XLON,GHA,DEC,SATANG,SUNANG,RELAN
     *G)
C
C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
C $ SUBROUTINE ANGLES(JDAY,JTIME,XLAT,XLON,GHA,DEC,SATANG,SUNANG,RELANG)
C $ ANGLES - computes zenith angles of sun and satellite and relative
C $   azimuth angle              (DAS)
C $ INPUT:
C $   JDAY = (I) picture day (YYDDD)
C $   JTIME = (I) picture start time
C $   XLAT = (R) latitude of point
C $   XLON = (R) longitude of point
C $   GHA = (R) Greenwich hour angle of sun
C $   DEC = (R) declination of sun
C $ OUTPUT:
C $   SATANG = (R) zenith angle of satellite
C $   SUNANG = (R) zenith angle of sun
C $   RELANG = (R) relative angle
C $$ ANGLES = COMPUTATION, NAVIGATION
C ANGLES MOSHER 1074 WINLIB  ZENITH ANGLES TO SAT,SUN,AND REL AZIMUTH AN
C
C     INCLUDE 'ELCONS.INC'
C=========================== DIELCONS =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCONS = INCLUDE                                                
                                                                        
      REAL*8 PI                                                         
           PARAMETER (PI=3.141592653589793D0)                           
      REAL*8 DEG                                                        
           PARAMETER (DEG=180.D0/PI)                                    
      REAL*8 RAD                                                        
           PARAMETER (RAD=PI/180.D0)                                    
C                    DEGREES TO RADIANS CONVERSION PI/180               
      REAL*8 NOMORB                                                     
           PARAMETER (NOMORB=42164.365D0)                               
C                    NOMINAL RADIAL DISTANCE OF SATELLITE (KM)          
      REAL*8 AE                                                         
           PARAMETER (AE=6378.137D0)                                    
C                    EARTH EQUATORIAL RADIUS (KM)                       
      REAL*8 FER                                                        
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                        
C                    EARTH FLATTENING COEFFICIENT = 1-(BE/AE)           
      REAL*4 AEBE2                                                      
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                         
      REAL*4 AEBE3                                                      
           PARAMETER (AEBE3=AEBE2-1.)                                   
      REAL*4 AEBE4                                                      
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                           
C========================= END DIELCONS ===========================     
C     INCLUDE 'ELCOMM.INC'
C=========================== DIELCOMM =============================     
                                                                        
C $   (JR)                                                              
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE       
C $                                                                     
C $   DESCRIPTION:                                                      
C $                                                                     
C $                                                                     
C $$  DIELCOMM = INCLUDE                                                
                                                                        
C     COMMON VARIABLES                                                  
C                                                                       
      REAL*8 XS(3)                                                      
C                      NORMALIZED S/C POSITION IN ECEF COORDINATES      
      REAL*8 BT(3,3)                                                    
C                      ECEF TO INSTRUMENT COORDINATES TRANSFORMATION    
      REAL*8  Q3                                                        
C                      USED IN SUBROUTINE LPOINT                        
      REAL*8 PITCH,ROLL,YAW                                             
C                          PITCH,ROLL,YAW ANGLES OF INSTRUMENT (RAD)    
      REAL*4 PMA,RMA                                                    
C                          PITCH,ROLL MISALIGNMENTS OF INSTRUMENT (RAD) 
         COMMON /ELCOMM/ XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                
C========================= END DIELCOMM ===========================     
      DATA IDAY/0/
      DATA R/6371.221/
      RDPDG=PI/180.0
      IF(IDAY.EQ.JDAY)GO TO 1
      IDAY=JDAY
      INORB=0
 1    PICTIM=FTIME(JTIME)
C
C   DETERMINE SATELLITE POSITION
C
      XSAT = XS(1) * AE 
      YSAT = XS(2) * AE 
      ZSAT = XS(3) * AE 

      HEIGHT=SQRT(XSAT**2+YSAT**2+ZSAT**2)
      YLAT=RDPDG*XLAT
      YLAT=GEOLAT(YLAT,1)
      YLON=RDPDG*XLON
      SLAT=SIN(YLAT)
      CLAT=COS(YLAT)
      SLON=SIN(YLON)
      CLON=COS(YLON)
      XSAM=R*CLAT*CLON
      YSAM=R*CLAT*SLON
      ZSAM=R*SLAT
C
C   DETERMINE ZENITH ANGLE OF SUN
C
      SNLG=-PICTIM*PI/12.0-RDPDG*GHA
      SNDC=RDPDG*DEC
      COSDEC=COS(SNDC)
      US=COS(SNLG)*COSDEC
      VS=SIN(SNLG)*COSDEC
      WS=SIN(SNDC)
      SUNANG=ACOS((US*XSAM+VS*YSAM+WS*ZSAM)/R)/RDPDG
C
C   DETERMINE ZENITH ANGLE OF SATELLITE
C
      XVEC=XSAT-XSAM
      YVEC=YSAT-YSAM
      ZVEC=ZSAT-ZSAM
      XFACT=SQRT(XVEC**2+YVEC**2+ZVEC**2)
      SATANG=ACOS((XVEC*XSAM+YVEC*YSAM+ZVEC*ZSAM)/(R*XFACT))/RDPDG
C
C   DETERMINE RELATIVE ANGLE
C
      X1=CLAT*CLON
      Y1=CLAT*SLON
      Z1=SLAT
      X2=SLON
      Y2=-CLON
      X3=-SLAT*CLON
      Y3=-SLAT*SLON
      Z3=CLAT
      XC1=US-X1
      YC1=VS-Y1
      ZC1=WS-Z1
      XC2=XSAT/HEIGHT-X1
      YC2=YSAT/HEIGHT-Y1
      ZC2=ZSAT/HEIGHT-Z1
      XAN1=XC1*X3+YC1*Y3+ZC1*Z3
      XAN2=XC2*X3+YC2*Y3+ZC2*Z3
      YAN1=XC1*X2+YC1*Y2
      YAN2=XC2*X2+YC2*Y2
      XAN3=XAN1*XAN2+YAN1*YAN2
      YAN3=-YAN1*XAN2+XAN1*YAN2
      RELANG=ATAN2(YAN3,XAN3)/RDPDG
      RELANG=ABS(RELANG)
      RETURN
      END



C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION FTIME(M)
C *** McIDAS Revision History ***
C 1 FTIME.FOR 23-Mar-90,12:38:58,`SSEC' PC-McIDAS ver 5.00
C 2 FTIME.FOR 25-Sep-90,7:28:44,`SMG' First Release into COMmon
C 3 FTIME.FOR 1-Apr-94,1:12:36,`BARRYR' Add proprietary statement
C 4 FTIME.FOR 2-May-94,16:40:44,`USER' Released
C *** McIDAS Revision History ***
C $ FUNCTION FTIME(M)  (BL)
C $ CONVERT PACKED INTEGER (SIGN HH MM SS) TIME TO REAL*4
C $ M = (I) INPUT  PACKED INTEGER (SIGN HH MM SS) TIME
C $$ FTIME = CONVERT, INTEGER, TIME, REAL
C
      IF(M.LT.0)GO TO 1
      N=M
      X=1.0
      GO TO 2
 1    N=-M
      X=-1.0
 2    FTIME=FLOAT(N/10000)+FLOAT(MOD(N/100,100))/60.0+FLOAT(MOD(N,100))/
     13600.0
      FTIME=X*FTIME
      RETURN
      END

C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION GEOLAT(XLAT,IDIR)
C *** McIDAS Revision History ***
C 1 GEOLAT.FOR 23-Mar-90,12:40:32,`SSEC' PC-McIDAS ver 5.00
C 2 GEOLAT.FOR 25-Sep-90,7:28:48,`SMG' First Release into COMmon
C 3 GEOLAT.FOR 1-Apr-94,1:12:42,`BARRYR' Add proprietary statement
C 4 GEOLAT.FOR 2-May-94,16:40:54,`USER' Released
C *** McIDAS Revision History ***
C $ FUNCTION GEOLAT(XLAT, IDIR)  (DAS)
C $ GEOCENTRIC/GEODETIC LATITUDE CONVERSION.  FN VAL IN RADIANS.
C $ XLAT = (R) INPUT  LATITUDE (RADIANS)
C $ IDIR = (I) 1 FOR GEODEDIC TO GEOCENTRIC CONVERSION, 2 FOR GEOCENTRIC
C $   TO GEODEDIC CONVERSION
C $$ GEOLAT = CONVERT, LATITUDE, NAVIGATION
C
C-----XLAT, FN VALUE EXPRESSED IN RADIANS AS PER HARRIS SYSTEM
C
      DATA A/6378.388/,B/6356.912/
      ASQ=A**2
      BSQ=B**2
      CX=COS(XLAT)
      SX=SIN(XLAT)
      IF(IDIR.EQ.2)GOTO 1
      GEOLAT=ATAN2(BSQ*SX,ASQ*CX)
      RETURN
    1 GEOLAT=ATAN2(ASQ*SX,BSQ*CX)
      RETURN
      END

C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      SUBROUTINE SOLARP(JDAY,JTIME,GHA,DEC,XLAT,XLON)
C *** McIDAS Revision History ***
C 1 SOLARP.FOR 23-Mar-90,12:34:48,`SSEC' PC-McIDAS ver 5.00
C 2 SOLARP.FOR 25-Sep-90,7:36:38,`SMG' First Release into COMmon
C 3 SOLARP.FOR 1-Apr-94,1:41:40,`BARRYR' Add proprietary statement
C 4 SOLARP.FOR 2-May-94,17:30:14,`USER' Released
C *** McIDAS Revision History ***
C SOLARP MOSHER 1074 WINLIB  Z HOUR ANGLE AND SOLAR DECL FOR DAY-TIME
C $ SUBROUTINE SOLARP(JDAY, JTIME, GHA, DEC, XLAT, XLON)  (DAS)
C $ COMPUTES GREENWICH HOUR ANGLE AND DECLINATION OF SUN
C $ JDAY = (I) INPUT  SATELLITE/YEAR/DAY
C $ JTIME = (I) INPUT  HOUR/MINUTE/SECOND
C $ GHA = (R) OUTPUT  GREENWICH HOUR ANGLE
C $ DEC = (R) OUTPUT  DECLINATION
C $ XLAT = (R) OUTPUT  LATITUDE OF SUN POSITION
C $ XLON = (R) OUTPUT  LONGITUDE OF SUN POSITION
C $$ SOLARP = COMPUTATION, NAVIGATION
C
C     ORBITAL CONSTANTS
C
C     IEPYD = EPOCH YEAR-DAY
C     IEPHMS = EPOCH HOUR-MINUTE-SECOND
C     OECCEN = ECCENTRICITY OF EARTH ORBIT
C     OINCLI = INCLINATION TO CELESTIAL EQUATOR
C     PERHEL = PERIHELION
C     ASNODE = ASCENDING NODE
C     XMANOM = MEAN ANOMOLY
C     XMMC = MEAN MOTION CONSTANT
C     SHA = CELESTIAL HOUR ANGLE
C     IRAYD  =  YYDDD WHEN CELESTIAL COOR. SYS. COINCIDES WITH EARTH COO
C     IRAHMS = HHMMSS WHEN CELESTIAL COOR. SYS. COINCIDES WITH EARTH COO
C
      REAL*8 DIFTIM,ECANM1,ECANOM,RAHA,TIMDIF,XHA,XMANOM
C     REAL*8 DABS,DMOD,DSQRT,DSIN,DCOS,DATAN2
      DATA INIT/0/
C
C
      IF(INIT.NE.0)GO TO 1
      INIT=1
      PI=3.14159265
      RDPDG=PI/180.0
      SOLSID=1.00273791
      IEPYD=74004
      IEPHMS=0
      OECCEN=0.016722
      OINCLI=RDPDG*FLALO(232700)
      PERHEL=RDPDG*FLALO(1011311)+PI
      ASNODE=RDPDG*FLALO(0)
      XMMC=.01720209895/1440.0
      SHA=100.26467
      IRAYD=74001
      IRAHMS=0
      SINC=SIN(OINCLI)
      CINC=COS(OINCLI)
      SPER=SIN(PERHEL)
      CPER=COS(PERHEL)
      SAND=SIN(ASNODE)
      CAND=COS(ASNODE)
      PX=CPER*CAND-SPER*SAND*CINC
      PY=CPER*SAND+SPER*CAND*CINC
      PZ=SPER*SINC
      QX=-SPER*CAND-CPER*SAND*CINC
      QY=-SPER*SAND+CPER*CAND*CINC
      QZ=CPER*SINC
 1    IDAY=MOD(JDAY,100000)
      PTIME=FTIME(JTIME)
      DIFTIM=TIMDIF(IEPYD,IEPHMS,IDAY,JTIME)
      XMANOM=XMMC*DIFTIM
      ECANM1=XMANOM
      EPSILN=1.0E-8
      DO 2 I=1,20
      ECANOM=XMANOM+OECCEN*SIN(ECANM1)
      IF(ABS(ECANOM-ECANM1).LT.EPSILN)GO TO 3
 2    ECANM1=ECANOM
 3    XOMEGA=DCOS(ECANOM)-OECCEN
      YOMEGA=SQRT(1.0-OECCEN**2)*DSIN(ECANOM)
      XFACT=1.0/SQRT(XOMEGA**2+YOMEGA**2)
      XOMEGA=XOMEGA*XFACT
      YOMEGA=YOMEGA*XFACT
      XS=XOMEGA*PX+YOMEGA*QX
      YS=XOMEGA*PY+YOMEGA*QY
      ZS=XOMEGA*PZ+YOMEGA*QZ
      SLRA=ATAN2(YS,XS)/RDPDG
      RAHA=TIMDIF(IRAYD,IRAHMS,IDAY,JTIME)*SOLSID/4.0
      GHA=PTIME*15.0
      XHA=360.0-SHA-RAHA+SLRA+GHA
      GHA=DMOD(XHA,360.0D0)
      GHA=360.0-GHA-2.0
      DEC=ATAN2(ZS,SQRT(XS**2+YS**2))/RDPDG
      XLAT=GEOLAT(DEC*RDPDG,1)/RDPDG
      XLON=-GHA-PTIME*15.0+720.0
      XLON=AMOD(XLON,360.0)
      RETURN
      END

      FUNCTION IFTOK(CTOK)
C *** McIDAS Revision History ***
C 1 IFTOK.FOR 27-Feb-90,9:34:44,`SSEC' PC-McIDAS ver 5.00
C 2 IFTOK.FOR 24-Sep-90,18:19:20,`SMG' First Release into COMmon
C 3 IFTOK.FOR 1-Jul-94,9:20:00,`USER' Released
C *** McIDAS Revision History ***
C $ FUNCTION IFTOK(CTOK)  (TMW)
C $ CONVERT NUMERIC CHARACTER TOKEN TO INTERNAL FORM. FN VAL IS INTEGER
C $   (ROUNDED) PART OF NUMERIC  VALUE.
C $ CTOK = (C) INPUT  CHARACTER TOKEN TO CONVERT
C $$ IFTOK = CONVERT, UTILITY, SCANNER
C
C-----CONTAINS ENTRY POINTS IFTOK,DFTOK
C-----CONVERT NUMERIC TOKEN TO BINARY
C-----TOKEN TO CONVERT IS IN CTOK
C-----FUNCTION VALUE:
C        IFTOK: INTEGER (ROUNDED) PART OF NUMERIC VALUE
C        DFTOK: REAL*8
C-----FUNCTION COMPUTED BY CALCULATING IVAL & ISCAL, WHERE TRUE
C        VALUE = IVAL * (10**ISCAL). ISCAL IS ARBITRARILY RESTRICTED TO
C        BETWEEN -30 AND 30.
C-----IF SYNTAX ERROR OR OUT-OF-RANGE CONDITION DETECTED, 0 IS
C        RETURNED
C
C-----NUMBER IN CTOK CAN APPEAR AS: HEX (E.G. $7FFF) DECIMAL INTEGER
C        (E.G. -123) FLOATING POINT (E.G. -123. , -123.E-6 , -123.D15)
C        BABYLONIAN (E.G. 12:30 , -179:59:59)
C        DATE (E.G. 83/1/31 , 2883/1/31 (28 IS SS) , 28/// )
C
C
C
      PARAMETER (IMAXL=214748364,IMAXR=7)
      IMPLICIT CHARACTER*12 (C)
      IMPLICIT REAL*8 (D)
      CHARACTER CTOK*(*)
      CHARACTER*33 CSTR
      CHARACTER*2 C2
      CHARACTER*1 C
      INTEGER IPART(4)
      INTEGER MAXVAL(3)
      INTEGER *2 ITYPE,IZERO,ICHA,J,ISIGN,ICH,IDIG,K,IC,INDEX,NP
      INTEGER *2 JJ
      DATA MAXVAL/9999,59,59/
C
C
C
C----(FUNCTION IFTOK(CTOK))
         ITYPE=1
         GOTO 1
      ENTRY DFTOK(CTOK)
C $ ENTRY DFTOK(CTOK)  (TMW)
C $ CONVERT NUMERIC CHARACTER TOKEN TO INTERNAL FORM. FN VAL IS REAL*8.
C $ CTOK = (C) INPUT  CHARACTER TOKEN TO CONVERT
C $$ DFTOK = CONVERT, UTILITY, SCANNER
         ITYPE=2
         GOTO 1
C
C
C
 1    IZERO=ICHAR('0')
       ICHA=ICHAR('A')
      LENC=MIN0(32,LEN(CTOK))
C-----LOCATE FIRST NON-BLANK
      DO 2 J=1,LENC
         C=CTOK(J:J)
         IF (C.NE.' ') GOTO 3
 2    CONTINUE
      GOTO 89
 3    CSTR=CTOK(J:LENC)
      IVAL=0
      ISCAL=0
      ISIGN=1
      C=CSTR(1:1)
      K=1
      IF (C.EQ.'+') THEN
         K=2
      ELSE IF (C.EQ.'-') THEN
         K=2
         ISIGN=-1
      ENDIF
      C2=CSTR(1:2)
C
C
      IF (C.EQ.'$') THEN
C
C-----CONVERT HEX NUMBER
      IF (C2.EQ.'$ '.OR.CSTR(10:10).NE.' ') GOTO 89
      DO 5 J=2,9
         C=CSTR(J:J)
         IF (C.EQ.' ') GOTO 90
         ICH=ICHAR(C)
         IDIG=ICH-IZERO
         IF (IDIG.GE. 0 .AND.IDIG.LE. 9 ) GOTO 6
         IDIG=ICH-ICHA+ 10
         IF (IDIG.GE. 10 .AND.IDIG.LE. 15 ) GOTO 6
         GOTO 89
 6       CONTINUE
         IF (J.LT. 9 ) THEN
            IVAL=IVAL*16+IDIG
         ELSE
            K=IC(IVAL,0)
            CALL STC(0,IVAL,0)
            IVAL=IVAL*16+IDIG
            CALL STC(K*16+IC(IVAL,0),IVAL,0)
         ENDIF
 5    CONTINUE
C
C
      ELSE IF (INDEX(CSTR(1:16),':').GT.0) THEN
C
C
C-----CONVERT BABYLONIAN TOKEN (E.G. TIME-- 12:45:00) TO INTERNAL
C        BINARY (SCALED INTEGER) FORM. IVAL IS HHDDDDD, WHERE HH IS
C        HOURS AND DDDDD IS FRACTIONS OF AN HOUR
C
C
      IVAL=0
      ISCAL=-5
      IF (C2.EQ.': ') THEN
         CALL GETTIM(L)
         LSECS=MOD(L,10000)/100*60+MOD(L,100)
         IVAL=L/10000*100000+(LSECS*1000+18)/36
         GOTO 90
      ENDIF
      IPART(1)=0
      IPART(2)=0
      IPART(3)=0
C-----NP COUNTS # OF ':' SEEN (PLUS 1)
      NP=1
C
C-----GET NEXT CHARACTER
C
      DO 22 J=K,16
         C=CSTR(J:J)
         IF (C.EQ.' ') THEN
            L=((IPART(2)*60+IPART(3))*1000+18)/36
            IVAL=IPART(1)*100000+L
            IVAL=IVAL*ISIGN
            GOTO 90
         ELSE IF (C.EQ.':') THEN
            NP=NP+ 1
            IF (NP.GT. 3 ) GOTO 89
         ELSE IF (C.GE.'0'.AND.C.LE.'9') THEN
            IPART(NP)=IPART(NP)*10+ICHAR(C)-IZERO
            IF (IPART(NP).GT.MAXVAL(NP)) GOTO 89
         ELSE
            GOTO 89
         ENDIF
 22   CONTINUE
C
C
      ELSE IF (INDEX(CSTR(1:16),'/').GT.0) THEN
C
C
C-----CONVERT DATE IN (GENERAL) FORM SS/YY/MM/DD OR SSYY/MM/DD
      CALL GETDAY(IYYDDD)
      DO 31 J=1,4
 31   IPART(J)=0
      NP=1
      DO 35 J=K,16
         C=CSTR(J:J)
         IF (C.EQ.' ') THEN
C-----      REVERSE IPART ARRAY SO THAT DAY IS IN IPART(1), MON
C           IN IPART(2), ETC.
            I=IPART(1)
            IPART(1)=IPART(NP)
            IPART(NP)=I
            IF (NP.GT. 3 ) THEN
               I=IPART(2)
               IPART(2)=IPART(3)
               IPART(3)=I
            ENDIF
            IF (IPART(3).EQ.0) IPART(3)=IYYDDD/1000
            IF (IPART(1).EQ.0.AND.IPART(2).EQ.0) THEN
               IVAL=MOD(IYYDDD,1000)+IPART(3)*1000
            ELSE IF (IPART(1).EQ.0.OR.IPART(2).EQ.0) THEN
               GOTO 89
            ELSE
               IF (IPART(1).GT.31) GOTO 89
               IF (IPART(2).GT.12) GOTO 89
               IVAL=IDMYYD(IPART(1),IPART(2),IPART(3))
            ENDIF
            IF (IPART(4).GT.9999) GOTO 89
            IVAL=IVAL+IPART(4)*100000
            IVAL=ISIGN*IVAL
            GOTO 90
         ELSE IF (C.EQ.'/') THEN
            NP=NP+ 1
            IF (NP.GT. 4 ) GOTO 89
         ELSE IF (C.GE.'0'.AND.C.LE.'9') THEN
            IPART(NP)=IPART(NP)*10+ICHAR(C)-IZERO
         ELSE
            GOTO 89
         ENDIF
 35   CONTINUE
C
C
      ELSE
C
C
C-----CONVERT NUMERIC (INTEGER OR FLOATING POINT) VALUE
C-----SAMPLE FORMS: 0, +12, 12 , 12., -12.3, +12.3@09, .004@-11
C
C
      IF (C2.EQ.'. '.OR.C2.EQ.'+ '.OR.C2.EQ.'- ') GOTO 89
C-----NDEC COUNTS # OF PLACES AFTER DECIMAL POINT
      NDEC=-1
C-----IEXPSN IS SIGN OF EXPONENT
      IEXPSN=1
C
C-----NEXT-CHARACTER LOOP
C
      DO 42 J=K,16
         C=CSTR(J:J)
         II=ICHAR(C)
         IF (C.EQ.' ') GOTO 45
         IF (C.EQ.'.') THEN
            IF (NDEC.GE.0) GOTO 89
            NDEC=0
         ELSE IF (C.GE.'0'.AND.C.LE.'9') THEN
            IDIG=ICHAR(C)-IZERO
           IF(IVAL.GT.IMAXL.OR.(IVAL.EQ.IMAXL.AND.IDIG.GT.IMAXR))GOTO 891
            IVAL=IVAL*10+IDIG
            IF (NDEC.GE.0) NDEC=NDEC+ 1
         ELSE IF (C.EQ.'E'.OR.C.EQ.'D') THEN
            JJ=J+ 1
            C=CSTR(JJ:JJ)
            JJ=JJ+ 1
            IF (C.EQ.'+') THEN
               C=CSTR(JJ:JJ)
               JJ=JJ+ 1
            ELSE IF (C.EQ.'-') THEN
               IEXPSN=-1
               C=CSTR(JJ:JJ)
               JJ=JJ+ 1
            ENDIF
            IF (C.LT.'0'.OR.C.GT.'9') GOTO 89
            ISCAL=ICHAR(C)-IZERO
            C=CSTR(JJ:JJ)
            IF (C.EQ.' ') GOTO 45
            IF (C.LT.'0'.OR.C.GT.'9') GOTO 89
            ISCAL=ISCAL*10+ICHAR(C)-IZERO
            GOTO 45
         ELSE
            GOTO 89
         ENDIF
 42   CONTINUE
 45   CONTINUE
      IVAL=IVAL*ISIGN
      ISCAL=IEXPSN*ISCAL-MAX0(0,NDEC)
      IF (ISCAL.LT.-30.OR.ISCAL.GT.30) GOTO 89
      GOTO 90
C
C
      ENDIF
C
C-----DONE
C
C-----ERROR CONDITION COMES HERE
 89   CONTINUE
      IVAL=0
      ISCAL=0
C-----ALL ORDINARY RETURNS COME HERE
 90   CONTINUE
C-----CONVERT TO CORRECT OUTPUT TYPE
      IF (ITYPE.EQ. 1 ) THEN
C-----   INTEGER TYPE. IF IABS(INTEGER) .GE. 1D10, 0 IS RETURNED
         IF (IVAL.EQ.0.OR.ISCAL.EQ.0) THEN
            IFTOK=IVAL
            RETURN
         ELSE
            D=DFLOAT(IABS(IVAL))*(10.D0**ISCAL)
            IF (D.LE.1.D9) THEN
               IFTOK=IDINT(D+.5D0)
               IF (IVAL.LT.0) IFTOK=-IFTOK
            ELSE
               IFTOK=0
            ENDIF
         ENDIF
      ELSE
         DFTOK=DFLOAT(IVAL)*(10.D0**ISCAL)
      ENDIF
      RETURN
      END

C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION IROUND(X)
C *** McIDAS Revision History ***
C 1 IROUND.FOR 16-Mar-90,16:38:50,`SSEC' PC-McIDAS ver 5.00
C 2 IROUND.FOR 25-Sep-90,7:34:30,`SMG' First Release into COMmon
C 3 IROUND.FOR 1-Apr-94,1:17:16,`BARRYR' Add proprietary statement
C 4 IROUND.FOR 2-May-94,16:48:14,`USER' Released
C *** McIDAS Revision History ***
C $ FUNCTION IROUND(X)  (JMB)
C $ ROUNDS A FLOATING POINT VALUE
C $ X = (R) INPUT  FLOATING POINT VALUE
C $$ IROUND = REAL
C
      IROUND=NINT(X)
      RETURN
      END


C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION FLALO(M)
C *** McIDAS Revision History ***
C 1 FLALO.FOR 19-Mar-90,21:49:24,`SSEC' PC-McIDAS ver 5.00
C 2 FLALO.FOR 25-Sep-90,7:33:56,`SMG' First Release into COMmon
C 3 FLALO.FOR 1-Apr-94,1:11:14,`BARRYR' Add proprietary statement
C 4 FLALO.FOR 2-May-94,16:38:40,`USER' Released
C *** McIDAS Revision History ***
C $ FUNCTION FLALO(M)  (BL)
C $ CONVERT PACKED INTEGER (SIGN DDD MM SS) LATITUDE-LONGITUDE TO REAL*4
C $ M = (I) INPUT  PACKED INTEGER (SIGN DDD MM SS) LATITUDE-LONGITUDE
C $$ FLALO = CONVERT, INTEGER, LATITUDE, LONGITUDE, REAL
C
      IF(M.LT.0)GO TO 1
      N=M
      X=1.0
      GO TO 2
 1    N=-M
      X=-1.0
 2    FLALO=FLOAT(N/10000)+FLOAT(MOD(N/100,100))/60.0+FLOAT(MOD(N,100))/
     13600.0
      FLALO=X*FLALO
      RETURN
      END

C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
       CHARACTER*12 FUNCTION CFZ(L)
C *** McIDAS Revision History ***
C 1 CFZ.FOR 27-Feb-90,21:47:28,`SSEC' PC-McIDAS ver 5.00
C 2 CFZ.FOR 25-Sep-90,7:35:58,`SMG' First Release into COMmon
C 3 CFZ.FOR 1-Apr-94,1:03:14,`BARRYR' Add proprietary statement
C 4 CFZ.FOR 2-May-94,16:25:54,`USER' Released
C *** McIDAS Revision History ***
       IMPLICIT CHARACTER*12 (C)
       WRITE(C,1)L
 1     FORMAT(4X,Z8)
       CFZ=C
       RETURN
       END

C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      INTEGER*4 FUNCTION IDMYYD(IDAY,MON,IYEAR)
C *** McIDAS Revision History ***
C 1 IDMYYD.FOR 27-Feb-90,9:44:14,`SSEC' PC-McIDAS ver 5.00
C 2 IDMYYD.FOR 24-Sep-90,18:19:18,`SMG' First Release into COMmon
C 3 IDMYYD.FOR 12-Jan-94,16:53:38,`ROBERTM' Treat 2000 as a leap year (#4324)
C 4 IDMYYD.FOR 24-Jan-94,9:28:56,`USER' Released
C 5 IDMYYD.FOR 1-Apr-94,1:14:52,`BARRYR' Add proprietary statement
C 6 IDMYYD.FOR 2-May-94,16:44:36,`USER' Released
C *** McIDAS Revision History ***
C
C     CONVERT IDAY, MON, IYEAR TO YYDDD AS FUNCTION VALUE
C     (Valid only from 1 Jan 1901 through 28 Feb 2100)
C
      IMPLICIT INTEGER (A-Z)
      INTEGER MTBL(12)
      DATA MTBL/0,31,59,90,120,151,181,212,243,273,304,334/
C
C
      IDMYYD=0
      IF (IDAY.LT.1.OR.IDAY.GT.31) RETURN
      IF (MON.LT.1.OR.MON.GT.12) RETURN
      IDDD=IDAY+MTBL(MON)
      IYY=MOD(IYEAR,100)
      IF (MOD(IYY,4).EQ.0.AND.MON.GT.2) IDDD=IDDD+1
      IDMYYD=IYY*1000+IDDD
      RETURN
      END


C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
       FUNCTION LIT(C)
C *** McIDAS Revision History ***
C 1 LIT.FOR 27-Feb-90,9:04:00,`SSEC' PC-McIDAS ver 5.00
C 2 LIT.FOR 24-Sep-90,18:19:26,`SMG' First Release into COMmon
C 3 LIT.FOR 1-Apr-94,1:22:02,`BARRYR' Add proprietary statement
C 4 LIT.FOR 2-May-94,16:57:00,`USER' Released
C *** McIDAS Revision History ***
C $    FUNCTION LIT(CC)   (JMB)
C $    LIT  --  CHANGE CHARACTER*4 INTO INTEGER*4
C $    INPUT:
C $        CC  (C)  CHARACTER STRING
C $    FUNCTION VALUE:
C $        THE SAME BYTES, CHANGED TO TYPE INTEGER
       CHARACTER*(*) C
       CHARACTER*4 C1
       C1=C
       CALL MOVCW(C1,L1)
       LIT=L1
       RETURN
       END


C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION TIMDIF(IYRDA1,IHMS1,IYRDA2,IHMS2)
C *** McIDAS Revision History ***
C 1 TIMDIF.FOR 23-Mar-90,12:38:46,`SSEC' PC-McIDAS ver 5.00
C 2 TIMDIF.FOR 25-Sep-90,7:28:46,`SMG' First Release into COMmon
C 3 TIMDIF.FOR 1-Apr-94,1:43:24,`BARRYR' Add proprietary statement
C 4 TIMDIF.FOR 2-May-94,17:33:56,`USER' Released
C *** McIDAS Revision History ***
C $ TIMDIF(IYRDA1, IHMS1, IYRDA2, IHMS2)  (JI)
C $ TIME DIFFERENCE IN MINUTES
C $ IYRDA1 = (I) INPUT  FIRST YEAR AND JULIAN DAY (YYDDD)
C $ IHMS1 = (I) INPUT  FIRST TIME (HHMMSS)
C $ IYRDA2 = (I) INPUT  SECOND YEAR AND DAY (YYDDD)
C $ IHMS2 = (I) INPUT  SECOND TIME (HHMMSS)
C $$ TIMDIF = TIME
C
C FUNC VAL (REAL*8) IS TIME DIFFERENCE IN MINUTES (POSITIVE IF
C FIRST DAY/TIME IS THE EARLIER OF THE TWO).
C
      DOUBLE PRECISION TIMDIF,D1,D2,T1,T2
      IY1=MOD(IYRDA1/1000,100)
      ID1=MOD(IYRDA1,1000)
      IFAC1=(IY1-1)/4+1
      D1=365*(IY1-1)+IFAC1+ID1-1
      IY2=MOD(IYRDA2/1000,100)
      ID2=MOD(IYRDA2,1000)
      IFAC2=(IY2-1)/4+1
      D2=365*(IY2-1)+IFAC2+ID2-1
      T1=1440.D0*D1+60.D0*FLALO(IHMS1)
      T2=1440.D0*D2+60.D0*FLALO(IHMS2)
      TIMDIF=T2-T1
      RETURN
      END


C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      SUBROUTINE MOVCW(CBUF,IBUF)
C *** McIDAS Revision History ***
C 1 MOVCW.FOR 27-Feb-90,9:34:30,`SSEC' PC-McIDAS ver 5.00
C 2 MOVCW.FOR 24-Sep-90,18:19:22,`SMG' First Release into COMmon
C 3 MOVCW.FOR 1-Apr-94,1:30:32,`BARRYR' Add proprietary statement
C 4 MOVCW.FOR 2-May-94,17:10:36,`USER' Released
C *** McIDAS Revision History ***
      INTEGER IBUF(*)
      CHARACTER*(*) CBUF
      LENC=LEN(CBUF)
      CALL MOVB(LENC,CBUF,IBUF,0)
      RETURN
      END

