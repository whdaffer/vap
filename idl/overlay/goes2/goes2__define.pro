;+
; NAME:   goes2__define
; PURPOSE:   Defines and object of type goes2. This object is used in 
;            reading and navigating the goes data Scott Gennari
;            maintains on the goes 8/10 sites (explorer.arc.nasa.gov
;            and rsd.gsfc.nasa.gov)
;
; AUTHOR: William Daffer
;
; CATEGORY:   OO
;
; CALLING SEQUENCE:   goes2 = obj_new('goes2' $
;                                [, datetime, region, sensor ... ])
; 
; METHODS:
;   Init:
;   Set:
;   Get:
;   Cleanup:
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  

;============================================

FUNCTION goes2::Init,datetime, region, sensor
    ; (F is flattening factor = 1-BE/AE where AE = Equatorial
    ; radius  (6378.137 Km) and  BE = Polar Radius (6356.7533D)
    ; See Elcons.INC. 
  self.ae = double(6378.137)
  self.be = double(6356.7533)
  self.F = 1.0-self.be/self.ae

  self.INCMAX = 6136.  ; Number of Increments/Cycle
  self.SCNINC = 16.e-6 ; Change in scan angle/increment (MicroRadians)
  self.ELVINC =  8.e-6 ; Change in elevation angle/increment (microradians)
  self.SCNPX  = 16.e-6 ; Change in scan angle/pixel (microradians)
  self.ELVLN  = 28.e-6 ; Change in elevation angle/detector-line (microradians)

  self.SCNMAX =  (self.ELVMAX= !values.d_nan)
  self.Rma = (self.Pma=!values.d_nan)
  self.Roll = (self.Pitch=(self.Yaw=!values.d_nan))
  self.RR  = !values.d_nan
  self.mapLimits[*] = !values.d_nan
  self-> Set,region = region, sensor=sensor, datetime=datetime
  self.status = self->readdoc()
  self.status =  self.status ? self->ReadData(): self.status
  return,self.status
END

;============================================
; ReadDoc
;============================================
FUNCTION goes2::ReadDoc
  IF strlen( self.datetime ) NE 0 AND $
     strlen( self.region ) NE 0 THEN BEGIN 
    doc = Goes2ReadDoc(self.datetime, self.region )
    ptr_free, self.raw
    ptr_free, self.image
    IF isa(doc, /structure, name='GOES2DOC') THEN BEGIN 
      self.doc = temporary(doc)
        ; The following two quantities come down in words 6305-6310 of
        ; GVAR Block 0, but the yymmddhhmm.txt files maintained on the
        ; UoHawaii site don't go that far, so we have to calculate them.

      ; Bias in scan angle between line/pixel and elevation/scan angle
      ; coordinate system 
      self.SCNMAX = (self.doc.imdpx-1)*self.SCNPX
       ; Bias in elevation  between line/pixel and elevation/scan angle
      ; coordinate system 
      self.ELVMAX = -(4.5-self.doc.imdln)*self.ELVLN

      ; Is this a 'flipped' spacecraft?

      self.flipped =  (self.doc.iscan[2] AND 1) NE 0
      self.IMC     =  (self.doc.iscan[1] AND 1) NE 0 

      self.Roll = self.doc.a9
      self.Pitch = self.doc.a10
      self.Yaw = self.doc.a11

      IF self.IMC THEN BEGIN 
        self.LAM = self.doc.a5
        self.R   = 42164.17478 + self.doc.a6
        self.PHI =  self.doc.a7
        self.PSI =  self.doc.a8
        self.Rma = 0.
        self.Pma = 0.
      ENDIF ELSE BEGIN 

        et = self.doc.epochtime[0]
        daymon = doy2date(et[0],et[1])
        T0 = Var_To_DT(et[0],daymon[1],daymon[2],et[2],et[3],et[4] + et[5]*1.e-3)
        tt = self.doc.SpsTime
        daymon = doy2date(tt[0],tt[1])
        t1 = Var_To_DT(tt[0],daymon[1],daymon[2],tt[2],tt[3],tt[4] + tt[5]*1.e-3)
        self.T = (t1.julian-t0.julian)*84600.d
        B = -0.7292115d-4*self.T
        A = self.doc.a
        Dlon =  a[18] + a[19]*B + a[20]*B^2+$
          2*(a[21]*sin(B) + a[22]*cos(B)+$
             a[23]*sin(2*B) + a[24]*cos(2*B) + $
              a[25]*sin(1.9268*B) + a[26]*cos(1.9268*B) + $
               a[27]*sin(0.927*B) +  a[28]*cos(0.927*B)) + $
                2*B*(a[29]*sin(B) + a[30]*cos(B) )
        DR =  a[31]+a[32]*cos(B) + A[33]*sin(B) + $
               a[34]*cos(2*B) + a[35]*sin(2*B) + $
                a[36]*cos(1.9268*B) + a[37]*sin(1.9268*B) + $
                 a[38]*cos(0.927*B) + a[39]*sin(0.927*B) + $
                  B*(a[40]*cos(B)+ a[41]*sin(B))
        DLAT =  a[42] + a[43]*cos(B) + a[44]*sin(B) + $
                 a[45]*cos(2*b)+a[46]*sin(2*b) + $
                  B*(a[47]*cos(B) + a[48]*sin(B)) + $
                  a[49]+cos(0.927*B) + a[50]*sin(0.927*B)  
        DYAW = a[51]+a[52]*sin(B)+a[53]*cos(B) + $
               a[54]*sin(2*B) + a[55]*cos(2*B) + $
                B*(a[56]*sin(B) + a[57]*cos(B) ) + $
                a[58]*sin(0.927*B) + a[59]*cos(0.927*B)
        self.LAM = self.doc.a5 + DLON
        self.R = 42164.17478 + DR
        self.PHI = asin(DLAT)
        self.PSI = asin(DYAW)
        Start =  [62,117,172,227,282]
        FOR i=1,5 DO BEGIN 
          C = self.doc.a[start:*]

          k = findgen(C[3])*2
          j = findgen(C[34])*5
          WA = self.doc.a[60]*self.T

          signum = (T GE self.doc.a[61])
          self.ATT[i-1] = $
           C[0]* (exp( -(self.T-self.doc.a[61])/C[1]))*signum + C[2]+ $
             total( (C[2+k]*cos(WA*k/2+c[3+k]))) + $
              total( (C[32+j]*(WA-C[34+j])^C[31+j])*cos(WA*C(30+j)+C[33+j]))
        ENDFOR 
        self.roll = self.roll + self.ATT[0] + self.doc.a[15]
        self.Pitch = self.pitch + self.ATT[1] + self.doc.a[16]
        self.Yaw = self.Yaw + self.ATT[2] + self.doc.a[17]
        self.Rma = self.Att[3]
        self.Pma = self.ATT[4]
      ENDELSE 


        ; Compute Orbit Inclination (i), Argument of Latitude (u) and 
        ; Longitude of the Ascending node (ASC)

      self.I = Asin(sqrt(sin(self.PHI)^2+sin(self.PSI)^2))
      self.U = atan(sin(self.PHI),sin(self.PSI))
      self.ASC = self.LAM-self.u

         ; Computer the Subsatellite Longitude and Geodetic Latitude.

      self.RLON = self.ASC+atan(cos(self.i)*sin(self.u),cos(self.u))
      self.RLAT = atan( tan(self.PHI), (1-self.F)^2 )

      R1 = self.R*cos(self.PHI)*cos(self.LAM)/self.ae
      R2 = self.R*cos(self.PHI)*sin(self.LAM)/self.ae
      R3 = self.R*sin(self.PHI)/self.ae
      
      self.RR = [r1,r2,r3]

      ASC = self.ASC
      u = self.U
      I = self.I
      R = self.Roll
      P =  self.Pitch
      Y = self.Yaw

      tt = cos(u)*sin(i)
      ca = cos(ASC)
      sa = sin(ASC)
      su = sin(u)
      cu = cos(u)
      si = sin(i)
      ci = cos(i)

        ; If U is a pointing vector in Instrument coordinates, then 
        ; Y=B##U is the same vector in Spacecraft coordinates.
      B = [ $
           [-cA*su - sa*cu*ci, -sA*si,   -cA*cu + sA*su*ci ],$
           [-sA*su + cA*cu*ci,  ca*si,   -sA*cu - cA*su*ci ], $
           [cu*si            , -ci   ,   -su*si            ] $
          ]

      self.XS =  -B[2,*]*self.R/self.AE

        ; The program Lmodel has B[2,2] = -DLAT = -sin(self.PHI)

        ; If Y is a pointing vector in Spacecraft coordinates, then 
        ; X=B##Y is the same vector in  Geocentric coordinates.

      M = [ $
           [1-0.5*(y^2+p^2), -y             ,  p              ], $
           [y+r*p          , 1-0.5*(r^2+y^2), -r              ] ,$
           [-p+r*y         , r+p*y          , 1-0.5*(r^2+p^2) ] $
          ]

      self.BT = B##M ;Takes U to X.
      ;self-> ImageLimits
      self.status = 1l
    ENDIF ELSE  self.status = 0l
  ENDIF ELSE BEGIN 
    Message,"Please do a 'goes2->set, datetime=datetime, region=region!'",/cont
    self.status = 0L
  ENDELSE 
  return,self.status
END

;============================================
; ReadData
;============================================
FUNCTION goes2::ReadData
   IF strlen(self.datetime) NE 0 AND $
     strlen(self.region ) NE 0 AND $
     strlen(self.sensor ) NE 0 THEN BEGIN 
     raw = Goes2ReadData(self.datetime, self.region, self.sensor)
    IF n_elements(raw) NE 0 THEN BEGIN 
      self.status = 1l
      self.raw = ptr_new(raw,/no_copy)
    ENDIF ELSE  self.status = 0l
  ENDIF ELSE BEGIN 
     Message,"Please set datetime and region (and sensor if != ir4)",/cont
     Message,$
      " -- Do goes2->set,datetime=datetime,region=region [,sensor=sensor]",$
         /cont
     self.status = 0l
  ENDELSE 
  return, self.status
END

;============================================
; Cleanup
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO goes2::Cleanup
  ptr_free, self.raw
  ptr_free, self.image
END


;============================================
; Set Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO goes2::Set, datetime =  datetime, $
         region = region, $
         sensor=sensor, $
         lonpar = lonpar, $
         latpar=latpar

   IF n_elements(datetime) NE 0 THEN BEGIN 
     IF isa(datetime,/string,/nonempty) THEN BEGIN 
       self.datetime = datetime
     ENDIF ELSE BEGIN 
       Message,'DATETIME must be non-empty STRING!',/cont
     ENDELSE 
   ENDIF 

   IF n_elements(region) NE 0 THEN BEGIN 
     IF isa(region,/string,/nonempty) THEN BEGIN 
       CASE region OF 
         'east': self.region = region
         'west': self.region = region
         ELSE: BEGIN 
           Message,'Unrecognized Region <' + region + '>',/cont
           Message,'Must be "east" or "west": Region is UNSET!',/CONT
           self.region = ''
         END
       ENDCASE 
     ENDIF ELSE Message,'REGION must be non-empty STRING!',/cont
   ENDIF 

   IF n_elements(sensor) NE 0 THEN BEGIN 
     IF isa(sensor,/string,/nonempty) THEN BEGIN 
       CASE sensor OF 
         'ir1': self.sensor = sensor
         'ir2': self.sensor = sensor
         'ir3': self.sensor = sensor
         'ir4': self.sensor = sensor
         'vis': self.sensor = sensor
         ELSE: BEGIN 
           Message,'Unrecognized Sensor <' + sensor + '>',/cont
           Message,'Must be "ir{1,2,3,4}" or "vis": Sensor is UNSET!',/CONT
           self.sensor = ''
         END
       ENDCASE 
     ENDIF ELSE Message,'SENSOR must be non-empty STRING!',/cont
   ENDIF 

   IF n_elements(lonpar) EQ 3 THEN BEGIN 
     self.lonpar = lonpar
   ENDIF 
   IF n_elements(latpar) EQ 3 THEN BEGIN 
     self.latpar = latpar
   ENDIF 

END


;============================================
; Get Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO goes2::Get, datetime = datetime, $
         region=region, $
         sensor=sensor, $
         lonpar = lonpar, $
        latpar=latpar, $
        raw=raw, image=image, navigate=navigate

  IF arg_present(datetime) THEN datetime = self.datetime
  IF arg_present(region) THEN region = self.region
  IF arg_present(sensor) THEN sensor = self.sensor

  navigate = keyword_set(navigate)
  IF arg_present( lonpar ) THEN BEGIN 
    lonpar = self.lonpar
  ENDIF 

  IF arg_present( latpar ) THEN BEGIN 
    latpar = self.latpar
  ENDIF 

  IF arg_present( raw ) THEN BEGIN 
    IF ptr_valid( self.raw) THEN $
       raw = *(self.raw) ELSE Message,"Invalid Ptr! <RAW>",/cont
  ENDIF 


  IF arg_present( image ) THEN BEGIN 
    IF navigate THEN self->navigate
    IF ptr_valid( self.image) THEN $
       image = *(self.image) ELSE Message,"Invalid Ptr! <IMAGE>",/cont
  ENDIF 

END

;============================================
; Navigate. 
;  given the longitude/latitude range, get the 'image' array.
;
;============================================
PRO goes2::navigate, lonpar = lonpar, latpar=latpar 

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return
  ENDIF 

    ; All of the code in this section is straight from NOAA/NESDIS
    ; Document DRL 504-11 
    ; (NOAA/OSD3-1998-015R1UD0 March 16, 1998, DCN 1)
    ; Sections 2 through 4. Also known as the Goes Earth Location
    ; Users Guide (GELUG) or sometimes just ELUG, I've seen both.
    ; Available online at
    ; http://rsd.gsfc.nasa.gov/goes/text/goestechnotes.html
    ; About 2/3 the way down the page, the URL 
    ; http://rsd.gsfc.nasa.gov/goes/text/ELUG0398.pdf has a PDF file
    ; of the users guide.

  IF NOT (finite(self.SCNMAX) OR finite(self.ELVMAX) ) THEN BEGIN 

      ; The following two quantities come down in words 6305-6310 of
      ; GVAR Block 0, but the yymmddhhmm.txt files maintained on the
      ; UoHawaii site don't go that far, so we have to calculate them.

    ; Bias in scan angle between line/pixel and elevation/scan angle
    ; coordinate system 
    self.SCNMAX = (self.doc.imdpx-1)*SCNPX
     ; Bias in elevation  between line/pixel and elevation/scan angle
    ; coordinate system 
    self.ELVMAX = (4.5-self.doc.imdln)*ELVLN

  ENDIF 

  IF n_elements(lonpar) EQ 3 THEN self-> Set,lonpar = lonpar
  IF n_elements(latpar) EQ 3 THEN self-> Set,latpar = latpar
  IF self-> docempty() THEN BEGIN 
    Message,"Must read .doc file to navigate!",/cont
    IF NOT self-> ReadDoc() THEN self-> ReportError,"Can't Read Doc file"
  ENDIF 

  IF NOT ptr_valid( self.raw ) THEN BEGIN 
    Message,"self.raw is NULL. Reading data",/cont
    IF NOT self-> ReadData() THEN self-> ReportError,"Can't Read Data!"
  ENDIF 
  IF NOT self-> CheckMapLimits() THEN BEGIN 
   str= 'Problem with maplimits! Please fix by setting lonpar/latpar!' + $
        '(i.e. goes2-> set,lonpar=lonpar, latpar=latpar )'
   Message,str
  ENDIF 

  nlon = (self.lonpar[1]-self.lonpar[0])/self.lonpar[2]+1
  nlat = (self.latpar[1]-self.latpar[0])/self.latpar[2]+1
  lon = findgen(nlon)*self.lonpar[2] + self.lonpar[0]
  lat = findgen(nlat)*self.latpar[2] + self.latpar[0]
  PHI = atan((1-F)^2*tan(lat))

   ; Calculate the Satellite position vector, in Earth centered
   ; coordinate system (R1 through lon=0,lat=0, R2 90 degrees east,
   ; R3 throught north pole)

 R = sqrt(total(self.RR^2))
 R1 = self.RR[0]
 R2 = self.RR[1]
 R3 = self.RR[2]
   ; Calculate the earthcentered coordinates of each lat/lon point
   ; whose instrument coordinates we want.
  P = 1./(1-F)^2
  R = (1+p*sin(self.PHI)^2)
  R = 1./R

    ; Will be a nlon by nlat array
  X1 = cos(lon)#(R*cos(PHI))
  X2 = sin(LON)#(R*cos(PHI))
  X3 = replicate(1.,nlon)#(R*sin(PHI))

    ; Calculate the pointing vector for each such point
    ; A nlon by nlat by 3 array 
  W = [[[X1-R1]],[[X2-R2]],[[X3-R3]]]

  test = w[*,*,0]*RR[0] + w[*,*,1]*RR[1] + w[*,*,2]*RR[2]/(1-F)^2
  invisible = where( test GT 0, ninvisible)
  IF ninvisible NE 0 THEN $
    self-> ReportError,"Impossible points! Lat/Lon range TOO BIG!"


  print,'me here'
  TransMat = transpose(B##M)



  dim = size( *(self.raw), /dim)
  ev = ELVMAX + (4.5-lindgen(dim[1]))*ELVLN
  sc = (lindgen(dim[0]-1)*SCNPX)-SCNMAX

END

;============================================
; Error
;   Report Error
;============================================
PRO goes2::ReportError, string, routine
  IF n_params() LT 1 THEN string = 'Unspecified Error'
  IF n_params() LT 2 THEN BEGIN 
    help,calls=calls
    routine = strcompress(( str_sep( calls[1], '<' ) )[0],/remove_all)
  ENDIF 
  str =   routine + ':' + string
  Message,str,/noname
END

;============================================
; IMCisOn()
;   Check to see whether the IMC is active
;============================================
FUNCTION goes2::IMCisOn
  return, (self.doc.iscan[1] AND 1) NE 0 
END


;============================================
; IsFlipped
;   Check to see whether the Spacecraft is flipped
;============================================

FUNCTION goes2::IsFlipped
  return, (self.doc.iscan[2] AND 1) NE 0 
END

;============================================
; docempty
;   Check to see if we have the navigation parameters
;============================================
FUNCTION goes2::docempty
  return, self.doc.epochtime[0] EQ 0 AND $
           self.doc.epochtime[1] EQ 0 AND $
           self.doc.epochtime[2] EQ 0 AND $
           self.doc.epochtime[3] EQ 0 AND $
           self.doc.epochtime[4] EQ 0 AND $
           self.doc.epochtime[5] EQ 0 
END


;============================================
; ImageLimits
;   Find the lon/lat limits of image
;============================================
PRO goes2::ImageLimits
  ;npix = self.doc.IEFPX-self.doc.IWFPX+1
  dim = size(*(self.raw),/dim)
  npix = dim[0]
  nlines = dim[1]
  npix = self.doc.iefpx-self.doc.iwfpx+1    
  nlines = self.doc.isfln-self.doc.infln+1 
  tt = self-> pixlin2lonlat( findgen(npix)+self.doc.IWFPX, $
                             replicate( self.doc.IMDLN, npix) )
  xx = where( finite(reform(tt[*,0]) ),nxx )
  IF nxx ne 0 THEN self.maplimits[[0,2]]=  minmax(tt[xx,0])

  ;nlines = self.doc.ISFLN-self.doc.INFLN+1
  tt = self-> pixlin2lonlat( replicate(self.doc.IMDPX, nlines), $
                             findgen(nlines)+self.doc.INFLN )
  xx = where( finite(reform(tt[*,1]) ),nxx )
  IF nxx ne 0 THEN self.maplimits[[1,3]]=  minmax(tt[xx,1])
  a =  1
END


;============================================
; CheckMapLimits
;   Make sure the lonpar/latpar limits make sense
;============================================
FUNCTION goes2::CheckMapLimits, fix = fix
  
  fix = keyword_set(fix)
  g = where( finite(self.maplimits), ng )
  IF ng NE 4 THEN self-> ImageLimits

  x = where(lonpar,nx )
  IF nx EQ 0 THEN BEGIN 
    lonpar[0:1] =  self.maplimits[*,0]
    lonpar[2] =  (lonpar[1]-lonpar[0])/1000.
  ENDIF 

  x = where(latpar,nx )
  IF nx EQ 0 THEN BEGIN 
    latpar[0:1] =  self.maplimits[*,1]
    latpar[2] =  (latpar[1]-latpar[0])/1000.
  ENDIF 

  IF lonpar[0] LT self.maplimits[0,0] THEN BEGIN 
    Message,'Lonpar[0] too small, Minlon =  '  + $
      string(self.maplimits[0,0],form='(g10.5)' ),/cont
    IF fix THEN BEGIN 
      Message,'Lonpar[0] set to ' + $
      string(self.maplimits[0,0],form='(g10.5)' ),/cont
      self.lonpar[0] =  self.maplimits[0,0]
    ENDIF ELSE return,0
  ENDIF 

  IF lonpar[1] GT self.maplimits[0,1] THEN BEGIN 
    Message,'lonpar[1] too big, Maxlon =  '  + $
      string(self.maplimits[0,1],form='(g10.5)' ),/cont
    IF fix THEN BEGIN 
      Message,'lonpar[1] set to ' + $
      string(self.maplimits[0,1],form='(g10.5)' ),/cont
      self.lonpar[1] =  self.maplimits[0,1]
    ENDIF ELSE return,0
  ENDIF 
  


  IF latpar[0] LT self.maplimits[1,0] THEN BEGIN 
    Message,'Latpar[0] too small, Minlon =  '  + $
      string(self.maplimits[1,0],form='(g10.5)' ),/cont
    IF fix THEN BEGIN 
      Message,'Latpar[0] set to ' + $
      string(self.maplimits[1,0],form='(g10.5)' ),/cont
      self.latpar[0] =  self.maplimits[1,0]
    ENDIF ELSE return,0
  ENDIF 

  IF latpar[1] GT self.maplimits[1,1] THEN BEGIN 
    Message,'latpar[1] too big, Maxlon =  '  + $
      string(self.maplimits[1,1],form='(g10.5)' ),/cont
    IF fix THEN BEGIN 
      Message,'latpar[1] set to ' + $
      string(self.maplimits[1,1],form='(g10.5)' ),/cont
      self.latpar[1] =  self.maplimits[1,1]
    ENDIF ELSE return,0
  ENDIF 

  return,1
END


;============================================
; pixlin2lonlat
;   Convert pixel/line to lon/lat
;============================================
FUNCTION goes2::pixlin2lonlat, pixel, line,ev,sc,u,w
  tt = self-> pixlin2evsc(pixel,line)
  ndim = size(tt,/n_dim)
  IF ndim EQ 2 THEN BEGIN 
    ev = reform(tt[*,0])
    sc = reform(tt[*,1])
  ENDIF ELSE BEGIN 
    ev = tt[0]
    sc = tt[1] 
  ENDELSE 
  nn = n_elements(ev)

  tt=0
  IF self.flipped THEN BEGIN 
    E0 = EV + self.Pma*sin(EV)*(1./cos(SC)-tan(SC)) - self.Rma*(1-cos(EV)/cos(SC))
    S0 = SC - Rma*sin(EV)
  ENDIF ELSE BEGIN 
    E0 = EV - self.Pma*sin(EV)*(1./cos(SC)+tan(SC)) - self.Rma*(1-cos(EV)/cos(SC))
    S0 = SC + self.Rma*sin(EV)
  ENDELSE 

  R = self.RR
  lon = (lat=!values.d_nan)
  IF nn GT 1 THEN lon = (lat=replicate(!values.d_nan,nn))

;  FOR ii=0l,nn-1 DO BEGIN 
;    U =  [sin(S0[ii]), -sin(E0[ii])*cos(S0[ii]), cos(E0[ii])*cos(S0[ii]) ]
;    W = self.BT##U


;    Q1 = W[0]^2 + W[1]^2 + (W[2]/(1-self.F))^2
;    Q2 = W[0]*R[0] + W[1]*R[1] + W[2]*R[2]/(1-self.F)^2
;    Q3 = R[0]^2 + R[1]^2 + (R[2]/(1-self.F))^2-1
;    Descrim = Q2^2 - Q1*Q3
;    IF descrim GE 0 THEN BEGIN 
;      h = -(Q2-sqrt(descrim))/Q1
;      X = R+h*W
;      inv = 1./(1-self.F)^2
;      lat[ii] = atan( inv*X[2], sqrt( total(X[0:1]^2 ) ) )
;      lon[ii] =  atan(X[1], X[0] )
;    ENDIF ELSE BEGIN 
;      lon[ii] = (lat[ii]=!values.d_nan)
;    ENDELSE 
;  ENDFOR 

  U =  [ [sin(S0)], [-sin(E0)*cos(S0)], [cos(E0)*cos(S0) ]]
  W = self.BT##U

  denom = 1-self.F
  Q1 = reform(W[*,0]^2 + W[*,1]^2 + (W[*,2]/denom)^2)
  Q2 = reform(W[*,0]*R[0] + W[*,1]*R[1] + W[*,2]*R[2]/denom^2)
  Q3 = R[0]^2 + R[1]^2 + (R[2]/denom)^2-1
  Descrim = Q2^2 - Q1*Q3
  xx = where( descrim GE 0, nxx )
  IF nxx  NE 0 THEN BEGIN 
    h = -(Q2[xx]+sqrt(descrim[xx]))/Q1[xx]
    IF nxx EQ 1 THEN BEGIN 
      X = R+h[0]*W[xx,*]
      lat[xx] = ATAN( X[2], $
            (1-self.F)^2 * sqrt( total( X[0:1]^2 )  ) )/!dtor
      lon[xx] =  ATAN(X[1], X[0] )/!dtor

    ENDIF ELSE BEGIN 
      ;t = replicate(1.,nxx)#transpose(R)
      X = dblarr(nxx,3)
;      FOR i=0,nxx-1 DO BEGIN 
;        X[xx[i],*] =  R + h[i]*W[xx[i],*]
;      ENDFOR
      FOR i=0,2 DO X[*,i] =  h*W[xx,i] + R[i]

      lat[xx] = atan( X[*,2], $
                      (1-self.F)^2 * sqrt( $
                       (X[*,0]#[1])^2  + (X[*,1]#[1])^2) )/!dtor
      lon[xx] =  atan(X[*,1], X[*,0] )/!dtor
    ENDELSE 
    inv = 1./(1-self.F)^2
  ENDIF 
  return,reform([[lon],[lat]])
END
;============================================
; pixlin2evsc
;   Convert pixel/line to elevation/scan angles
;============================================
FUNCTION goes2::pixlin2evsc, pixel, line
  elevation = self.ELVMAX + (4.5-line)*self.ELVLN
  scanangle = (pixel-1)*self.SCNPX-self.SCNMAX
  return,reform([[elevation], [scanangle]])
END

PRO goes2::debug
   print,'here'
END

;============================================
; Definition Routine
;============================================

PRO goes2__define
  junk = {goes2, $
          datetime: "",$
          region: "", $ ; east or west 
                        ; (currently goes8 and 10, respectively)
          sensor: "", $ ; ir{1,2,3,4} or vis
          doc: {GOES2DOC}, $
          raw: ptr_new(), $
          image: ptr_new(), $
          lonpar: fltarr(3),$
          latpar: fltarr(3), $
          status: 0l, $
;
; Taken from the Earth Location User's Guide, Revision 1, march 1998
;  NOAA/NESDIS DRL 504-11
;  NOAA/OSD3-1998-015R1UD0
;
; Satellite position parameters
           FLIPPED: 0l, $ ; 1 if this spacecraft orientation is 'flipped'
           IMC  : 0L  , $ ; flag, 1= imc is on, 0=off
           LAM  : 0.0d, $ 
           R    : 0.0d, $ ; Radial Distance from Center of Earth
           RR   : dblarr(3),$ ; The S/C radius vector in Earth centered coords
           XS   : dblarr(3),$ ; normalized to Earth radius.
           BT   : dblarr(3,3), $ ; Transform Instrument to Geo Earth Centered corrds.
           mapLimits: dblarr(2,2), $; Limits of current image
           PHI  : 0.0d, $
           PSI  : 0.0d, $
           i    : 0.0d, $
           u    : 0.0d, $
           ASC  : 0.0d, $
           RLON : 0.0d, $
           RLAT : 0.0d, $
           ROLL : 0.0d, $
           PITCH: 0.0D, $
           YAW  : 0.0D, $
           Rma  : 0.0D, $ ; Roll Misalignment
           Pma  : 0.0d, $ ; Pitch Misalignment
           C1   : dblarr(5), $ ; Corrections to R/P ma.
           ATT  : dblarr(5), $ ; Corrections for R/P ma.
           T    : 0.0d, $      ; Time since epoch
    ; Items that could be in a common, if we could be assured that
    ; there'd be no conflicts

            AE      : 0.0d, $ ; Radius of Earth at Equ
            BE      : 0.0d, $ ; Radius of Earth at Pole
            F       : 0.0d, $ ; 'Flattening' Factor (1-AE/BE)
            INCMAX  : 0.0d, $ ; Number of Increments/Cycle
            SCNINC  : 0.0d, $ ; Change in scan angle/increment (MicroRads)
            ELVINC  : 0.0d, $ ; Change in elevation angle/increment (microrads) 
            SCNPX   : 0.0d, $ ; Change in scan angle/pixel (microrads)         
            ELVLN   : 0.0d, $ ; Change in elev angle/detector-line (microrads)
            SCNMAX  : 0.0d, $ ; Scan Angle Bias between lin/pix,elv/scan coords
            ELVMAX  : 0.0d }  ; Elevation Bias between lin/pix,elv/scan coords
END
