;+
; NAME:  checkinterpfile
; $Id$
; PURPOSE:  Check an Interpolated file to see if there's any voids
;
; AUTHOR:  Whd
;
; CATEGORY:  SeaWinds Utility
;
; CALLING SEQUENCE:  1|0 = checkinterpfile(numbad, badlon, badlat,
;                   file=file, u=u,v=v,lon=lon,lat=lat)
; 
; INPUTS:  none:
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
;
;  interpfile: scalar string. FQFN of the Seawinds format
;              Interpolated file (i.e. a 'QIF-*.hdf' file)
;  u,v,lon,lat: N by M float arrays. All four must be specified and
;               have the same dimensions. 
;
;  One may either enter a filename, and the data is read from it, or
;  the actual data via the u/v/lon/lat keywords.
;
; OUTPUTS:  
;  1: File has no voids
;  0: file has voids
;  2: Error
;
; OPTIONAL OUTPUTS:  
;
;  numbad: scalar. The number of bad points
;  badlon: vector. the Longitudes of the bad points
;  badlat: vector. The Latitudes of the bad points.
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
;
; PROCEDURE:  Read data, (or not, if the data is inpur via u/v/lon/lat
;            keywords) calculate the speed, find all entries where
;            the speed is `small' (i.e. le 1.e-7) and over
;            water. Create a mask array of the same dimensions as
;            'speed', with a '1' everywhere that speed has a small
;            value over water, convolve that array with a 2 by 2
;            kernel and look for spikes. These indicate where there
;            are 2 by 2 patches of zero's in the interpolated field.
; 
;            Exclude from the calculation areas that might have
;            legitimate voids, like the Red Sea or the Gulf of
;            St. Lawrence.
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION checkInterpFile, numbad, badlon, badlat,file=file, u=u,v=v,lon=lon,lat=lat
   check = 1
   badlon = (badlat=!values.f_nan)
   ;; the restricted regions. [lon,lat,radius]
   ;; We don't check these, since they are either too small to matter
   ;; or they could have legitimate voids that don't really mean the
   ;; interpolated field is bad.
   rr = [ [ 50.0,   28.0,  5], $
          [ 38.125, 21.98, 9], $
          [ 293,    48.0,  4], $
          [-61.75,  49,  4.5],$
          [-60.25, 46.5,  4 ],$
          [20.24,   60.94, 3], $
          [-86,44,7],  $
          [-80,44,4.5], $
          [-100,52,4.5], $
          [-80.5,52.4,2], $
          [-109.5,58.5,2.5], $
          [-102,57,2], $
          [-77,38,1.5], $
          [17,57,10] ]
   nrestricted = n_elements(rr[0,*])

   IF n_elements(file) EQ 0 AND $
      (n_elements(u) EQ 0 OR $
       n_elements(v) EQ 0 OR $
       n_elements(lon) EQ 0 OR $
       n_elements(lat) EQ 0) THEN BEGIN 
      Message,"Usage: 1|0|2=checkInterpFile([file=file|u=u,v=v,lon=lon,lat=lat][,numbad,badlat,badlon])",/info
     return,2
   ENDIF 

   IF n_elements(u) EQ 0 OR $
      n_elements(v) EQ 0 OR $
      n_elements(lon) EQ 0 OR $
      n_elements(lat) EQ 0 THEN BEGIN 
     qm = obj_new('qmodel',file=file)
     IF NOT obj_valid(qm) THEN BEGIN 
       Message,"Error initializing Qmodel object for file " + file,/cont
       return,2
     ENDIF 
     s = qm-> getplotdata(u,v,lon,lat)
     obj_destroy,qm
   ENDIF 

   speed = sqrt(u^2+v^2)
   bad = where(speed LE 1.e-7,nbad)
   mask = runlandmask(lon[bad],lat[bad])
   water = where(mask EQ 0, nwater)
   IF nwater NE 0 THEN BEGIN 
     
     bad = bad[water]
     speed = speed*0
     speed[bad] = 1

     kernel = replicate(1.,2,2)
     ;kernel = [ [0.5,0.5,0.5], $
     ;           [0.5,1,  0.5],
     ;           [0.5,0.5,0.5] ]
     scale = total(kernel)
     test = convol(speed,kernel,scale,/center,/edge_wrap)
     eps = 1.0d - 1.e-7
     x = where(test GE eps,nx)
     IF nx NE 0 THEN BEGIN 
       a = 1
       ;; Go through and eliminate those positives that are in the
       ;; restricted areas, i.e. close to various gulfs/bays/straights
       ;; which could have legitimate voids.
       ii = 0
       REPEAT BEGIN 
         d = sphdist( lat[x], lon[x], rr[1,ii], rr[0,ii])
         xx = where(d LT rr[2,ii],nxx)
         IF nxx NE 0 THEN test[x[xx]] = 0
         x = where( test GE eps,nx)
         ii = ii+1
       ENDREP UNTIL ii GE nrestricted OR nx EQ 0
     ENDIF 
     numbad = nx
     check = nx EQ 0
     IF nx NE 0 THEN BEGIN 
       badlon = lon[x]
       badlat = lat[x]
     ENDIF 
   ENDIF 
   return, check
END
