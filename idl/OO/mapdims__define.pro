;+
; NAME:  MapDims__Define
; PURPOSE:  Defines the MapDims object, which handles map dimensions
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  OO
;
;
;
; CALLING SEQUENCE:  MapDims=Obj_New('MapDims',Lon,Lat, fix = fix)
;
;
; 
; INPUTS:   
;
;      Lon - 2 vector, [min,max], the Longitude
;      Lat - 2-vector, [min,max], the Latitude
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;        Fix: (Flag) if set, check the longitude to make sure that
;             max-min produces a sensible range.
;
;
;
; OUTPUTS:  if successful, an object of type MAPDIMS
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;
;
; SIDE EFFECTS:  
;
;
;
; RESTRICTIONS:  
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;====================================
; Init
;====================================

FUNCTION MapDims::init, Lon,Lat, fix = fix
  status = 0
  self.fix =  keyword_set( fix )

  IF N_Elements(Lon) eq 2  AND N_Elements(Lat) EQ 2 THEN BEGIN 

    self.flon = self->FixLonRange(Lon) 
    self.flat = self->FixLatRange(Lat)
    IF self.fix  THEN BEGIN 
      self.lon = self.flon 
      self.lat = self.flat
    ENDIF ELSE BEGIN 
      self.lon =  Lon
      self.lat =  Lat
    ENDELSE 
    self->CaculateCenter
    status = 1

  ENDIF ELSE Message,"Either Lon or Lat ARE NOT 2-vectors",/cont
  RETURN,status
END

;====================================
; FixLonRange
;====================================


FUNCTION MapDims::fixlonrange, lonrange
  return, FixLonRange(LonRange)
END


;====================================
; FixLatRange
;====================================


FUNCTION MapDims::fixLatrange, Latrange
  return, FixLatRange(LatRange)
END


;====================================
; Calculate the Center, given lon,lat
;====================================


PRO MapDims::CaculateCenter
  self.center[0] = total( self.flon )/2.
  self.center[1] = total(self.lat)/2.
END


;====================================
; Cleanup (blank, but speeds up processing)
;====================================


PRO MapDims::Cleanup
END

;====================================
; Get 
;====================================
PRO MapDims::Get, lon = lon,lat=lat,flon=flon, center=center,fix=fix

  lon = self.lon
  flon = self.flon
  lat = self.lat
  center = self.center
  fix = self.fix

END

;====================================
; Set
;====================================
PRO MapDims::Set, lon = lon, lat=lat, center=center, fix=fix

   self.fix = keyword_set(fix)

   IF n_elements(lat) NE 0 THEN BEGIN 
     lat =  double(lat) 
     IF N_Elements( lat ) EQ 2 THEN BEGIN 
       self.flat =  self->FixLatRange( lat )
       IF self.fix THEN self.lat =  self.flat
     ENDIF ELSE Message,'Lat must be a 2-Vector',/cont
   ENDIF 

   IF N_Elements(lon) NE 0 THEN BEGIN 
     lon = double(lon) 
     IF N_Elements( lon ) EQ 2 THEN BEGIN 
       self.flon =  self->FixLonRange( lon ) 
       IF self.fix THEN self.lon = self.flon
     ENDIF ELSE Message,'Lon must be a 2-Vector',/cont
   ENDIF 

   IF N_Elements(center) NE 0 THEN BEGIN 
     center = double(center) 
     IF N_Elements( center ) EQ 2 THEN $
      self.center = center ELSE $
      Message,'Center must be a 2-Vector',/cont
   ENDIF 
END


;====================================
;Print 
;====================================
PRO MapDims::Print
  print,'Lon       = ',self.lon
  print,'Lat       = ',self.lat
  print,'Fix Lon = ',self.flon
  print,'Fix Lat = ',self.flat
  print,'Center    =',self.center
  print,'Fix Flag= ',self.fix
END

;====================================
; FixLon
;====================================
PRO MapDims::FixLon
  self.lon =  self.flon
END

;====================================
; FixLat
;====================================
PRO MapDims::FixLat
  self.lat =  self.flat
END


;============================================
; Version
;============================================

FUNCTION MapDims::Version
   rcsid = "$Id$"
   super = Obj_Class(self,/Super,count=cnt)
   IF cnt NE 0 THEN BEGIN 
     versions = strarr(cnt+1)
     versions[0] = rcsid
     FOR i=0,cnt-1 DO versions[i] = call_method("VERSION",super[i])
     return,versions
   ENDIF ELSE return,rcsid
END


;====================================
; Class Definition
;====================================


PRO MapDims__define

     ; the 'fix' ranges are the true range, e.g. a Longitude of
     ; [350,10] would be [-10,10] when fixed. True, there probably
     ; isn't much use for the ''fixed' Latitude. I put them there in
     ; the case that someone might actually want to use something
     ; other than the 'fix' range. Set the 'fix' flag, if you always
     ; want to use the fixed quantities. This will set
     ; self.lon=self.flon and self.lat=self.flat

   junk = { MAPDIMS,$
            lon    : dblarr(2), $   ; Longitude range
            lat    : dblarr(2), $   ; Latitude range
            center : dblarr(2), $   ; center of lon/lat area.
            flon   : dblarr(2), $   ; Fixed Longitude Range 
            flat   : dblarr(2), $   ; Fixed Latitude Range 
            fix  : 0           }    ; if set, always fix the lon range.
END
