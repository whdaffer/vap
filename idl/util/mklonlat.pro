;+
; NAME:  mklonlat.pro
; $Id$
; PURPOSE:  Given min/max/inc lon and lat values, return structure
;          with the arrays.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  General map manipulation stuff
;
; CALLING SEQUENCE:  retstruct=mkLonLat(lonpar,latpar)
; 
; INPUTS:  
;
;  Lonpar: a 3 vector [ minlon,maxlon,loninc]
;  Latpar: a 3 vector [ minlat,maxlat,latinc]
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;  success: a structure. 
;           retstruct.lon = a 2 dim array of the correct shape
;           containing the longitudes, simile for retstruct.lat
;
;  failure: the empty string
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  none
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION mklonlat, lonpar, latpar

  retstruct = ''
  IF n_params() LT 2 THEN BEGIN 
    Usage,"retstruct=mklonlat(Lonpar, latpar)",/cont
    return, ''
  ENDIF 

  IF n_Elements( lonpar ) LT 3 THEN BEGIN 
    Message,'Lonpar must be 3-vector [nlon, lon0, increment ]',/cont
    return, ''
  ENDIF 

  IF n_Elements( latpar ) LT 3 THEN BEGIN 
    Message,'Lonpar must be 3-vector [nlat, lat0, increment ]',/cont
    return, ''
  ENDIF 

  nlon = lonpar[0]
  lon0 = lonpar[1]
  loninc = lonpar[2]

  nlat = latpar[0]
  lat0 = latpar[1]
  latinc = latpar[2]

  lon = (findgen(nlon)*loninc+lon0)#(replicate(1.,nlat))
  lat = replicate(1.,nlon)#(findgen(nlat)*latinc+lat0)

  retstruct =  { lon: lon, lat: lat}
  return, retstruct
  END

  
