;+
; NAME:  gms5idx.pro
; $Id$
;
; PURPOSE:  Creates a index suitable for use with Scott Genari's GMS5
;          HDF files.
;
;
; AUTHOR:  William Daffer
;
;
; CATEGORY:  GMS 5 data manipulation
;
;
;
; CALLING SEQUENCE:  idx_struct=gms5idx( lonpar, latpar, minlon=minlon )
;
;
; 
; INPUTS:  
;
;   Lonpar: a 3-vector [min lon, max lon, lon increment]
;   Latpar: a 3-vector [min lat, max lat, Lat increment]
;
;
; OPTIONAL INPUTS:   None
; KEYWORD PARAMETERS:  
;
;      Minlon: (I) a scalar float. Used to differentiate between
;              'grid' files, where the longitude range is 80 to 200,
;              and 'grida', whose longitude range is 70 to
;              190. Default is 'grid' files, i.e. longitude range = 70
;              to 190.
;
; OUTPUTS:  idx_struct: A structure comprising the lon/lat index arrays.
;
;
;     idx_struct.loni = fltarr(nlon) and 
;     idx_struct.lati = fltarr(nlat) where
;
;     Where
;
;           nlon = (maxLon-minLon)/LonInc + 1, 
;           nlat = (maxLat-MinLat)/LatInc + 1. 
;
;
;     This array is used to interpolate the row/column indices given
;     in Scott Genari's 'grid' files - which give the row/column index
;     in the images of a 0.5 by 0.5 degree grid- to give the
;     row/column indices in the image of the grid defined by the input
;     lonpar/latpar variables. That is, the returned array are the
;     virtual indices to which we will interpolate the 'grid'/'grida'
;     index arrays.
;
;     I know, I hardly understand it myself.
;
;
; OPTIONAL OUTPUTS:  None
; COMMON BLOCKS:     None
; SIDE EFFECTS:      None
;
; RESTRICTIONS:      
;
;    min/max Lon must be in [70,200] range. If the minimum Longitude
;    is < 80, the max must be <= 190. This is because there are two
;    'grid's used by Genari. One (the 'grida') file goes has a
;    longitude ;range of 70 to 190. The other has a range from 80 to
;    200.
;
;    min/max Lat must be in [-60,60] range
;
;
; PROCEDURE:  
;
; EXAMPLE: 
;
;
; MODIFICATION LOG:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION gms5idx, lonpar, latpar, minlon=minlon
  ; lonpar=[min,max,incr]
  ; latpar=[min,max,incr]

  IF n_params() LT 2 THEN BEGIN 
    Usage,'latlonidx=gms5idx( lonpar, latpar )'
    return,''
  ENDIF 

  IF n_elements(lonpar) NE 3 THEN BEGIN 
    Usage,'latlonidx=gms5idx( lonpar, latpar )'
    return,''
  ENDIF 

  IF n_elements(latpar) NE 3 THEN BEGIN 
    Usage,'latlonidx=gms5idx( latpar, latpar )'
    return,''
  ENDIF 

  lonpar[ 0:1 ] =  fixlonrange( lonpar[0:1] )
  IF lonpar[0] LT 70 OR lonpar[1] GT 200 THEN BEGIN 
    Message,'Out of range longitude parameter',/cont
    print,'  lonpar must be within [80,200] '
    return,''
  ENDIF 

  IF lonpar[0] LT 80 AND lonpar[1] GT 190 THEN BEGIN 
    Message,'Out of range Longitude parameter',/cont
    print,'If min(lon) < 80 then max(lon) MUST be <= 190! (grida case)'
    return,''
  ENDIF 

  IF latpar[1] GT 60 OR latpar[0] lt -60 THEN BEGIN 
    Message,'Out of range latitude parameter',/cont
    print,'  latpar must be within [-60,60] (NB) '
    return,''
  ENDIF 
  

  IF not exist(minlon) THEN BEGIN 
    IF lonpar[0] LT 80 THEN minlon =  70 ELSE minlon =  80
  ENDIF 


  
;   latidx = (60-latpar[[1,0]])*2.
;   latidxincr = 0.5/latpar[2]
;   nlat = (latidx[1]-latidx[0])*latidxincr+1
  



;   lonidx = (lonpar[0:1]-minlon)*2.
;   lonidxincr = 0.5/lonpar[2]
;   nlon = (lonidx[1]-lonidx[0])*lonidxincr +1


  nlon = (lonpar[1]-lonpar[0])/lonpar[2]+1
  nlat = (latpar[1]-latpar[0])/latpar[2]+1

  retStruct = { loni: fltarr(nlon), lati: fltarr(nlat) }

  retStruct.loni = findgen(nlon)*lonpar[2] + lonpar[0]
  retStruct.lati = findgen(nlat)*latpar[2] + latpar[0]

  retStruct.loni = (retStruct.loni-minlon)*2
  retStruct.lati = (60-retStruct.lati)*2

  
  return, retStruct
END


  
