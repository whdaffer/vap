;+
; NAME:  RunLandmask
; $Id$
; PURPOSE:  Wrapper for idl Linkimage routine Landmask
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Damned if I know
;
; CALLING SEQUENCE:  mask = RunLandmask( lon, lat )
; 
; INPUTS:  
;  Lon - Float vector of longitudes
;  Lat - Corresponding Lattitudes
;
; OPTIONAL INPUTS:  None
;       
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  
;   Failure: -1l
;   Mask: Longword Array of the same dimensions as Lon. 1=land, 0=water
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  
;
;   The Arrays are converted to Float, if not already.
;   If Lon/Lat are passed as scalars, they are converted to 1
; elemenent arrays.
;
; RESTRICTIONS:  
;
;   -180<=Longitude<=360
;   -90<=Latitude<=90.
;
;  land_mask.so reads the data file
;  ~vapuser/idl/linkimage/LANDMASK.DAT
;
;
;
; PROCEDURE:  
;
; EXAMPLE: 
;
;   Lon=findgen(2,3)*360/5.
;   Lat=findgen(2,3)*90/5.
;   Mask=runLandMask(lon,lat)
;   help,mask 
;      MASK            LONG      = Array[2, 3]
;   Print,Mask
;           0           0
;           0           0
;           0           0
;
;
;
; $Log$
;
; Jet Propulsion Laboratory
; Copyright (c) 1998, California Institute of Technology
; Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION RunLandmask, lon, lat
  mask = -1
  IF n_params() LT 2 THEN BEGIN 
    Message,'Usage: mask=RunLandMask(lon,lat)'
    return,mask
  ENDIF 

  IF n_elements(lon) EQ 0 OR n_elements(lat) EQ 0 THEN BEGIN 
    Message,"Both Lon and Lat must EXIST",/cont
    return,-1
  ENDIF 


  IF n_Elements(lon) NE  n_Elements(lat) THEN BEGIN 
    Message,"Lon and Lat must have same number of elements!",/cont
    return,-1
  ENDIF 
  

  IF size(lon,/n_dim) EQ 0 THEN BEGIN 
    tlon = fltarr(1)
    tlon[0] = lon
    lon = tlon
  ENDIF 

  IF size(lat,/n_dim) EQ 0 THEN BEGIN 
    tlat = fltarr(1)
    tlat[0] = lat
    lat = tlat
  ENDIF 

  lonrange = minmax(lon)
  x=where(lonrange GT 360 OR lonrange LT -180,nx )
  IF nx NE 0 THEN BEGIN 
    Message,'Out of range Longitude, must be -180<lon<360.',/cont
    return,-1
  ENDIF 

  latrange = minmax(lat)
  x=where(latrange GT 90 OR latrange Lt -90,nx )
  IF nx NE 0 THEN BEGIN 
    Message,'Out of range Latitude, must be -90<lat<90.',/cont
    return,-1
  ENDIF 


  IF Vartype(lon) NE 'FLOAT' THEN lon = float(lon) 
  IF Vartype(lat) NE 'FLOAT' THEN lat = float(lat)
  mask = long(lon*0l)

    ; Run the linkimage routine land_mask.so
  Land_Mask, lon, lat, mask
    
  
  return, mask
END
