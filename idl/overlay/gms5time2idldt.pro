;+
; NAME:  gms5time2idldt.pro
; $Id$
;
; PURPOSE:  Converts the file times of Scott Genari's files into IDLDT
;
; AUTHOR: WHD
;
; CATEGORY:  Qscat VAP/GMS 5 processing
;
; CALLING SEQUENCE:  idldt=gms5time2idldt(datetimes)
; 
; INPUTS:  Datetimes: a string array of datetimes of the sort Scott
;         Genari uses for his GMS 5 file names
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  
;  failure: a scalar 0
;           success: an array of IDLDTs of the same dimensionality as the input array.
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  The time strings must be of the form yymmddhhmm
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

FUNCTION gms5time2idldt, datetime
  IF n_params() LT 1 THEN BEGIN 
    Usage,'IDLDT = gms5time2idldt( datetime )'
    return,0
  ENDIF 
     
  year = fix(strmid(datetime,0,2))
  mon  = fix(strmid(datetime,2,2))
  day  = fix(strmid(datetime,4,2))
  hh   = fix(strmid(datetime,6,2))
  mm   = fix(strmid(datetime,8,2))

  x = where( (mon LE 0 OR mon GT 12 ) OR $
             (day LE 0 OR day GT 31 ) OR $
             (hh LT 0 OR hh GT 23 ) OR $
             (mm LT 0 OR mm GT 59 ), nx )
  idldt = 0
  
  IF nx EQ  0 THEN $
    idldt = var_to_dt(year,mon,day,hh,mm) $
  ELSE $
    Message,'Bad Time! One or more fields out of range!',/cont

  return,idldt

END
