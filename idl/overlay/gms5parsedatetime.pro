;+
; NAME:  Gms5ParseDatetime
; $Id$
; PURPOSE:  Parse a datetime string (yymmddhhmm) and return a
;          structure with the info broken down.
; 
; AUTHOR: William Daffer
; 
; CATEGORY:  Qscat VAP GMS 5 Image Processing
; 
; CALLING SEQUENCE:  datetime_struct = gms5parsedatetime(datetime)
; 
; INPUTS:  
;
;    datetime: String[s] of form YYMMDDHHMM, like the one's Scott
;              Genari uses as filenames on his GMS 5 web site.
;
; OPTIONAL INPUTS:  None
;       
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  
;
;    Success: A structure of type GMS5DATETIME and of same
;             dimensionality as input array with the datetimes broken
;             down into constituent parts. 
;
;    Failure: a scalar 0.
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  None
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION LOG:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION Gms5ParseDatetime, datetime
   
  IF n_params() LT 1 THEN BEGIN 
    Usage,'datetime_str=Gms5ParseDatetime(datetime)'
    return,0
  ENDIF 

  IF NOT isa(datetime, /string, /nonempty) THEN BEGIN 
    Message,'Input parameter must be NONEMPTY STRING',/cont
    return,0
  ENDIF 
  nf = n_elements(datetime)
  datetime_str =  Gms5Datetime_Str(nf)

  FOR d=0,nf-1 DO BEGIN 
    yy = fix(strmid(datetime,0,2))
    IF yy LT 98 THEN yy = yy+2000 ELSE yy = yy+1900
    datetime_str[d].year  =  strtrim(yy,2)
    datetime_str[d].month = PadAndJustify(strmid(datetime,2,2),2,pad='0',/right)
    datetime_str[d].day   = PadAndJustify(strmid(datetime,4,2),2,pad='0',/right)
    datetime_str[d].hour  = PadAndJustify(strmid(datetime,6,2),2,pad='0',/right)
    datetime_str[d].min   = PadAndJustify(strmid(datetime,8,2),2,pad='0',/right)
  ENDFOR 
  return, datetime_str
END

