;+
; NAME:  utc2vaptime
; $Id$
; PURPOSE:  Converts a vector of time strings of the form
;          yyyy-doyThh:mm:ss.ccc to to  one of the form yyyy/mm/dd/hh/mi
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  Qscat 
;
;
;
; CALLING SEQUENCE: vaptime=Utc2Vaptime( utc )
;
;
; 
; INPUTS:  
;
;   utc : A vector of time strings having the UTC format
;         yyyy-doyThh:mm:ss.ccc, e.g. 1998-033T02:03:04.567
;
;
;
; OPTIONAL INPUTS:   None
;
;
;	
; KEYWORD PARAMETERS:  None
;
;
;
; OUTPUTS:  A vector of the same dimensionality where the strings have
;          been converted to vaptime format yyyy/mm/dd/hh/mi.
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
; Revision 1.1  1998/10/05 23:49:16  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION utc2vaptime, utc

  rcsid = "$Id$"
  vaptime = ''
  
  IF n_Params() LT 1 THEN BEGIN 
    Message,'Usage, vaptime=utc2vaptime(utc)',/cont
    return,''
  ENDIF 
  n1 = n_elements(utc)
  vaptime = strarr(n1)
  s = '/'
  FOR i=0,n1-1 DO BEGIN 
    tmp  = strsplit( utc[i], 'T' ,/extract)
    tmp2 = strsplit( tmp[0], '-',/extract)
    tmp3 = strsplit( tmp[1], ':',/extract)
    year = tmp2[0]
    doy = fix(tmp2[1])
    date = doy2date( fix(year),doy)
    month = date[0]
    day = date[1]
    hh = tmp3[0]
    n2 = n_elements(tmp3)
    IF n2 GE 2 THEN mm = tmp3[1] ELSE mm = '00'
    vaptime[i] = year + s + month + s + day + s + hh + s + mm
  ENDFOR 
    
  return, vaptime
END
