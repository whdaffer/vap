;+
; NAME:  Ccsds2idldt.pro
; $Id$
; PURPOSE:  Convert Code A format time strings
;          (yyyy-dddThh:mm:ss.cccZ) to idldt format
;
; AUTHOR: William Daffer  
;
; CATEGORY:  Time manipulation
;
; CALLING SEQUENCE:  idldt=Ccsds2IdlDt(ccsds)
; 
; INPUTS:  Ccsds: An array of Code As
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  
;
;  check: flag, check for format soundness. Otherwise, the routine
;         assumes the strings are the correct format.
;
; OUTPUTS:  
;
;  success: An array of IDLDTs of the same dimensionality as the input
;           array.
;  failure: a scalar 0.
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
;Jet Propulsion Laboratory
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION Ccsds2idldt, ccsds, check=check

  IF n_params() LT 1 THEN BEGIN 
    Usage,'IDLDT=Ccsds2idldt(ccsds)'
    return,0
  ENDIF 


  IF NOT isa(ccsds,/string)  THEN BEGIN 
    Message,'Argument must be of type STRING',/cont
    return,0
  ENDIF 

;  catch,error
;  IF error NE 0 THEN BEGIN 
;    Message,!error_state.msg,/cont
;    return,0
;  ENDIF 

  nn = n_elements(ccsds)

  IF keyword_set(check) THEN BEGIN 
    FOR i=0L,nn-1 DO BEGIN 
      tmp = strsplit( strtrim( strcompress(ccsds[i]),2), 'T',/extract)
      IF n_elements(tmp) NE 2 THEN BEGIN 
        Message,"Bad Ccsds, no 'T'",/cont
        Message,"Format is yyyy-dddThh:mm:ss.cccZ",/cont
        return,0
      ENDIF 
      tmp1 = strsplit(tmp[0],'-',/extract)
      IF n_elements(tmp1) NE 2 THEN BEGIN 
        Message,"Bad Ccsds, first part doesn't look like 'yyyy-ddd'",/cont
        Message,"Format is yyyy-dddThh:mm:ss.ccc",/cont
        return,0
      ENDIF 
      tmp2 = strsplit(tmp[1],':',/extract)
      IF n_Elements(tmp2) NE 3 THEN BEGIN 
        Message,"Bad Ccsds, second part doesn't look like 'hh:mm:ss.ccc'",$
          /cont
        Message,"Format is yyyy-dddThh:mm:ss.ccc",/cont
        return,0
      ENDIF 
    ENDFOR 
  ENDIF 


    ; Ccsds looks like this
    ; 1998-220T06:48:57.831Z
  returndt =  0 

    ;           1         2
    ; 012345678901234567890123456789
    ; yyyy-dddThh:mm:ss.cccZ

  year  = fix(strmid( ccsds, 0,  4 ))
  doy = fix(strmid( ccsds, 5,  3 ))
  month =  (day= intarr(nn))
  FOR i=0l,nn-1  DO BEGIN 
    tmp = doy2date(year[i],doy[i])
    month[i] = fix(tmp[0])
    day[i] = fix(tmp[1])
  ENDFOR 

  hour  = fix(strmid( ccsds, 9, 2 ))
  min   = fix(strmid( ccsds, 12, 2 ))
  sec   = float(strmid( ccsds, 15, 6 ))
  returndt =  Var_To_dt( year, month, day, hour, min, sec)

  return, returndt
END
