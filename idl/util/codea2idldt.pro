;+
; NAME:  CodeA2idldt.pro
; $Id$
; PURPOSE:  Convert Code A format time strings
;          (yyyy-mm-ddThh:mm:ss.cccZ) to idldt format
;
; AUTHOR: William Daffer  
;
; CATEGORY:  Time manipulation
;
; CALLING SEQUENCE:  idldt=CodeA2IdlDt(codeA)
; 
; INPUTS:  CodeA: An array of Code As
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
; $Log$
; Revision 1.2  1999/07/03 21:47:37  daffer
; Corrected call to var_to_dt
;
; Revision 1.1  1999/04/07 16:14:14  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION CodeA2idldt, codea, check=check

  IF n_params() LT 1 THEN BEGIN 
    Usage,'IDLDT=CodeA2idldt(codea)'
    return,0
  ENDIF 


  IF NOT isa(codea,/string)  THEN BEGIN 
    Message,'Argument must be of type STRING',/cont
    return,0
  ENDIF 

;  catch,error
;  IF error NE 0 THEN BEGIN 
;    Message,!error_state.msg,/cont
;    return,0
;  ENDIF 

  nn = n_elements(codea)

  IF keyword_set(check) THEN BEGIN 
    FOR i=0L,nn-1 DO BEGIN 
      tmp = strsplit( strtrim( strcompress(codea[i]),2), 'T',/extract)
      IF n_elements(tmp) NE 2 THEN BEGIN 
        Message,"Bad CodeA, no 'T'",/cont
        Message,"Format is yyyy-mm-ddThh:mm:ss.cccZ",/cont
        return,0
      ENDIF 
      tmp1 = strsplit(tmp[0],'-',/extract)
      IF n_elements(tmp1) NE 3 THEN BEGIN 
        Message,"Bad CodeA, first part doesn't look like 'yyyy-mm-dd'",/cont
        Message,"Format is yyyy-mm-ddThh:mm:ss.cccZ",/cont
        return,0
      ENDIF 
      tmp2 = strsplit(tmp[1],':',/extract)
      IF n_Elements(tmp2) NE 3 THEN BEGIN 
        Message,"Bad CodeA, second part doesn't look like 'hh:mm:ss.cccZ'",$
          /cont
        Message,"Format is yyyy-mm-ddThh:mm:ss.cccZ",/cont
        return,0
      ENDIF 
    ENDFOR 
  ENDIF 


    ; CodeA looks like this
    ; 1998-12-22T06:48:57.831Z
  returndt =  0 

    ;           1         2
    ; 012345678901234567890123456789
    ; yyyy-mm-ddThh:mm:ss.cccZ

  year  = strmid( codea, 0,  4 )
  month = strmid( codea, 5,  2 )
  day   = strmid( codea, 8,  2 )
  hour  = strmid( codea, 11, 2 )
  min   = strmid( codea, 14, 2 )
  sec   = strmid( codea, 17, 2 )
  ccc   = strmid( codea, 21, 3 )
  returndt =  Var_To_dt( year, month, day, hour, min, sec)

;  FOR i=0,nn-1 DO BEGIN 
;    year = fix(tmp1[0])
;    month = fix(tmp1[1])
;    day = fix(tmp1[2])
;    hour = fix(tmp2[0])
;    minute = fix(tmp2[1])
;    second = fix( (str_sep( tmp2[2], '.'))[0] )
;    returndt[i] =  
;  ENDFOR 

  return, returndt
END
