;+
; NAME:  
; $Id$
; PURPOSE:  
;
;
; AUTHOR:
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  
;
;
; 
; INPUTS:  
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:  
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
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
;
FUNCTION dtcompare, testtime_dt, idldt, op
    ; All times are IDLDT format
    ; testtime_dt - (a scalar) the time to be compared against 
    ; idldt - array of times to compare.
  lf =  string( 10b )
  IF !version.release LT 5.1 THEN return,-1
  IF n_params() NE 3 THEN return,-1
  result = -1
  IF VarType( testtime_dt ) EQ 'STRUCTURE' AND $
      VarType( idldt ) EQ 'STRUCTURE' THEN BEGIN 
    IF Tag_Names( testtime_dt,/STRUCTURE_NAME ) EQ 'IDLDT' and $
       Tag_Names( idldt[0],/STRUCTURE_NAME ) EQ 'IDLDT' THEN BEGIN 
      jd1 = testtime_dt.julian 
      jd2 =  idldt.Julian 
      exe_str =  'result = jd1 ' + op + ' jd2'
      IF NOT execute( exe_str ) THEN BEGIN 
        Message,"Failure executing " + lf + '    ' + exe_string
        return,-1
      ENDIF 
    ENDIF ELSE Message,"Both parameters must be type 'IDLDT'",/cont
  ENDIF ELSE Message,"Both parameters must be STRUCTURES",/cont
  return, result
END

