;+
; NAME:  dot.pro
; $Id$
; PURPOSE:  Dot product of two vectors
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Vector algebra
;
; CALLING SEQUENCE:  dotproduct=dot(v1,v2)
; 
; INPUTS:  V1 and V2, vectors having the same dimensionality
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;  Success:  dot: the dot product
;  Error: !values.f_nan
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1998, William Daffer
;-


FUNCTION dot, v1, v2
  errval = !values.f_nan

  IF n_Params() LT 2 THEN BEGIN 
    Usage, "dotproduct = dot(v1,v2)"
    return, errval
  ENDIF 

  IF n_elements(v1) NE n_elements(v2) THEN BEGIN 
    Message,'V1 and V2 must have same number of elements',/cont
    return,errval
  ENDIF 

  Catch, error
  IF error NE 0 THEN BEGIN 
    message,!error_state.msg
    return,errval
  ENDIF 

return, reform(transpose(v1)#v2)
end
