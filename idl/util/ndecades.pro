;+
; NAME:  William Daffer
; $Id$
; PURPOSE:  ndecades.pro
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: ndec=ndecades( array) 
; 
; INPUTS:  array: A array of numbers.
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  The dynamic range, in order's of magnitude, of the data in
;          'array'
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  none
; SIDE EFFECTS:  none
; RESTRICTIONS:  
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1997, William Daffer
;-
; No Warranties!
;
;
FUNCTION ndecades, inarray

  errval = -1
  IF n_Params() LT 0 THEN BEGIN 
    Message,'Usage: ndecades=decades( inarray )',/cont
    return,-1
  ENDIF 
  
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_State.msg
    return,-1
  ENDIF 

  IF NOT isa(inarray,/type_integer) AND  $
     NOT isa(inarray,/type_float) AND $
     NOT isa(inarray,/type_complex)  THEN BEGIN 
    Message,"Input Array must NUMBERS!",/CONT
    return,-1
  ENDIF 
  mm = MinMax(Inarray)
  return,Max(fix( alog10(abs(mm)) )+1)
 END
