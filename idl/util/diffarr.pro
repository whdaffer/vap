;+
; NAME:  diffarr.pro
; $Id$
; PURPOSE: Return the difference between each element of an array and
;          it's neighbor. Useful in finding discontinuities in
;          monotonic data.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: diff = diffarr(A) 
; 
; INPUTS:  A: An array.
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;  success: the difference A[i+1] - A[i], i=0,2,... n_elements(a)-2
;  failure: !values.f_nan
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  The routine fill fail if the 'difference' operation
;                isn't appropriate. But, it'll still return !values.f_nan
;
; PROCEDURE:  
; EXAMPLE:  
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/04/07 21:23:45  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION diffarr, array
nn =  n_elements( array )

IF nn eq 0 THEN BEGIN 
  Message,'Usage: diff=diffarr( array )',/cont
  return,!Values.F_Nan
ENDIF 

catch, error
IF error NE 0 THEN BEGIN 
  Message,!error_state.msg,/cont
  return,!Values.F_Nan
ENDIF 

diff =  abs(array[1:*]-array)
return,diff
END
