;+
; NAME:  diag.pro
; $Id$
; PURPOSE:  Return diagonal elements of a square array
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: diagonal=diag(array) 
; 
; INPUTS:  array: a squar array
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;  success: the diagonal elements, as a vector
;  failure: scalar -1.
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  none
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; PROCEDURE:  
; EXAMPLE:  
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/04/07 21:13:52  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-
;
; No Warranty, you're on your own!
;

FUNCTION diag, array
; returns the diagonal elements of ARRAY
;
diag = -1
on_error,2 ; return to caller
s =  size(array)
IF s[1] EQ  s[2] THEN BEGIN 
  n =  s[1]
  diag = array[ lindgen(n) + n*lindgen(n) ]
ENDIF ELSE message,' Array must be square ',/cont

RETURN, diag
END
