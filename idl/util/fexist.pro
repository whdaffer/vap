;+
; NAME:  Fexist.pro
; $Id$
; PURPOSE:  Check to see if a file exists
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: 1|0 = fexist(filename) 
; 
; INPUTS:  filename - fully qualified filename
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;  1 - yes, the file exists;
;  0 - no it doesn't or the argumentation is wrong.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;William Daffer
;Copyright (c) 1999
;-
;
; No Warranties

FUNCTION fexist, filename
  IF n_params() LT 1 THEN return,0
  IF NOT isa(filename,/string,/nonempty) THEN return,0
  ff = findfile(filename,count=n)
  return, n NE 0
END
