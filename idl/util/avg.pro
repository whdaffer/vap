;+
; NAME:  avg.pro
; $Id$
; PURPOSE:  returns mean(inarray)
;
; AUTHOR:  whd
;
; CATEGORY:  
;
; CALLING SEQUENCE:  
; 
; INPUTS:  inarray
;
; OPTIONAL INPUTS:none  
;	
; KEYWORD PARAMETERS: none
;
; OUTPUTS:  mean(inarray)
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  fails when mean(inarray) fails.
;
; PROCEDURE:  pass array to mean() without further ado.
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION avg, inarray
  return, mean(inarray)
END


