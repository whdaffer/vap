;+
; NAME:  psv
; $Id$
; PURPOSE:  Wrapper for pv object
;
; AUTHOR:  William Daffer
;
; CATEGORY:  
;
; CALLING SEQUENCE:  
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
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
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
;
;
PRO psv, pv, _ref_extra=_extra

IF n_params() NE 1 THEN BEGIN 
  Message,'Usage, psv,pv ',/cont
  return
ENDIF 
pv = obj_new('pv', _extra=_extra)
END
