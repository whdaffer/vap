;+
; NAME:  EQX_STR
; $Id$
; PURPOSE:  Create and return a structure of type EQX
;
; AUTHOR:  William Daffer
;
; CATEGORY:   Qscat Utility.
;
; CALLING SEQUENCE:  str=eqx_str([n])
; 
; INPUTS:  the number of EQX structure you want (default=1)
;
; OPTIONAL INPUTS: 
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  struct: A str of type EQX
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
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION eqx_str, nstruct

COMMON eqx_cmn, eqx_defined, eqx_size, eqx
;
IF n_elements( eqx_defined ) eq 0 THEN BEGIN

  eqx = { EQX,$
          date: '',$
          time: '',$
          lon: 0.0 }

  eqx_defined = 1

ENDIF

IF n_elements( nstruct ) eq 0 THEN nstruct = 1
IF nstruct le 0 THEN nstruct = 1
eqx_size =  n_tags(eqx,/length) ; record length in bytes.
RETURN, replicate( eqx , nstruct )
end





