;+
; NAME:  
; $Id$
; PURPOSE:  
; 
; AUTHOR: 
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
; MODIFICATION LOG:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION fts2codeA, fts

  IF n_params() LT 1 THEN return,''
  IF NOT isa(fts,/double) THEN return,''
  nf = n_elements(fts)
  codeA = strarr(nf)
  FOR f=0,nf-1 DO BEGIN 
    codeA[f] = idldt2codea( fts2idldt( fts[f] ))
  ENDFOR 
  IF nf EQ 1 THEN codea = codea[0]
  return, codea

END
