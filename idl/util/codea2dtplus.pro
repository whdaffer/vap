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
;Copyright (c) 1999, William Daffer
;-
FUNCTION CodeA2DtPlus, codea
  IF n_params() EQ 0 THEN return, {dtplus}
  IF NOT isa(codea, /string, /nonempty) THEN BEGIN 
    Message,"CodeA must be NON-EMPTY STRING",/CONT
    return, replicate( {dtplus}, n_elements(codea) )
  ENDIF 

  nn = n_elements(codeA)
  dtplus = replicate({dtplus},nn)
  FOR c=0,nn-1 DO BEGIN 
    idldt = codeA2idldt(codea[c])
    dtplus[c].idldt = idldt[0]
    tmp = str_sep(codea[c],'.')
    usec = fix(strmid(tmp[1],0,3))
    dtplus[c].usec = usec
  ENDFOR 

  IF nn EQ 1 THEN dtplus = dtplus[0]
  return,dtplus

END
