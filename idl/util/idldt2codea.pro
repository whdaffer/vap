;+
; NAME:  idldt2codea
; $Id$
; PURPOSE:  Convert IDLDT structures to codea (yyyy-mm-ddThh:mm:ss.cccZ)
;
; AUTHOR:  William Daffer
;
; CATEGORY: Time representation
;
; CALLING SEQUENCE:  codea=idldt2codea(idldt)
; 
; INPUTS:  idldt - and array of idldt structures
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  codea - an array of codea strings
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
;Copyright (c) 1999, William Daffer
;-

FUNCTION idldt2codea, idldt
  IF n_params() LT 1 THEN begin
    Usage,"codea=idldt2codea(idldt)"
    return,0
  ENDIF 
  IF NOT isa( idldt, /structure, name="IDLDT" ) THEN BEGIN 
    Message,'Paramter must be a STRUCTURE of type IDLDT',/CONT
    return,0
  ENDIF 
  
  nn = n_elements(idldt)
  codea =  strarr( nn )
  dt_to_var, idldt, year=year, month=month, day=day, hour=hour, min=min, sec=sec
  codea =  strtrim( year,2 ) + '-' + $
             PadAndJustify( month, 2, pad='0', /left ) + '-' + $
             PadAndJustify( day, 2, pad='0', /left ) + 'T' + $
             PadAndJustify( hour, 2, pad='0', /left ) + ':' + $
             PadAndJustify( min, 2, pad='0', /left ) + ':' + $
             PadAndJustify( sec, 2, pad='0', /left ) + '.000Z'
return,codea
END

