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
; Revision 1.2  1999/03/09 15:27:28  vapuser
; changed justification to /right (what *was* I thinking?)
;
; Revision 1.1  1999/03/04 18:53:37  vapuser
; Initial revision
;
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
  dt_to_var, idldt, year=year, month=month, day=day, hour=hour, min=min, sec=sec
  codea =  strtrim( year,2 ) + '-' + $
             PadAndJustify( month, 2, pad='0', /right ) + '-' + $
             PadAndJustify( day, 2, pad='0', /right ) + 'T' + $
             PadAndJustify( hour, 2, pad='0', /right ) + ':' + $
             PadAndJustify( min, 2, pad='0', /right ) + ':' + $
             PadAndJustify( sec, 2, pad='0', /right ) + '.000Z'
return,codea
END

