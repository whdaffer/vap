;+
; NAME:  JD2Vaptime
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
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1.1.1  2001/12/03 18:09:57  vapuser
; Imported sources
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1407 is acknowledged.
;-

FUNCTION jd2vaptime, jd
  IF n_params() LT 1 THEN return,0
  njd = n_elements(jd)
  IF njd EQ 0 THEN return,0
  IF NOT isa(jd,/double) THEN return,0
  vaptime = idldt2vaptime( jul_to_dt( jd ) )
  IF njd EQ 1 THEN vaptime = vaptime[0]
  return, vaptime
END
