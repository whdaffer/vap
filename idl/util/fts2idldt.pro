;+
; NAME:  fts2idldt
; $Id$
; PURPOSE:  Convert fts to idldt 
;
; AUTHOR:  whd
;
; CATEGORY:  Qscat/SeaWinds time utility
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

FUNCTION fts2idldt, fts
    ; Take the l1A parameter 'frame_time_secs' 
    ; - which is seconds since 1970/1/1/00:00:00.000,
    ; i.e. 'Unix time' and convert it to a 
    ; like dimensioned array of idldts.
  ;save_dt_base = !dt_base
  ;!dt_base = str_to_dt('1/1/1970','00:00:00.000')
  IF n_params() LT 1 THEN return,{idldt}
  idldt = replicate({idldt},n_elements(fts))
  FOR i=0,n_elements(fts)-1 DO $
    idldt[i] = sec_to_dt(fts[i],base='1/1/1993')
  ;!dt_base = save_dt_base
  IF n_elements(idldt) EQ 1 THEN idldt = idldt[0]
  return,idldt

END
