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
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION rnoaagaps, filelist,regions=regions
  gaps = ''
  IF n_params() LT 1 THEN BEGIN 
    Usage,"gaps=rnoaagaps(list_of_MGDR_files)"
    return,''
  ENDIF 
  IF NOT isa(filelist,/string,/nonempty) THEN BEGIN 
    Message,"Parameter FILELIST must be NON-EMPTY STRINGS",/cont
    return,''
  ENDIF 
  nf = n_elements(filelist)
  dt = wfnames2dt(filelist)
  
  diff = (dt[1:nf-1].start_time.julian - dt[0:nf-2].end_time.julian)*(24*60.)
  x = where(diff GT 10, nx )
  IF nx NE 0 THEN BEGIN 
    gaps = strarr(2,
  ENDIF 
  return, ''
END
