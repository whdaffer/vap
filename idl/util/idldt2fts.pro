;+
; NAME:  idldt2fts
; $Id$
; PURPOSE:  Convert an IDLDT time to FTS 
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Qscat/SeaWinds time utility
;
; CALLING SEQUENCE:  fts=idldt2fts(dt)
; 
; INPUTS:  dt: An array of IDL dt times
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  An Array of L1A Frame_Time_Secs (Seconds since 1993/01/01 00:00:00.000)
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:   Currently, this routine doesn't account for leap
;               seconds, which L1A Frame_Time_Secs does. This means
;               they're will be about a 4 second discrepency with some
;               other programs. I know, it's a problem, but I'll fix
;               it later.
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

FUNCTION idldt2fts, dt
  ; Convert and Array of IDL DT variables to L1A Frames_Time_Secs.
  ; 
  ; Have to do it ourselves, since there is no inverse for the function
  ; sec_to_dt.pro

  IF n_params() LT 1 THEN BEGIN 
    Usage,'Unix_seconds=idldt2fts(dt)'
    return,-1
  ENDIF 

  secs_per_day = 24.0d*60.*60.
  secs1 =  dt.julian*secs_per_day
  l1abase = str_to_dt('1/1/1993','00:00:00.000')
  secs2 = l1abase.julian*secs_per_day
  fts = secs1-secs2
  return,fts
END
