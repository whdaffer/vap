;+
; NAME:  fts2vaptime
; $Id$
; PURPOSE:  Convert L1A frame_time_secs (seconds since 19993-01-01T00:00:00.000Z)
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Qscat/Seawinds Vap utility
;
; CALLING SEQUENCE:  vaptimes = fts2vaptime( fts )
; 
; INPUTS:  fts: Array of doubles
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;    Success: An array of vaptimes, strings of the form
;             yyyy/mm/dd/hh/mm, of the same dimensionality at the
;             input fts array.
;
;    Error: An empty string or array of strings
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
FUNCTION fts2vaptime, fts
  IF n_params() LT 1 THEN return,''
  IF NOT isa(fts, /number) THEN BEGIN 
    tt = string(fts*0 )
    tt[*] = ''
    return,tt
  ENDIF 
  idldt = fts2idldt(fts)
  dt_to_var, idldt, year=year, month=month, day=day, hour=hour, minute=minute
  vaptime = string(fts*0)
  vaptime[*] = ''
  nn = n_elements(fts)
  FOR i=0,nn-1 DO BEGIN 
    vaptime[i] =  strtrim(year[i],2) + '/' + $
      padAndJustify(month[i],2,pad='0',/right) + '/' + $
       padAndJustify(day[i],2,pad='0',/right) + '/' + $
        padAndJustify(hour[i],2,pad='0',/right) + '/' + $
         padAndJustify(minute[i],2,pad='0',/right)
       
  ENDFOR 
  IF nn EQ 1 THEN vaptime = vaptime[0]
  return,vaptime
END
