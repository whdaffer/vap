;+
; NAME:  vaptime2fts
; $Id$
; PURPOSE:  Turn time strings of the 'vaptime' format
;          (yyyy/mm/dd/hh/mm) to 'fts' (l1a Frame_Time_Secs, seconds
;          since 1993-01-01T00:00:00.000Z) 
;
; AUTHOR:  William Daffer
;
; CATEGORY:  QuikScat/Seawinds utility
;
; CALLING SEQUENCE:  fts=vaptime2fts(vaptimes)
; 
; INPUTS:  vaptimes: an array of strings of 'vaptime' format (yyyy/mm/dd/hh/mm)
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  An array of doubles.
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
FUNCTION vaptime2fts, vaptime
  IF n_params() LT 1 THEN return,0.0d
  IF NOT isa(vaptime, /string,/nonempty) THEN return,double(vaptime)*0.0d
  idldt = vaptime2idldt( vaptime )
  fts = idldt2fts(idldt)
  return,fts
END

  
