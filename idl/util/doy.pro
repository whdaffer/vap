;+
; NAME:  Doy
; $Id$
; PURPOSE:  Return the Date-of-year of the given IDLDT structure(s)
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  Time Manipulation
;
;
;
; CALLING SEQUENCE:  doy = doy(idldt)
;
;
; 
; INPUTS:  IDLDT : array of IDL DT variables
;
;
;
; OPTIONAL INPUTS:  None
;
;	
; KEYWORD PARAMETERS:  None
;
;
;
; OUTPUTS:  doys : Dates-of-year
;
;
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  None
;
;
;
; RESTRICTIONS:  None
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION doy, idldt

  IF n_params() EQ 0 THEN BEGIN 
    Message,'Usage: doy=doy(idldt)',/cont
    return,0
  ENDIF 

  IF vartype(idldt) NE 'STRUCTURE' THEN BEGIN 
    Message,'Usage: doy=doy(idldt)',/cont
    print,'  idldt must be structure of type IDLDT (see Date/Time Routines in Online help)'
    return,0
  ENDIF 

  IF tag_Names(idldt,/structure_name) NE 'IDLDT' THEN BEGIN 
    Message,'Usage: doy=doy(idldt)',/cont
    print,'  idldt must be structure of type IDLDT (see Date/Time Routines in Online help)'
    return,0
  ENDIF 
    

  dt_to_var, idldt, year=year, month=month, day=day
  month = fix(month)
  day = fix(day)
  doy = date2doy( year, month, day )
  
  return, doy
END


    
