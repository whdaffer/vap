;+
; NAME:  doy2date.pro
; $Id$
; PURPOSE:  Return month/day for input Day-of-year.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  General date manipulation
;
; CALLING SEQUENCE:  date=doy2date(doy)
; 
; INPUTS:  doy : a integer between 0 and 366
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;  Success: [month, day-o-month]
;  Failure: [-1,-1]
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  none
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; PROCEDURE:  
; EXAMPLE:  
; MODIFICATION HISTORY:
;
; $Log$
;
; 
;Copyright (c) 1998, William Daffer
;-
; No Warranty, you're on your own!
;

FUNCTION Doy2date, year, doy

; ndaystoeom = number of days in the year until the end of that month,
; i.e. ndaystoeom(4,0) = numbers of days in year until the end of may
; in a non leap-year.


out = ['-1','-1']

rcsid = "$Id$"
ndaystoeom = [ [ 31, 59, 90,120, 151, 181,  212, 243, 273, 304, 334, 365 ],$
               [ 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 ] ]

leap = ( (year mod 100 ne 0) and (year mod 4 eq 0) ) xor ( year mod 400 eq 0 )

x = where( ndaystoeom( *, leap ) Ge doy, nx )
IF nx EQ 0 THEN BEGIN 
  message," Couldn't find the month ",/cont
ENDIF  ELSE BEGIN 
  out[0] = strtrim( x[0]+1, 2 )
  IF fix(out[0]) LT 10 THEN out[0] =  '0' + out[0]
  IF x[0] NE 0 THEN   $
    tmp  =  doy - ndaystoeom( x[0]-1, leap ) $
  ELSE $
     tmp =  doy
  IF tmp LT 10 THEN $
     out[1] =  '0' + strtrim(tmp,2) $
  ELSE $
     out[1] =  strtrim(tmp,2)
ENDELSE 

RETURN, out ; [month, day-o-month]
END

