;+
; NAME:  Date2jd
; $Id$
; PURPOSE:  Convert year/month/day to julian day
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Date manipulation
;
; CALLING SEQUENCE:  jd=date2jd(y,m,d)
; 
; INPUTS:  
;
;  y: the year
;  m: the month
;  d: the day of month
;
;  They may be arrays.
;
; OPTIONAL INPUTS:  none
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  An array of julian days
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  none
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; 
; PROCEDURE:  The procedure is from numerical recipes
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/04/07 16:20:21  vapuser
; Initial revision
;
;
;
;Copyright (c) 1994, William Daffer
;
;-
; No Warranty. You're on your own!
;
FUNCTION Date2jd, y, m, d
gregorian = 15+31l*(10+12l*1582)
iyyy = (jy= long(y))
mm = long(m)
id = long(m)
IF (mm gt 2 ) THEN BEGIN
  jm = mm +1 ;
ENDIF ELSE BEGIN
  jm = mm + 13;
  jy = jy - 1;
ENDELSE

;c = y/100;
;ya = y - 100*c;
;(146097L*C)/4 + (1461*YA)/4 + (153*M+2)/5 + D + 1721119;

jd =  long (floor(365.25*jy)+floor(30.6001*jm)+id+1720995)

IF (id+31l*(mm+12l*iyyy) GE gregorian) THEN BEGIN 
  ja = long(0.01*jy)
  jd = jd + 2 - ja+long(0.25*ja)
ENDIF 

RETURN, jd
END
