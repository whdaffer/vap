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
;
;
;Copyright (c) 1994, William Daffer
;
;-
; No Warranty. You're on your own!
;
FUNCTION Date2jd, y, m, d

y = long(y)
m = long(m)
d = long(m)
IF (m gt 2 ) THEN BEGIN
  m = m - 3;
ENDIF ELSE BEGIN
  m = m + 9;
  y = y - 1;
ENDELSE

c = y/100;
ya = y - 100*c;
RETURN,(146097L*C)/4 + (1461*YA)/4 + (153*M+2)/5 + D + 1721119;
END
