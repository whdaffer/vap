;+
; NAME:  Date2Doy
; $Id$
; PURPOSE:  Given year/month/day, returns the DOY
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY: Time
;
;
;
; CALLING SEQUENCE:  doy=date2doy(year,month,day)
;
;
; 
; INPUTS:  
;   Year: Array of years
;   Month: Array of months
;   Day: Day of Month
;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  None
;
;
;
; OUTPUTS:  Doy: array of Days-of-year.
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
; Revision 1.1  1998/10/20 20:45:57  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION date2doy, year,month,day

leap = 0
tyear = long(year)
tmonth = long(month)
tday = long(day)

DaysInMonth = [ 31, 28, 31,30,31,30,31,31,30,31,30,31]
leap = ( (year mod 100 ne 0) and (year mod 4 eq 0) ) xor ( year mod 400 eq 0 )

tmp = tmonth-1
x = where(tmp,nx)
doy = tday
IF nx NE 0 THEN BEGIN 
  FOR i=0,nx-1 DO BEGIN 
    doy[x[i]] =  doy[x[i]] + total( DaysInMonth[ 0:tmp[x[i]]-1 ] )
  ENDFOR 
ENDIF 
x = where( leap AND tmonth GT 2,nx )
IF nx NE 0 THEN doy[x] = doy[x] + 1

RETURN, doy
END
