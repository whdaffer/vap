;+
; NAME:  DateStr2DoyStr
; $Id$
; PURPOSE:  Converts a string of the form dd-mmm-yy, or possibly
;           d-mm-yy or dd-mm[m]-yyyy to one of the form  yyyydoy
;           taking into account leap years

;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  Date/Time conversion
;
;
;
; CALLING SEQUENCE:  doy_string = DateSTr2DoyStr( date_str )
;
;
; 
; INPUTS:  
;
;  Date_str : String of form dd-mm-yy or dd-mmm-yyyy 
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
; OUTPUTS:  Doy_str - string of form yyyydoy
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
; RESTRICTIONS:  
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
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION datestr2doystr, date_str

;

is_a_number = 0

names = ['jan','feb','mar','apr','may','jun','jul',$
	  'aug','sep','oct','nov','dec', $
         'JAN','FEB','MAR','APR','MAY','JUN','JUL',$
	  'AUG','SEP','OCT','NOV','DEC'                ]
numdays=[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
numstr = '0123456789'

leap = 0

db = (byte( '-'))(0)
bstr = byte( date_str )
d = where( bstr eq db, ndb )
len = strlen( date_str )

base_century = ( (len - d(ndb-1)) eq 3 )*1900 

dayomon = string( bstr( 0        : d(0) - 1               ) )
month   = string( bstr( d(0) + 1 : d(1) - 1               ) )
year    = string( bstr( d(1) + 1 : n_elements( bstr ) - 1 ) )
year    = string( fix( year ) + base_century, format = '(i4)' )
is_a_number = strpos( numstr, strmid( month, 0, 1 ) )  ne -1 

IF is_a_number THEN BEGIN
  i =  month-1
ENDIF ELSE BEGIN
  month =  strupcase( month )
  i = where( names eq month, ni )
  IF ni eq 0 THEN BEGIN
    message,' Can''t find month name - returning ', /cont
    return, ''
  ENDIF
  i = i mod 12
ENDELSE

leap = ( (year mod 100 ne 0) and (year mod 4 eq 0) ) xor ( year mod 400 eq 0 )

i =  i(0)
IF i EQ 0 THEN tmp =  dayomon ELSE $
 tmp =  strtrim( string( fix( total( numdays(0:i-1 )) + $
                              fix( dayomon ) + (leap eq 1)*1 )) , 2 )
doy ='000'
strput, doy, tmp, 3-strlen( tmp )

doy_str  = year + doy 
RETURN, doy_str
END
