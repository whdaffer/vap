;+
; NAME:   LeapYear
; $Id$
; PURPOSE:  Returns 1 if this is a leap year, 0 otherwise
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE:  1|0 = leapyear(year)
; 
; INPUTS:  year: the year. 
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  1 if input year is a leap year, 0 otherwise
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  1 = leapyear(1994)
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1999, William Daffer
;No Warranties!
;-

FUNCTION leapyear, year
  return, ( (year mod 100 ne 0) and (year mod 4 eq 0) ) xor ( year mod 400 eq 0 )
end
