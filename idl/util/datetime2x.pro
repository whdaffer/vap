;+
; NAME: DateTime2X  
; $Id$
; PURPOSE:   Convert strings of some Date/Time format (currently
;            yyyy/mm/dd/hh/mm/ss or some subset thereof) to some kind of
;            time useful in plotting.
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:   Time Manipulation
;
;
;
; CALLING SEQUENCE:  X = DateTime2X( DateTimeArray,
;                   DateTimeFormat=string, Zero=string )
;
;
; 
; INPUTS:  
;
;   DateTimeArray : Array of time strings whose format is determined
;                   by keyword DateTimeFormat (or defaults to yyyy/mm/dd/hh/mm/ss)
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;         DateTimeFormat: (I) String, format of input strings in
;                         DateTimeArray. Use the following formats.
;
;                         %Y - 4 digit year.
;                         %N - 2 digit month
;                         %D - 2 digit Day of Month
;                         %H - 2 digit hour
;                         %m - 2 digit minute
;                         %s - 2 digit second
;
;                          If this keyword is not present or is
;                          present but the equivalent of a null string
;                          the default format will be %Y/%N/%D/%h
;
;
; OUTPUTS:  
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;
;
; SIDE EFFECTS:  
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
;Copyright (c) 1998, William Daffer
;-
