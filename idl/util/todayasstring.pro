;+
; NAME:  TodayAsString()
; $Id$
; PURPOSE:  Converts current time to a string. One is allowed to
;          specify a separator between the various fields, but the
;          default is to use the null string, so that the output will
;          be  of form yyyymmddhhmi

;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:   Time Manipulation
;
;
;
; CALLING SEQUENCE:  
;
;       todayString=TodayAsString( [separator=some_string])
;
; 
; INPUTS:  
;      None
;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  
;
;   Separator : a string to be used in separating the fields.
;
;
;
; OUTPUTS:  
;
;    A String of form yyyy/mm/dd/hh/mi where '/' is standing in for
;    whatever separator is used, null string by default.
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
;    Call DT_To_Var,Today, year=Year, month=month, day=day, Hour=hour,$
;                  min=min
;
;    Pack the output array.
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

FUNCTION TodayAsString, separator=separator
  rcsid = "$Id:"
  Dt_To_Var,today(),year=year,month=month,day=day, hour=hour, min=min

  IF n_elements(separator) EQ  0 THEN separator = ''


  month = fix(month)
  day = fix(day)
  hour = fix(hour)
  min = fix(min)

  IF month GE 10 THEN BEGIN 
    month = strtrim(month,2)
  ENDIF ELSE BEGIN
    month = '0' + strtrim(month,2)
  ENDELSE 

  IF day GE 10 THEN BEGIN 
    day = strtrim(day,2)
  ENDIF ELSE BEGIN
    day = '0' + strtrim(day,2)
  ENDELSE 

  IF hour GE 10 THEN BEGIN 
    hour = strtrim(hour,2)
  ENDIF ELSE BEGIN
    hour = '0' + strtrim(hour,2)
  ENDELSE 

  IF min GE 10 THEN BEGIN 
    min = strtrim(min,2)
  ENDIF ELSE BEGIN
    min = '0' + strtrim(min,2)
  ENDELSE 
  
  
   year = strtrim(year,2)

   return,year+separator+month+separator+day+separator+hour+separator+min
END
