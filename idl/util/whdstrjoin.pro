;+
; NAME:  StrJoin
; $Id$
; PURPOSE:  Join strings using input separator
;
; AUTHOR:   William Daffer
;
; CATEGORY:  String processing
;
; CALLING SEQUENCE:  joined_string = strjoin(array[, separator])
; 
; INPUTS:  
;  
;   Array: an array you want joined. A string copy is made of the
;          array, the elements of which are then joined using the
;          separator, which defaults to a space, if absent on input,
;          as the delimiter between fields
;
; OPTIONAL INPUTS:  
;
;   separator: A string. If not present, default is a space. As a
;              precaution, the separator is passed through the
;             'string' function.
;       
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  The joined string
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  None
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION strjoin, array, separator
  IF n_params() LT 1 THEN BEGIN 
    Message,'Usage: joined_string=strjoin(array [,separator])',/cont
    return,-1
  ENDIF 
  IF n_elements(separator) EQ 0 THEN $
    separator = ' ' ELSE $
    separator =  string(separator)
  tarr = strtrim(array,2)
  joined_string = tarr[0]
  FOR i=1,n_elements(tarr)-1 DO $
     joined_string = joined_string + separator + tarr[i] 
  
  return,joined_string
END
