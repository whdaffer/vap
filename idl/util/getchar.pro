;+
; NAME:  GetChar.pro
; $Id$
; PURPOSE:  Retrieve one character from a string
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utitlity
;
; CALLING SEQUENCE:  char=getchar(string [, charnum | [,/first |/last]])
; 
; INPUTS:  string : the string from which the single character will be
;                   extracted
;
; OPTIONAL INPUTS:  charnum : the position of the desired character 
;                             (first character=position 0 ) 
;                   One of (charnum, first or last must be set)
;	
; KEYWORD PARAMETERS:  
;
;   first: flag, take first character
;   last: flag, take last character.
;
;
;
; OUTPUTS:  
;
;  Success: The character. If charnum>strlen(string), returned character=''.
;  Failure: a null string.
;
; OPTIONAL OUTPUTS:  None
; COMMON BLOCKS:  none
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; PROCEDURE:  
; EXAMPLE:  
;
;  't' = getchar( 'lets have a ball',2)
;  'l' = getchar( 'lets have a ball',/first)
;  'l' = getchar( 'lets have a ball',/last)
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION getchar, string, charnum, first=first, last=last
  IF n_params() LT 1 THEN BEGIN 
    message,'Usage: char=getchar(string [,channum [,/first | ,/last] ] )',/cont
    return,''
  ENDIF 
  IF VarType( string ) NE 'STRING' THEN BEGIN 
    Message,' Input string must be of type STRING',/cont
    return,''
  ENDIF 
  len = strlen(string)
  IF keyword_set( first ) THEN return, strmid(string,0,1)
  IF keyword_set( last ) THEN return, strmid(string, len-1,1)
  IF n_elements(charnum) NE 0 THEN BEGIN 
    Message,'Input parameter CHARNUM is REQUIRED if first/last not present',/cont
    return,''
  ENDIF 
  charnum = long(charnum) 
  retchar = ''
  IF charnum LE len-1 THEN retchar = strmid( string, charnum, 1)
  return,retchar
END

  
