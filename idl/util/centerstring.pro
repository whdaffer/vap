;+
; NAME:   CenterString
; $Id$
; PURPOSE:   Center a string in an output string
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Util
;
; CALLING SEQUENCE:  centered_string=centerstring( string, $
;                                                 borderchar=borderchar,$
;                                                 maxlen=maxlen,$
;                                                  back=back)
; 
; INPUTS:  
;
;   string: The string to center. 
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
;   maxlen    : The maximum length of each line. Default=80
;               characters.
;   borderchar: This character will begin and end each line,
;               default=''
;   back:  Put a border character on the back of the string
;         too. Otherwise, it only puts a border character at the
;         beginning of the output string.
;
; OUTPUTS:  
;
;  the string 'centered' with the border character at the beginning
;  (and end, if /back is set) of the string. Return null string if no
;  input.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) William Daffer, 1999
;-

FUNCTION centerstring, string, borderchar=borderchar, maxlen=maxlen, back=back
  lf = string(10b)
  back = keyword_set(back)
  IF n_elements(borderchar) EQ 0 THEN borderchar = ''
  IF n_elements(maxlen) EQ 0 THEN maxlen = 80
  IF n_params() LT 1 THEN $
    return,borderchar + $
     string(replicate(byte(' '),maxlen-2*strlen(borderchar))) + $
        borderchar
  IF NOT isa(string,/string) THEN return,''

  maxstrlen =  maxlen - 2*strlen(borderchar) 
  IF strlen(string) LT maxstrlen THEN BEGIN 
    npad = (maxstrlen - strlen(string))/2
    IF npad NE 0 THEN $
      padstring =  string(replicate(byte(' '),npad ) ) ELSE $
      padstring = ''
    newstring =  borderchar + padstring + string + padstring 
    IF strlen(newstring) LE maxlen-1 THEN $
      REPEAT newstring= newstring + ' ' UNTIL strlen( newstring ) GE  maxlen-1
    IF back THEN newstring =  newstring + borderchar 
  ENDIF ELSE BEGIN
    t = strpos(string,' ')
    IF t LT maxstrlen THEN BEGIN 
      tmpstring = string
      t = strlen(string)
      REPEAT t = rstrpos( tmpstring, ' ', t) UNTIL t LT maxstrlen
      this_string =  strmid(string,0,t )
      next_string =  strmid( string, t, strlen(string)-t)
      npad = (maxstrlen - strlen(this_string))/2
      IF npad NE 0 THEN $
        padstring =  string(replicate(byte(' '),npad ))  ELSE padstring = ''
      newstring =  borderchar + padstring + $
        this_string 
      IF strlen(newstring) LT maxstrlen-1 THEN $
        REPEAT newstring= newstring + ' ' UNTIL strlen( newstring ) GE maxlen-1
      IF back THEN newstring =  newstring + borderchar 
      newstring =  newstring + lf
      newstring =  newstring + centerstring( next_string,maxlen=maxlen, border=borderchar, back=back)
    ENDIF ELSE BEGIN 
      Message,'1st space is too far. Arbitrary cut!',/info
      this_str =  strmid(string, 0, maxstrlen )
      next_str =  strmid(string, strlen(string)-maxstrlen)
      npad = (maxstrlen - strlen(this_string))/2
      IF npad NE 0 THEN $
        padstring = string(replicate(byte(' '),npad) )  ELSE padstring =  ''
      newstring =  borderchar + padstring + $
        this_string 
      IF strlen(newstring) LT maxstrlen-1 THEN $
        REPEAT newstring= newstring + ' ' UNTIL strlen( newstring ) GE  maxlen-1
      IF back THEN newstring =  newstring + borderchar 
      newstring =  newstring + lf
      newstring =  newstring + centerstring( next_string,maxlen=maxlen,border=borderchar,back=back)
    ENDELSE 
  ENDELSE 
  return, newstring
END

