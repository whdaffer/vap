;+
; NAME:  bitstr2num.pro
; $Id$
; PURPOSE:  Convert a bitstring back to it's number
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: number=bitstr2num(bitstring)
; 
; INPUTS:  bitstring: Array of (binary) strings (.e.g. '00001111' = '0f'x)
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  The numbers, as longwords
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION bitstr2num, bitstrings

  IF n_params() LT 1 THEN BEGIN 
    Usage,'num=bitstr2num(bitstrings)',/cont
    return,''
  ENDIF 

  nn = n_elements(bitstrings)
  num = lonarr(nn)
  IF nn EQ 0 THEN BEGIN 
    Message,'BitStrings must be defined!',/cont
    return,''
  ENDIF 
  
  IF NOT isa(bitstrings,/string,/nonempty) THEN BEGIN 
    Message,'BitStrings must be NON-NULL STRING',/cont
    return,''
  ENDIF 
  
  scanset = '~!@#$%^&*()_+`23456789-=\|]}[{";:/?.>,<' + $
   'abcdefghijklmnopqrstuvwxyz' + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  test =  xchar( bitstrings, scanset ) 
  x = where(test EQ 0, nx )
  IF nx NE 0 THEN BEGIN 
    Message,"bitstrings has chars != 0|1",/cont
    return,''
  ENDIF 
  
  FOR i=0,nn-1 DO BEGIN 
    t = string(reverse(byte(bitstrings[i])))
    s = strlen(t)
    n = num[i]
    FOR j=0,s-1 DO n = n + long(strmid(t,j,1))*(2l^j)
    num[i] = n
  ENDFOR 
  IF nn EQ 1 THEN num = num[0]

  return,num
END
