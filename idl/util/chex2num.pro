;+
; NAME: chex2num.pro 
; $Id$ 
;
; PURPOSE: Convert a character representation of a hex number (like
;          one read from a file output by a C program e.g. 0xaaff) to
;          a number. 
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Munging of data output be C routines.
;
; CALLING SEQUENCE:  number=chex2num(string_num)
; 
; INPUTS:  string_num
;
; OPTIONAL INPUTS:  none
; KEYWORD PARAMETERS: 
; OUTPUTS:  the string, converted to a number
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  
; SIDE EFFECTS:  
; RESTRICTIONS:  
; PROCEDURE:  
; EXAMPLE:
;
; print,chex2num('0xaaff') 
; prints 43775
; MODIFICATION HISTORY:
;
; $Log$
;
;
;Copyright (c) 1999, William Daffer
;-

FUNCTION CHex2num, cHex ;, signed= signed
  IF NOT exist( cHex ) THEN BEGIN 
    Usage,'num = Chex2num(Chex [,/signed])'
    return,''
  ENDIF 
  IF NOT (isa(chex,/string)) THEN BEGIN 
    Message,'Chex must be a STRING ',/cont
    return,''
  ENDIF 
  IF strlen(chex[0]) EQ 0 THEN BEGIN 
    Message,'Chex must be a NON-ZERO STRING ',/cont
    return,''
  ENDIF 

  x = strpos( cHex, 'x' )
  nn =  n_elements(cHex)
  num = lonarr(nn)
  v = 0L
  FOR i=0,nn-1 DO BEGIN 
    junk = strmid(cHex[i],x[i]+1,strlen(cHex[i])-x[i]-1)
    reads,junk,v,form='(z)'
    num[i] = v
  ENDFOR 
  return, num
END
