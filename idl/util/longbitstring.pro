;+
; NAME:  LongBitString
; $Id$
; PURPOSE: Make a string with a hex representation of a 64 bit integer
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Util
;
; CALLING SEQUENCE:  longstring=longBitString(longwords)
; 
; INPUTS:  longwords: a 2 by n array, the first element is combined to
;         make 1 longword. A 2 vector will produce scalare output. 
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  A n element vector of strings, each string has the 2
;          longwords in the first dimension of the input array
;          combined into a long hexidecimal representation of those
;          longwords. The LSB of the representation is longwords[0,*]
;          and the MSB is longwords[1,*]
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
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION longBitString, longwords

  IF n_params() LT 1 THEN return,''
  dims = size(longwords,/dim)
  nl = 1
  IF n_elements(dims) EQ 2 THEN nl = dims[1]
  IF nl EQ 1 THEN BEGIN 
    bitstrings = $
     PadAndJustify(longwords[0],8,pad='0',/right,form='(z)') + $
     PadAndJustify(longwords[1],8,pad='0',/right,form='(z)') 
  ENDIF ELSE BEGIN 
    bitstrings = strarr(nl)
    FOR l=0,nl-1 DO  $
      bitstrings[l] = $
        PadAndJustify(longwords[0,l],8,pad='0',/right,form='(z)') + $
        PadAndJustify(longwords[1,l],8,pad='0',/right,form='(z)') 
  ENDELSE 
  return, bitstrings
END
