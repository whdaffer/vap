;+
; NAME: Bitstring
; $Id:
; PURPOSE: Convert a number to a bitstring
; CATEGORY: Utitlity
; CALLING SEQUENCE: bitstring=bitstring(number)
;
; INPUTS: number(s): Array of numbers whose binary representations (as
;         strings)you want returned
;
; OPTIONAL INPUTS: None
;
; KEYWORD PARAMETERS:none
;
; OUTPUTS: An array strings, having the same dimensionality as the
;          input argument, with a 1 for each bit that's set and a 0
;          where it ain't.
;
; OPTIONAL OUTPUTS: None
; COMMON BLOCKS: None
; SIDE EFFECTS: None
; RESTRICTIONS: None
; PROCEDURE:
; EXAMPLE: '00001111' = bitstring( '0f'x)
;
;
;
; MODIFICATION HISTORY:
; 
; $Log$
;
;
; Copyright; William Daffer, 1999
;
;-
; No Warranties
FUNCTION bitstring, number

  IF n_params() LT 1 THEN BEGIN 
    Usage,"bitsring=bitstring(number)"
    return,''
  ENDIF 

  IF NOT isa( number, /type_integer) THEN BEGIN 
    Message,'Number must be of BYTE, INTEGER or LONGWORD type',/cont
    RETURN,''
  ENDIF 

  nn = n_elements(number)
  strings = strarr(nn)
  FOR i=0,nn-1 DO BEGIN 
    strings[i] =  strjoin( binary(number[i] ),'')
  ENDFOR 
  IF nn EQ 1 THEN strings = strings[0]
  return, strings
END
