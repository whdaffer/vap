;+
; NAME:  IsCHex.pro
; $Id$
; PURPOSE:  Determine whether a string is a C Hex number (e.g. 0Xaaff)
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: Test=IsCHex(string) 
; 
; INPUTS:  string: a string, possibly of the form, '0Xxxxx', for
;          example, of the sort one might read from a file written by
;          a C routine. This is a pretty dumb routine. It assumes that
;          if it begins with '0x' or '0X', the answer is 'yes'
;
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  none
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
;
; PROCEDURE:  none
;
; EXAMPLE:  1=ischex('0xaaff')
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1998, William Daffer
;-
; No Warranties!
;
FUNCTION isChex, numstring

  IF n_params() LT 1 THEN BEGIN 
    Usage,'1|0 =  isChex(numstring)'
    return,-1
  ENDIF 

  IF NOT isa(numstring,/string) THEN BEGIN 
    Message,'Numstring must be a string',/cont
    return,-1
  ENDIF 

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,-1
  ENDIF 

  num =  strpos(strcompress(strupcase(numstring),/remove_all),'0X') EQ 0
  return,num
END

    
