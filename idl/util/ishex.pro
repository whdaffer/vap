;+
; NAME:  IsHex.pro
; $Id$
; PURPOSE:  Returns 1 if the string is only digits and letters a-f
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: test=IsHex(numstring) 
; 
; INPUTS:  numstring: A string to test of hexiness
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS: 
;
;  Success: An array of the same number of elements as numstring, with
;           a 1 where numstring has only digits or the letters a-f, and
;           a 0 where otherwise.
;
;  Error: a scalar -1
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  None
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; PROCEDURE:  none
; EXAMPLE:  none
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/04/07 23:15:56  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION isHex, numstring


    ; returns 1 if the 'numstring' contains only digits and letters
    ; a-f

  IF n_params() LT 1 THEN BEGIN 
    Usage,'1|0 =  isHex(numstring)'
    return,-1
  ENDIF 

  IF NOT isa( numstring,/string) THEN BEGIN 
    Message,"NumString must be STRING",/cont
    return,-1
  ENDIF 

  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_State.msg,/cont
    return,-1
  ENDIF 

  tnumstring =  strcompress(numstring,/remove_all)
  IF strmid(tnumstring,0,1) EQ "'" THEN BEGIN 
      ; String is of form 'yyyyy'x. Remove leading and trailing stuff.
    tnumstring =  strmid(tnumstring,1,strlen(tnumstring)-1)
    tnumstring =  strmid(tnumstring,0,strlen(tnumstring)-2)
  ENDIF 

  nn = n_elements(numstring)
  num = intarr(nn)

  teststrings =  ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']
  nteststrings =  n_elements(teststrings)

  copy = strupcase(tnumstring)

  FOR i=0,nn-1 DO BEGIN 

    chars =  byte(copy[i])
    num[i] =  1 ; test for nonhex

    j = 0
    len = n_elements(chars)
    nonhex = 0
    REPEAT BEGIN 
      test = string(chars[j])
      k = 0
      found = 0
      REPEAT BEGIN 
        found =  strpos( test, teststrings[k] ) ne -1
        IF NOT found THEN k =  k+1
      ENDREP UNTIL found OR k EQ nteststrings
      nonhex =  found EQ 0
      IF NOT nonhex THEN j = j+1
    ENDREP UNTIL nonhex OR j EQ len
    num[i] = nonhex NE 1
  ENDFOR 
  IF nn EQ 1 THEN num = num[0]
  return,num
END

    
