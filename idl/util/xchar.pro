;+
; NAME:  Xchar.pro
; $Id$
; PURPOSE:  Test a string for any character in a scan set, return 1 if
;           none are, else return 0.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: test=xchar(string,scanset) 
; 
; INPUTS:  
;
;  string: String(s) to be tested
;  scanset: (scalar) a set of characters to be used in testing.
;
; OPTIONAL INPUTS:  none
;       
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
; Success: An array of the same dimensionality as 'string' with a 1 where the
;          string has none of the characters contained in 'scanset'
;          and a 0 where there are 1 or more.
;
; Error: a scalar -1
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
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
;Copyright (c) 1998, William Daffer
;-
; No Warranty! You're on your own.
;
; $Id$
;
FUNCTION xchar, test_string, scanset
  ; Returns true if test_string has NONE of the characters in scanset
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,-1
  ENDIF 

  IF NOT isa(test_string, /string) THEN BEGIN 
    Message,'Input Paramter TEST_STRING must be of type STRING',/cont
    return,-1
  ENDIF 

  ns = n_elements(test_string)
  scan = byte(scanset)
  nn   = strlen(scanset)
  test = intarr(ns)
  FOR i=0,nn-1 DO BEGIN 
    test = test OR (strpos( test_string, string(scan[i]) ) NE -1 )
  ENDFOR 
  IF ns EQ 1 THEN test = test[0]
RETURN,test EQ 0
END
