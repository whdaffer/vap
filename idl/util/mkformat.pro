;+
; NAME:  mkformat.pro
; $Id$
; PURPOSE:  Given some data, return a good format for printing it.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  General data munging
;
; CALLING SEQUENCE:  format=mkformat(data [,/int|/float|/double])
; 
; INPUTS:   data: the data you want to get the printing format for
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  
;
;  int - flag, use 'I' format
;  float - flag, use 'F' format
;  double - flag, use 'G' format
;
; OUTPUTS:   a string of the 'format' variety, e.g.  '(f7.2)'
; OPTIONAL OUTPUTS:  None
; COMMON BLOCKS:  None
; SIDE EFFECTS:  None
; RESTRICTIONS:  none
; PROCEDURE:  
; EXAMPLE:  
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/04/06 19:15:43  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION mkformat, x, float=float, $
                   int=int, double=double
  int =  keyword_set(int)
  double = keyword_set(double)
  float =  ( ((int EQ 0) AND $
              (double EQ 0) ) OR $
              keyword_set(float) )
  CASE 1 OF 
  float EQ 1 OR double EQ 1: BEGIN 
    IF x NE 0 THEN BEGIN 
      decade = fix( alog10(abs(x)) )+1
      neg = x lt 0
      width = decade + neg + 3 
      IF float THEN $
       format = "(f" + $
       strtrim( string( width, form='(i2)'),2)+ ".2)" ELSE $
       format = "(g" + strtrim( string( width, form='(i2)'),2)+ ".2)"
    ENDIF ELSE $ 
      IF float THEN format='(f3.1)' ELSE format = '(g3.1)'
  END
  int EQ 1 : BEGIN 
    IF x NE 0 THEN BEGIN 
      width = strlen( strtrim( x,2 ) )
      format = "(i" + strtrim( string( width, form='(i2)'),2)+ ")"
    ENDIF ELSE format='(i2)'
  END
ENDCASE 
RETURN,format
END 
