;+
; NAME:  PadAndJustify
; $Id$
; PURPOSE:  Pads a string using either right or left justification
;          and the indicated pad character (default= 0)
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  Text Manipulation
;
;
;
; CALLING SEQUENCE:  padded_string = PadAndJustify( array,
;                   number_of_digits[, pad=pad , format=format, /right|/left])
;
;
; 
; INPUTS:  
;
;   array : array of numbers or strings to be padded
;   ndigits: length of returned string(s)
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;   Pad    : Pad character (must have length 1)
;   right  : flag, if set, right justify
;   left   : flag, if set, left justify.
;   format : format appropriate for string or print function
;
;
;
; OUTPUTS:  
;
;   A string array with same dimensionality as input array, each entry
; being 'ndigits' long padded with the 'pad' character and justified
; according to the 'right' or 'left' flags.
;
;
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  None

;
; SIDE EFFECTS:  A slight flush.
;
;
;
; RESTRICTIONS:  None
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.5  1999/01/28 21:05:40  vapuser
; Added 'format' keyword.
;
; Revision 1.4  1998/10/22 21:24:28  vapuser
; Handle strarr(1) case
;
; Revision 1.3  1998/10/21 23:10:04  vapuser
; Convert numbers to long
;
; Revision 1.2  1998/10/21 23:06:37  vapuser
; took out 'selecting right...' message
;
; Revision 1.1  1998/10/21 21:02:28  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION PadAndJustify, number, ndigits, pad=pad, $
                        right=right, left=left, format=format

 right = keyword_set(right)
 left = keyword_set(left)
 IF n_elements(pad) EQ 0 THEN pad = '0'
 IF VarType(pad) NE 'STRING' THEN pad = String(pad)
 IF strlen(pad) NE 1 THEN BEGIN 
   Message,'Pad must be a SINGLE character',/cont
   return,0
 ENDIF 

 IF right AND left THEN BEGIN 
   Message,'Only select 1 of /right or /left',/cont
   return,0
 ENDIF ELSE IF NOT (right OR left ) THEN BEGIN 
   ; Message,'Selecting right justification',/info
   right = 1
 ENDIF 

 IF n_params() EQ 2 THEN BEGIN 
   nnum = n_elements(number)
   IF nnum EQ 1 THEN retarr = '' ELSE $
     retarr = strarr(nnum)
   IF isa(number,/number) THEN $
     tnum = long(number) ELSE $
     tnum=number
   FOR i=0,nnum-1 DO BEGIN 
     IF n_elements(format) NE 0 THEN $
       str = strtrim(string(tnum[i],format=format),2) ELSE $
       str = strtrim( tnum[i], 2 ) 

     len = strlen(str)
     IF len LT ndigits THEN BEGIN 
       padding = string(bytarr(ndigits) + replicate( byte(pad), ndigits ))
       IF right THEN  $
         strput, padding, str, ndigits-len $
       ELSE $
         strput, padding, str, 0
     ENDIF ELSE padding= strmid(str, 0, ndigits )
     retarr[i] = padding
   ENDFOR 
 ENDIF ELSE BEGIN 
   Message,'Usage: padded_string = PadAndJustify( number, ndigits [,pad=pad, format=format, /right|/left])',/cont
   return,0
 ENDELSE 
 return,retarr

END

 
 
