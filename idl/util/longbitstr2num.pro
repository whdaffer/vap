;+
; NAME:  LongBitStr2num.pro
; $Id$
; PURPOSE:  Convert a string containing a 64 bit hex represention of a
;          number to the two numbers they represent.
;
; AUTHOR:  William Daffer
;
; CATEGORY:   EA Instrument state utility
;
; CALLING SEQUENCE:  longwords = longBitStr2Num(string)
; 
; INPUTS:  
;
;  String: A string array containing the hex representations of a double long
;          integers, i.e. 64 bit numbers.
;
;          The string must have only digits and the characters a-f or
;          A-F or combinations thereof. The far right hand of the
;          string is taken to be the LSB of the resulting number, so
;          that the first eight characters will go into longwords[0]
;          and the second into longwords[1]. Each longword defaults to
;          0 and the empty string will return [0l,0l] 
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  
;   Success: a n-vector, where n=n_elements(string)
;   Failure:  the null string
;
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

FUNCTION longBitStr2Num, bitstrings
   IF n_params() LT 1 THEN return,''
   IF NOT isa(bitstrings,/string,/nonempty) THEN return,''
   
   nl = n_elements(bitstrings)
   longwords = lonarr(2,nl)
   FOR i=0l,nl-1 DO BEGIN 
     t = strlen(bitstrings[i])
     IF t GT 0 THEN BEGIN 
       tmp = string(reverse(byte(bitstrings[i])))
       l0 = string(reverse(byte( strmid( tmp, 0, 8 < t))))
       reads,l0,l0,format='(z)'
       longwords[0,i] =  l0
       IF t GT 8 THEN  BEGIN 
         l0 = string(reverse(byte(strmid(tmp,8,0> t-8))))
         reads,l0,l0,format='(z)'
         longwords[1,i] =  l0
       ENDIF 
     ENDIF 
   ENDFOR 
   IF nl EQ 1 THEN longwords =  reform(longwords)
   return,longwords
END
