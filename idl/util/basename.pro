;+
; NAME:  Basename
; $Id$
; PURPOSE:  Return the basename of a file (i.e. remove the path)
;
; AUTHOR:  William Daffer
;
; CATEGORY:  File I/O
;
; CALLING SEQUENCE:  basename=basename(files)
; 
; INPUTS:  
;
;   Files: Array of filenames
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  
;   Failure: -1
;   Success: The basenames of the input files
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  Input must be string array.
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1998/11/23 23:35:43  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION basename, filenames
   
   IF n_params() LT 1 THEN BEGIN 
     Message,'Usage: basenames = basename(filenames)',/cont
     return,-1
   ENDIF 
   IF VarType(filenames[0]) NE 'STRING' THEN BEGIN 
     Message,'Input array must be STRING',/cont
     return,-1
   ENDIF 
   nf = n_elements(filenames)
   basenames = strarr(nf)
   FOR f=0l,nf-1 DO BEGIN 
     s = rstrpos(filenames[f],'/')+1
     basenames[f] = strmid(filenames[f],s[0],strlen(filenames[f])-s)
   ENDFOR
   IF nf EQ 1 THEN basenames = basenames[0]
   return,basenames
END
