;+
; NAME:  Basename
; $Id$
; PURPOSE:  Return the basename of a file (i.e. remove the path)
;
; AUTHOR:  William Daffer
;
; CATEGORY:  File I/O
;
; CALLING SEQUENCE:  basename=basename(files [,extsep = extsep])
; 
; INPUTS:  
;
;   Files: Array of filenames
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  extsep : single character. This is the
;                     character that separates the extensions. If this
;                     keyword is set, the routine returns the filename
;                     with the last extension (if there are any)
;                     stripped off. If it isn't present, this routine
;                     returns everything except the path.
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
; Revision 1.3  1999/06/01 18:47:39  vapuser
; Added 'extsep' keyword to separate off the last extension from the filename.
;
; Revision 1.2  1999/04/07 19:16:10  vapuser
; Return scalar output for scalar input
;
; Revision 1.1  1998/11/23 23:35:43  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION basename, filenames, extsep=extsep
   
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
     s = strpos(filenames[f],'/',/reverse_search)+1
     IF n_elements(extsep) EQ 0 THEN BEGIN 
       basenames[f] = strmid(filenames[f],s[0],strlen(filenames[f])-s)
     ENDIF ELSE BEGIN 
       tmp = strmid(filenames[f],s[0],strlen(filenames[f])-s)
       t = strpos( tmp, extsep,/reverse_search )
       basenames[f] =  strmid(tmp,0,t)
     ENDELSE 
   ENDFOR
   IF nf EQ 1 THEN basenames = basenames[0]
   return,basenames
END
