;+
; NAME:  rootname.pro
; $Id$
; PURPOSE:  Remove the leading path and the trailing suffix
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utitlity
;
; CALLING SEQUENCE:  rootnames = rootname( files )
; 
; INPUTS:  files: an array of files
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS: 
;
;  Success: A similarly dimensioned string array with only
;           the rootnames, i.e. with the suffixes and the paths
;           removed.
;  Error: A scalar null string
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS: only removes the first suffix. E.g. the file 'aa.b.c'
;               will be returned as 'aa.b'
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/04/21 17:46:59  vapuser
; Initial revision
;
;
; William Daffer
;Copyright (c) 1999
;-

FUNCTION rootname, files

  IF n_params() LT 1 THEN BEGIN 
    Usage,"rootnames = rootname(files)"
    return,''
  ENDIF 

  IF NOT isa( files, /string, /nonempty ) THEN BEGIN 
    Message,"Input Parameter 'FILES' must be array of non-empty strings",/cont
    return,''
  ENDIF 

  basenames =  basename(files)
  nn = n_elements(basenames)

  FOR i=0,nn-1 DO BEGIN 
    s =  strpos( basenames[i], '.' ,/reverse_search)    
    basenames[i] =  (b = strmid(basenames[i],0,s) )
  ENDFOR 
  return, basenames
END
