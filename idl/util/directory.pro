;+
; NAME:  directory
; $Id$
; PURPOSE:  Parse the directory off a filename
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: dir=directory(filename) 
; 
; INPUTS:  filename: A list of files
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  A vector of the same size as the input vector, but
;          containing only the directories.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  The assumption is made that the last elements in the
;               '/' separated list, i.e. the one which has no  '/'
;               following it, is the filename and not a
;               directory. That element is parsed off and the
;               remainder is returned
;
; PROCEDURE:  Find the last '/' in each entry. Return everything to
;            the left of it, including the terminating '/'.
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1999, William Daffer
; No Warranties!
;-
;
FUNCTION directory, filename
  IF n_params() LT 1 THEN BEGIN 
    Usage,"dir=directory(filename)"
    return,''
  ENDIF
  IF NOT isa(filename,/string,/nonempty) THEN BEGIN 
    Message,'Filename must be non-empty string!",/cont
    return,''
  ENDIF 

  nf = n_elements(filename)
  dir =  strarr(nf)
  FOR f=0,nf-1 DO BEGIN 
    s = rstrpos(filename[f],'/')
    CASE s OF 
      -1: dir[f]=filename[f] + '/'
      strlen(filename[f])-1: dir[f] = filename[f]
      ELSE : dir[f] = strmid(filename[f],0,s+1)
    ENDCASE 
  ENDFOR 
  IF nf EQ 1 THEN dir = dir[0]
  return,dir
END
