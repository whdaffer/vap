;+
; NAME:  nlines
; $Id$
; PURPOSE:  return the number of lines in a file (very unix centric!)
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: nn=nlines(filename) 
; 
; INPUTS:  filename: Scalar string. A fully qualified file.
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  nn: the number of lines. -1 if error
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
; Revision 1.1  1999/10/06 21:33:45  vapuser
; Initial revision
;
;
;Copyright (c) 1999, William Daffer
; No Warranties!
;-

FUNCTION nlines, file, noshell=noshell
  IF n_params() LT 1 THEN return,-1
  IF NOT isa(file,/string,/nonempty) THEN return,-1 
  IF keyword_set(noshell) THEN BEGIN 
    args =  ['wc', '-l', file]
    spawn, args, nlines,count=cnt, /noshell
  ENDIF ELSE spawn,'wc -l ' + file, nlines, count=cnt
  return,long(nlines[0])
END
  
