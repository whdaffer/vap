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
;
;Copyright (c) 1999, William Daffer
; No Warranties!
;-

FUNCTION nlines, file
  IF n_params() LT 1 THEN return,-1
  IF NOT isa(file,/string,/nonempty) THEN return,-1 
  spawn, 'wc -l ' + file, nlines,count=cnt
  return,long(nlines[0])
END
  
