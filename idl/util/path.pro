;+
; NAME: Path.pro
; $Id$
; PURPOSE: Return the path of the input file, or './' if there is none
;
; CATEGORY: Filename processing utility.
;
; CALLING SEQUENCE: path=path(filename)
;
; INPUTS: filename: a vector of fully qualified filenames.
;
; OPTIONAL INPUTS: none
;
; KEYWORD PARAMETERS: none
;
; OUTPUTS: the Paths 
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
; Copyright (c), 1999, William Daffer
; No Warranties!
;
;-

FUNCTION path, filename
  IF NOT isa(filename,/string,/nonempty) THEN return,''
  nf = n_elements(filename)
  path = strarr(nf)
  FOR f=0,nf-1 DO BEGIN 
    t = strpos(filename[f],'/',/reverse_search)+1
    IF t EQ 0 THEN path[f] = './' ELSE $
      path[f] = strmid( filename[f], 0, t )
  ENDFOR 
  IF nf EQ 1 THEN path = path[0]
  return,path
END



