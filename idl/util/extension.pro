;+
; NAME: Extension
;
; PURPOSE: Find the last extension of the input filename. Like the :e
;          file modifier in the 'c shell'
;
; CATEGORY: Utility
;
; CALLING SEQUENCE: ext=extension( filename, extsep=extsep)
;
; INPUTS:
;
;   Filename: filename (possibly a vector)
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
;   extsep: Scalar string. The character that separates the extentions
;           Default is '.'
;
; OUTPUTS:
;
;   The last extention. Just like the ':e' modifier in the c shell. 
;   The qualifier 'last' is important. If the file is a.b.c and the
;   default extension seperater of '.' is used, this routine returns 'c'.
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
;-
FUNCTION extension, filename, extsep=extsep
  IF n_elements(extsep) EQ 0 THEN extsep = '.'
  IF NOT isa(filename,/string,/nonempty) THEN return,''
  nf = n_elements(filename)
  extension = strarr(nf)
  FOR f=0,nf-1 DO BEGIN 
    t = strpos(filename[f],extsep,/reverse_search)+1
    IF t EQ -1 THEN extension[f] = '' ELSE $
     extension[f] = strmid( filename[f], t, strlen(filename[f])-t)
  ENDFOR 
  IF nf EQ 1 THEN extension = extension[0]
  return,extension
END


