;+
; NAME:  ReadColorTable
; $Id$
; PURPOSE:  Reads a file containing a 3 by n array of byte values,
;          i.e. a color table
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  PtrToColorTable=ReadColorTable(filename)
;
;
; 
; INPUTS:  
;
;   Filename: Fully qualified color table file.
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:  A pointer to the byte array, a null pointer if failure.
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;
;
; SIDE EFFECTS:  
;
;
;
; RESTRICTIONS:  
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
; Revision 1.3  1999/07/14 22:55:26  vapuser
; Added call to mpickfile, simplified code.
;
; Revision 1.2  1998/11/03 22:29:06  vapuser
; Changed some comments
;
; Revision 1.1  1998/11/03 22:16:17  vapuser
; Initial revision
;
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION ReadColorTable, filename

  IF !version.release LT 5.1 THEN BEGIN 
    Message,'IDL 5.1 or greater required.',/cont
    return,Ptr_New()
  ENDIF 
  IF n_params() LT 1 THEN BEGIN 
    filename = mpickfile(path='/usr/people/vapuser/Qscat/Resources/Color_Tables')
    IF filename EQ  '' THEN return,ptr_new()
  ENDIF 

  IF arg_present(filename) THEN BEGIN 
    filename = mpickfile(path='/usr/people/vapuser/Qscat/Resources/Color_Tables')
    IF filename EQ  '' THEN return,ptr_new()
  ENDIF 

  IF NOT isa(filename,/string, /nonempty) THEN BEGIN 
    Message,'Filename must be non-empty string',/cont
    return, ptr_new()
  ENDIF 

  Openr, lun, filename, /get_lun, error=err
  IF err NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return, Ptr_New()
  ENDIF 
  rec = ''
  readf, lun, rec
  rec = strtrim(strcompress( rec ),2)
  tmp = str_sep( rec, ' ')
  IF n_elements(tmp) NE 3 THEN BEGIN 
    Message,'All Records in color table file MUST have 3 columns!',/cont
    return, ptr_New()
  ENDIF
  nlines = nlines(filename)
  Point_Lun, lun, 0
  ColorTable = bytarr(3,nlines)
  Readf, lun, ColorTable
  PtrToColorTable = Ptr_New(ColorTable)
  free_lun, lun

  
  Return, PtrToColorTable
END

