;+
; NAME:  ReadColorTable
; $Id$
; PURPOSE:  Reads a file containing a 3 by n array of byte values,
;          i.e. a color table
;
; AUTHOR: William Daffer
; CATEGORY:  
;
; CALLING SEQUENCE:  PtrToColorTable=ReadColorTable(filename)
; 
; INPUTS:  
;
;   Filename: Fully qualified color table file.
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  A pointer to the byte array, a null pointer if failure.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
;  The file this routine will read must conform to the following
;  format.
;
;  Everything after a ';' is treated as a comment. This allows the
;  user to add a header to the file by making each line begin with
;  (possibly some white space) followed by a ';' and then comments.
;  
;  Each line that has a non-white space, non comment form must
;  consist of only 3 columns, of which the first is taken as the red,
;  the second as the green, and the third as the blue. The location of
;  this triplet in the color table is determined by the calling
;  program.
;
;
; PROCEDURE:  Read the file line by line ignoring comments, filling the
;            color table array (which is predefined to store the whole
;            file, if need be). Shorten the array at the end.
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.5  2000/12/14 23:12:40  vapuser
; check for `arg_present' FILENAME before trying to return it.
;
; Revision 1.4  1999/10/05 16:35:59  vapuser
; Allow for null filename on input, call mpickfile instead of returning
; NULL
;
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
    path = '/usr/people/vapuser/Qscat/Resources/Color_Tables'
    filename = dialog_pickfile(path=path, /read,/must_exist)
    IF filename EQ  '' THEN return,ptr_new()
  ENDIF 

  IF n_elements(filename) EQ 0 THEN BEGIN 
    IF arg_present(filename) THEN BEGIN 
      path = '/usr/people/vapuser/Qscat/Resources/Color_Tables'
      filename = dialog_pickfile(path=path, /read,/must_exist)
      IF filename EQ  '' THEN return,ptr_new()
    ENDIF
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
  nlines = nlines(filename)
  ct = bytarr(3,nlines)
  ii = 0
  REPEAT BEGIN 
    readf, lun, rec
    rec = strtrim(strcompress( rec ),2)
    IF strlen(rec) NE 0 AND strmid(rec,0,1) NE ';' THEN BEGIN 
      x = strpos(rec,';')
      IF x GT  0 THEN rec = strmid(rec,0,x[0])
      tmp = strsplit(rec,' ',/extract)
      IF n_elements(tmp) NE 3 THEN BEGIN 
        Message,'All Non-comment Records in color table file MUST have 3 columns!',/cont
        return, ptr_New()
      ENDIF
      ct[*,ii] =  byte(fix(tmp))
      ii = ii+1
    ENDIF 
  ENDREP UNTIL eof(lun)

  ct = ct[*,0:ii-1]
  free_lun, lun

  
  Return, Ptr_new(ct,/no_copy)
END

