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

  IF n_params() EQ 0 THEN BEGIN 
    Message,'Usage, PtrToColorTable = readct( filename )',/cont
    PtrToColorTable = Ptr_New()
  ENDIF ELSE BEGIN 
    Openr, lun, filename, /get_lun, error=err
    IF err EQ 0 THEN BEGIN 
      rec = ''
      readf, lun, rec
      rec = strtrim(strcompress( rec ),2)
      tmp = str_sep( rec, ' ')
      IF n_elements(tmp) NE 3 THEN BEGIN 
        Message,'All Records in color table file MUST have 3 columns!',/cont
        PtrToColorTable = Ptr_New()
        
      ENDIF ELSE BEGIN 
        Point_Lun, lun, 0
          ; Count the number of records
        nrecs = 0
        WHILE NOT eof(lun) DO BEGIN 
          readf,lun,rec
          nrecs = nrecs+1
        ENDWHILE
        Point_Lun, lun, 0
        ColorTable = bytarr(3,nrecs)
        Readf, lun, ColorTable
        PtrToColorTable = Ptr_New(ColorTable)
      ENDELSE 
      free_lun, lun
    ENDIF ELSE BEGIN 
      Message,!Error_State.msg,/cont
      PtrToColorTable = Ptr_New()
    ENDELSE 
  ENDELSE 
  
  Return, PtrToColorTable
END

