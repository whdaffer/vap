;+
; NAME:  Read_SatMovie_Defs
; $Id$
; PURPOSE:  Reads the $VAP_LIB/Satmovie_defs.dat file
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Qscat/Seawinds animation
;
; CALLING SEQUENCE:  defs=Read_Satmovie_Defs( roi )
; 
; INPUTS:  roi - Region-Of-Interests.
;    Must be one of:
;
;       NPAC - North Pacific
;       SPAC - South Pacific
;       NATL - North Atlantic
;       SATL - South Atlantic
;       INDIAN - Indian
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
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
; Revision 1.1.1.1  2001/12/04 19:14:14  vapuser
; Imported sources
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION read_satmovie_defs, desig
IF n_elements( desig ) EQ 0 THEN desig =  'npac'
desig =  strupcase( desig )


openr, rlun, '$VAP_LIBRARY/satmovie_defs.dat',/get,error=err
IF err EQ 0 THEN BEGIN
  rec =  ''
  test =  'BEGIN ' + DESIG
  REPEAT BEGIN 
    readf, rlun, rec
    found = (strpos( rec, test ) NE -1)
  ENDREP UNTIL found OR eof( rlun )
  IF eof( rlun ) AND NOT( found ) THEN BEGIN
    ret =  {desig: 'ERRORNOSUCHROI'}
    message,' No such ROI ',/cont
  ENDIF ELSE BEGIN
    ret =  satmovie_defs_str()
    TAGS =  tag_names( ret )
    test =  'END ' + desig 
    readf, rlun, rec
    WHILE (strpos( rec, test ) EQ -1 ) AND NOT( eof( rlun ) ) DO BEGIN
      tmp =  str_sep( rec, ':' )
      field =  strtrim( tmp(0), 2 )
      value =  tmp(1)
      xx =  where( tags EQ field, nxx )
      IF nxx eq 0 THEN BEGIN
        ret = {desig:'ERRORNOSUCHFIELD'}
        message,field + ' No such field ',/cont
        return, ret
      ENDIF 
      str =  'ret.' + field + ' = ' + value
      s =  execute( str )
      IF NOT(s) THEN BEGIN 
        message,!err_string,/cont
        ret =  {desig:"ERRORINEXECUTE"}
        return, ret
      ENDIF 
      readf,rlun,rec
    ENDWHILE 
    free_lun,rlun
  ENDELSE 
ENDIF ELSE BEGIN
  ret =  {desig:'ERROROPENFAILURE'}
  message, !err_string,/cont
ENDELSE 

RETURN, ret
END 
