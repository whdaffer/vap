;+
; NAME:  ppdt.pro (Pretty Print Idl DT variable)
; $Id$
; PURPOSE:  creates a formated string of the infromation in an IDL DT variable
;
; AUTHOR:  William Daffer
;
; CATEGORY:  
;
; CALLING SEQUENCE:  formated_string = ppdt(idldt [, yeardate=yeardate, hhmmss=hhmmss])
; 
; INPUTS:  idldt - and IDL dt variable
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
;   yeardate: (O) the year/date parts of the string (yyyymmdd)
;   hhmmss:   (O) the hour:min:sec.ccc part. (hh:mm:ss.ccc)
;
; OUTPUTS:  
; 
;    formatted_string: with format yyyymmddThh:mm:ss.ccc
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
; 
;Copyright (c) 1999, William Daffer
;-

FUNCTION ppdt, dt, yeardate=yeardate, hhmmss=hhmms
IF n_params() LT 1 THEN BEGIN 
  Usage, ' string=ppdt(dt) '
  return,0
END

IF vartype( dt ) NE 'STRUCTURE' THEN BEGIN 
  Message,"input DT must be a structure of type 'IDLDT'",/cont
  return,0
ENDIF ELSE IF tag_names(dt,/structure_name) NE 'IDLDT' THEN BEGIN 
  Message,"input DT must be a structure of type 'IDLDT'",/cont
  return,0
ENDIF 

yeardate =  strtrim(dt.year,2) + '-' + $
           PadAndJustify(dt.month,2,/right) + '-' + $
           PadAndJustify(dt.day,2,/right) 

hhmmss =   PadAndJustify(dt.hour,2,/right) + ':' + $
           PadAndJustify(dt.minute,2,/right) + ':' + $
           PadAndJustify(dt.second,2,/right) + '.000' 
string yeardate + 'T' + hhmmss
return,string
END
