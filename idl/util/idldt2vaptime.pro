;+
; NAME:  idlDt2VapTime
; $Id$
; PURPOSE:  Convert Idl DT time format to Vap Time format, strings
;          having the form 'yyyy/mm/dd/hh/mm'.
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  Vap
;
;
;
; CALLING SEQUENCE:  vaptime=idldt2vaptime( array_of_idl_dt_structures )
;
;
; 
; INPUTS:  IDLDT - and array of IDL dt structures.
;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  None
;
;
;
; OUTPUTS:  An array of Vaptime Strings.
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
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION idldt2vaptime, idldt
  rcsid = "$Id:"
  ; vaptime has form yyyy/mm/dd/hh/mm
  ;
  retarray = ''
  nn = n_elements(idldt)
  IF nn EQ 0 THEN BEGIN 
    idldt =  today()            ;
    nn = 1
  ENDIF 

  IF VarType( idldt ) eq 'STRUCTURE' THEN BEGIN 

    IF Tag_Names( idldt, /STRUCTURE_NAME ) EQ 'IDLDT') THEN BEGIN 

      retarray = strarr(nn)
      FOR i=0,nn-1 DO BEGIN 
        year   = strtrim(      idldt[i].year    ,2 )
        month  = fix( idldt[i].month  )
        day    = fix( idldt[i].day    )
        hour   = fix( idldt[i].hour   )
        minute = fix( idldt[i].minute )

        IF month LT 10 THEN $
          month = "0"+strtrim(month,2) $
        ELSE $
          month=strtrim(month,2)

        IF day LT 10 THEN $
          day = "0"+strtrim(day,2) $
        ELSE $
          day=strtrim(day,2)

        IF hour LT 10 THEN $
          hour = "0"+strtrim(hour,2) $
        ELSE $
          hour=strtrim(hour,2)

        IF minute LT 10 THEN $
          minute = "0"+strtrim(minute,2) $
        ELSE $
          minute=strtrim(minute,2)

        retarray[i] =  year + "/" + $
                        month + "/" + $
                         day + "/" + $
                          hour + "/" + $
                           minute

      ENDFOR 
    ENDIF ELSE Message,'Array must be structures of type IDLDT',/cont
  ENDIF ELSE Message,'Input must be an array of STRUCTURES of type IDLDT',/cont
  
  return,retarray
END

  
