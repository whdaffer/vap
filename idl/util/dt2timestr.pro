;+
; NAME:  dt2timestr
; $Id$
; PURPOSE:  Converts IDL DT to Time string
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:   IDLDT Time manipulation
;
;
;
; CALLING SEQUENCE:  strings = dt2timestr( idldts
;                                           [,separator=separator] )
;
;
; 
; INPUTS:  
;
;   Array of IDLDTs
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;   separator : The character that separates each of the fields in the
;               time string. (default='/', i.e. the routine outputs vaptime)
;
;
; OUTPUTS:   a String where the fields are separated by the separator
;          keyword (default='/')
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
; Revision 1.1  1998/10/15 22:17:58  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION Dt2TimeStr, DT, separator=separator


   catch, error
   IF error NE 0 THEN BEGIN 
     Message,!error_state.msg,/cont
     return,''
   ENDIF 
  
  IF n_Params() NE 1 THEN BEGIN 
    Message,'Usage: RetString=DT2TimeStr(IDLDT)',/cont
    return,''
  ENDIF 

   IF VarType( DT ) NE 'STRUCTURE' THEN BEGIN 
     Message,'DT must be Structure of type IDLDT',/cont
     return,''
   ENDIF 

   IF Tag_Names(DT,/structure_name) NE 'IDLDT' THEN BEGIN 
     Message,'DT must be Structure of type IDLDT',/cont
     return,''
   ENDIF 
  
   IF n_elements(separator) EQ 0 THEN separator = '/'
   nn = n_elements(dt)

   IF nn GT 1 THEN retarray = strarr(nn) ELSE retarray = ''

   FOR i=0,nn-1 DO BEGIN 
     year   = strtrim( dt[i].year ,2 )
     month  = fix( dt[i].month  )
     day    = fix( dt[i].day    )
     hour   = fix( dt[i].hour   )
     minute = fix( dt[i].minute )
     
     IF month LT 10 THEN $
       month = '0'+strtrim(month,2) $
     ELSE $
       month=strtrim(month,2)
     
     IF day LT 10 THEN $
       day = '0'+strtrim(day,2) $
     ELSE $
       day=strtrim(day,2)
        
     IF hour LT 10 THEN $
       hour = '0'+strtrim(hour,2) $
     ELSE $
       hour=strtrim(hour,2)

     IF minute LT 10 THEN $
       minute = '0'+strtrim(minute,2) $
     ELSE $
       minute=strtrim(minute,2)

     retarray[i] =  year + separator + $
                     month + separator + $
                      day + separator + $
                       hour + separator + $
                        minute

   ENDFOR 
  return, retarray
END

    
