;+
; NAME:  vaptime2idldt
; $Id$
; PURPOSE:  Convert time of form yyyy/mm/dd/hh/mm to IDLDT form
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  Time Routine
;
;
;
; CALLING SEQUENCE:  
;
;     idldt  = vaptime2idldt( vaptime )
;
;
; 
; INPUTS:  
;
;    Vaptime: An array of vaptimes 
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
; OUTPUTS:  
;
;    idldt : an array of IDLDT time structures
;
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  None
;
;
;
; RESTRICTIONS:  Input times must be string and formatted like vaptimes
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


FUNCTION vaptime2idldt, vaptimes
   rcsid = "$Id$"
   IF N_Params() GE 1 THEN BEGIN 
     nn = n_Elements(vaptimes)
     idldt = replicate( {IDLDT}, nn )
     FOR i=0,nn-1 DO BEGIN 
       tmp   = Str_Sep( vaptimes(i), '/' )
       year  = fix(tmp[0])
       month = fix(tmp[1])
       day   = fix(tmp[2])
       hour  = fix(tmp[3])
       min = 0
       IF N_Elements(tmp) ge 5 THEN min = fix(tmp[4])
       idldt[i] =  Var_To_Dt(year,month,day,hour,min)
     ENDFOR 
   ENDIF ELSE BEGIN 
     Message,'Usage: idldt=vaptimes2idldt( vaptimes )',/cont
     print,'vaptimes are string of format: yyyy/mm/dd/hh/mi'
     idldt = 0
   ENDELSE 

   return, idldt 
END





