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
; Revision 1.1  2001/12/11 22:26:32  vapdev
; Renamed from vap/idl/util to util/idl
;
; Revision 1.3  2001/12/08 00:02:37  vapdev
; Getting rid of obsolete RSI routines and fixing ENV vars
;
; Revision 1.2  2001/12/05 00:23:17  vapuser
; default the second field
;
; Revision 1.1  1998/10/08 16:57:25  vapuser
; Initial revision
;
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
       tmp   = strsplit( vaptimes(i), '/' ,/extract)
       year  = fix(tmp[0])
       month = fix(tmp[1])
       day   = fix(tmp[2])
       hour  = fix(tmp[3])
       min = 0
       IF N_Elements(tmp) ge 5 THEN min = fix(tmp[4])
       sec = 0
       IF n_elements(tmp) GE 6 THEN sec =  fix(tmp[5])
       idldt[i] =  Var_To_Dt(year,month,day,hour,min,sec)
     ENDFOR 
   ENDIF ELSE BEGIN 
     Message,'Usage: idldt=vaptimes2idldt( vaptimes )',/cont
     print,'vaptimes are string of format: yyyy/mm/dd/hh[/mi/sec]'
     idldt = 0
   ENDELSE 

   return, idldt 
END





