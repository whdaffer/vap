;+
; NAME:  Read_Area_Hdr
; $Id$
; PURPOSE:  Read the header of a GOES AREA file
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Goes AREA file utility
;
; CALLING SEQUENCE:  read_area_hdr,filename, date, time, lres, eres, inpath=inpath
; 
; INPUTS:  filename: The filename
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  inpath: The path. Should end in a '/'
;
; OUTPUTS:  date, time, lres, eres
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
; Revision 1.1  1999/10/06 22:57:21  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO read_area_hdr, file, date, time,lres,eres, $
                   inpath= inpath

openr,rlun,inpath + file, /get,error=err
IF err NE 0 THEN BEGIN
  message, !err_string,/cont
  return
ENDIF 

a =  area_hdr_str()
readu,1,a
date =  a.date
time =  a.time
lres =  a.lres
eres =  a.eres

return
end
