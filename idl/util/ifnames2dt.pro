;+
; NAME:  Ifnames2Dt
; $Id$
; PURPOSE:  get time from name of file
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  File Manipulation
;
;
;
; CALLING SEQUENCE:  
;
;    times=Ifnames2Dt( Filenames )
;
;
; 
; INPUTS:  
;
;   fileNames : Interpolated file names
;               ({Q,N}IF-yyyymmddhhmi.{bin,hdf})
;
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
; OUTPUTS:  
;
;    fileTimes : Array of IDLDT structures.
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
; RESTRICTIONS:  None
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
; Revision 1.2  1998/11/23 23:37:40  vapuser
; Added call to basename
;
; Revision 1.1  1998/10/08 18:09:23  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-


FUNCTION Ifnames2dt, IFNames
   rcsid = "$Id$"
   ; it is assumed the file names have the format xxx-yyyymmddhhmi.ext
  dts = 0
  tIfNames = basename(IFNames)
  IF n_Params() NE 0 THEN BEGIN
    nn = n_elements(tifnames)
    dts = replicate({IDLDT},nn)
    FOR i=0,nn-1 DO BEGIN 
      tmp = strsplit(tIFNames[i],'-',/extract)
      year  = fix(strmid(tmp[1], 0,  4))
      month = fix(strmid(tmp[1], 4,  2))
      day   = fix(strmid(tmp[1], 6, 2))
      hour  = fix(strmid(tmp[1], 8, 2))
      min   = fix(strmid(tmp[1], 10, 2))

      dts[i] = var_to_dt( year, month, day, hour, min )
    ENDFOR 
    
  ENDIF 
  return, dts
end
