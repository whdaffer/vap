;+
; NAME:  hdfgetvd.pro
; $Id$
; PURPOSE:  Return the named Vdata from the named HDF file
;
; AUTHOR:  whd
;
; CATEGORY:  hdf i/o
;
; CALLING SEQUENCE: Vdata = hdfgetVd(file, vdname) 
; 
; INPUTS:  
;
;   file: FQFN
;   vdname: the Vdata name
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;   Failure: the empty string
;   Success: the Vdata.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 2001, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION hdfgetvd,file, vd_name

IF n_params() LT 2 THEN BEGIN 
  Message,"Usage: geoloc_data=hdfgetvd(file,vd_name)",/info
  return,''
ENDIF 
IF NOT isa(file,/string,/nonempty) THEN BEGIN 
  Message,"PARAM 1 <file> must be a STRING",/cont
  return,''
ENDIF 
IF NOT isa(vd_name,/string,/nonempty) THEN BEGIN 
  Message,"PARAM 2 <vd_name> must be a STRING",/cont
  return,''
ENDIF 

IF NOT hdf_ishdf(file) THEN BEGIN 
  Message,"<" + file + "> is not an HDF file",/cont
  return,''
ENDIF 


  ;; Hope this catchs generic HDF errors.
catch, error
IF error NE 0 THEN BEGIN 
  Message,!error_state.msg
  return,''
ENDIF 


fid = hdf_open(file)
ref=hdf_vd_find(fid,vd_name)  

IF ref EQ 0 THEN BEGIN 
  Message,"Can't find Vdata " + vd_name,/cont
  hdf_close,fid
  return,''
ENDIF

vd = hdf_vd_attach(fid,ref)
r = hdf_vd_read(vd,data)
hdf_vd_detach,vd
hdf_close,fid

return, data
END

