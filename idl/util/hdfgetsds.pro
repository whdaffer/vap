;+
; NAME:  hdfgetsds
; $Id$
; PURPOSE:  Read one SDS from an HDF file
;
; AUTHOR:  Scott Dunbar or Vincent Hsiao (don't know which)
;
; CATEGORY:  HDF i/o
;
; CALLING SEQUENCE: data=hdfgetsds(file,sdsname) 
; 
; INPUTS:  
;
;   file: FQFN
;   sdsname: The SDS name.
;            Must be spelled *EXACTLY* as it is in the HDF file
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;  Success: The data is returned as a result of the function
;  Failure: the empty string.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
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
;Jet Propulsion Laboratory
;Copyright (c) 2001, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION hdfgetsds,file, sds_name

IF n_params() LT 2 THEN BEGIN 
  Message,"Usage: geoloc_data=hdfgetsds(file,sds_name)",/info
  return,''
ENDIF 
IF NOT isa(file,/string,/nonempty) THEN BEGIN 
  Message,"PARAM 1 <file> must be a STRING",/cont
  return,''
ENDIF 
IF NOT isa(sds_name,/string,/nonempty) THEN BEGIN 
  Message,"PARAM 2 <sds_name> must be a STRING",/cont
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

sd_id=hdf_sd_start(file,/read) 

r=hdf_sd_nametoindex(sd_id,sds_name) 
IF r EQ -1 THEN BEGIN 
  Message,"Can't find SD " + sds_name,/cont
  return,''
ENDIF 

sds_id=hdf_sd_select(sd_id,r) 
hdf_sd_getdata,sds_id,data 
hdf_sd_endaccess,sds_id 
hdf_sd_end,sd_id 

return, data
END

