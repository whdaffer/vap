;+
; NAME:  hdfattr
; $Id$
; PURPOSE:  Goes through an HDF file and prints information about the
;          Global Attributes.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  HDF Utility
;
; CALLING SEQUENCE:  hdfattr, filename
; 
; INPUTS:  filename: scalar string
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  nonen
;
; OUTPUTS:  All to screen
;
; OPTIONAL OUTPUTS:  none
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
; Revision 1.1  1999/10/06 21:28:23  vapuser
; Initial revision
;
;
;Copyright (c) 1999, William Daffer
; No Warranties!
;-

PRO hdfattr, file
  IF n_params() LT 1 THEN BEGIN 
    Usage,"hdfattr,filename"
    return
  ENDIF 

  IF NOT isa(file,/string,/nonempty) THEN BEGIN 
    Message,"File must be non-empty string!",/cont
    return
  ENDIF 
  fileid = hdf_sd_start(file)
  IF fileid LE 0 THEN BEGIN 
    Message,"Error opening file " + file,/cont
    return
  ENDIF 
  
  hdf_sd_fileinfo, fileid, ndatasets, nattributes
  lf = string(10b)
  FOR i=0,nattributes-1 DO BEGIN 
    hdf_sd_attrinfo,fileid,i,name=name,type=type,count=count,data=data
    IF VarType(data) EQ 'STRING' THEN BEGIN 
      tmp =  strsplit(  data, lf ,/extract) 
      tmp = tmp(where(strlen(tmp)))
      data =  tmp(n_elements(tmp)-1)
    ENDIF 
    print,name, ' = ', data
  ENDFOR 
  hdf_sd_end, fileid
END
