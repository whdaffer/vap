;+
; NAME:  hdfL1AParseGlobalAttr
; $Id$
; PURPOSE:  Parse a global attribute read from an L1A file
;
; AUTHOR:  William Daffer
;
; CATEGORY:  HDF utility (l1A file type)
;
; CALLING SEQUENCE:  parse_attribute=hdfl1aparseglobalattr(attribute)
; 
; INPUTS:  
;
;    attribute : The global attribute to be parsed. Not the name of
;                it, the actual result of reading the attribute from
;                the L1A HDF file.
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;  success:  the attribute as a string, disencumbered of all it's
;            accoutrements.
;  failure: the empty string.
;
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
;
; PROCEDURE:  Figure it out.
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION HdfL1AParseGlobalAttr, globalAttr

  IF n_params() Lt 1 THEN BEGIN 
    Usage,"data=HdfL1AParseGlobAttry(globalAttribute)"
    return,''
  ENDIF 

  lf = string(10b)
  tmp = str_sep(globalAttr,lf)
  nreps = tmp[1]
  data = strtrim(strcompress(tmp[2:2+nreps-1]),2)
  IF n_elements(data) EQ 1 THEN data = data[0]
  return, data

END

