;+
; NAME:  gms5datetime_str 
; PURPOSE:  Returns (and defines, if not already defined) a structure
;           for use with gms5datetime data.
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  vector_of_structures = gms5datetime_str(nstruct )
;
;
; 
; INPUTS:  
;  Nstruct - the dimensionality of the vector_of_structures.
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
; vector_of_structures: a vector of structures of type gms5datetime
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;     gms5datetime_cmn: containing gms5datetime_cmn, gms5datetime_defined, gms5datetime_size, gms5datetime
;
;
;
; SIDE EFFECTS:  
;
;   A copy of one structure is kept in common, so there is a
;   concomitant reduction in core memory. 
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
; EXAMPLE:  rec=gms5datetime_str(10) will create a vector of 10 structures of
;          type gms5datetime
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION gms5datetime_str, nstruct, redefine=redefine

COMMON gms5datetime_cmn, gms5datetime_defined, gms5datetime_size, gms5datetime

rcsid = "$Id$"
redefine =  keyword_set( redefine )
IF n_elements( gms5datetime_defined ) eq 0 OR redefine THEN BEGIN

  gms5datetime =  {GMS5DATETIME, $
                  year: '',$
                  month:'',$
                  day:'',$
                  hour:'',$
                  min:'' } 
  gms5datetime_defined = 1

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1
gms5datetime_size = 0

RETURN, replicate( gms5datetime , nstruct )
end






