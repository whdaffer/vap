;+
; NAME:  gms5image_str 
; PURPOSE:  Returns (and defines, if not already defined) a structure
;           for use with gms5image data.
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  vector_of_structures = gms5image_str(nstruct )
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
; vector_of_structures: a vector of structures of type gms5image
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;     gms5image_cmn: containing gms5image_cmn, gms5image_defined, gms5image_size, gms5image
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
; EXAMPLE:  rec=gms5image_str(10) will create a vector of 10 structures of
;          type gms5image
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
FUNCTION gms5image_str

COMMON gms5image_cmn, gms5image_defined, gms5image_size, gms5image

rcsid = "$Id$"
IF n_elements( gms5image_defined ) eq 0 THEN BEGIN

  gms5image =  { GMS5IMAGE, $
                 filename: '',$
                 date: '',$
                 image: BYTARR(2291, 2291 ),$
                 palette: bytarr(3,256) }
  gms5image_defined = 1
  gms5image_size = TotSize(gms5image)

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( {gms5image} , nstruct )
end






