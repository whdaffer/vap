;+
; NAME:  gms5data_str 
; PURPOSE:  Returns (and defines, if not already defined) a structure
;           for use with gms5data data.
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  vector_of_structures = gms5data_str(nx, ny)
;
;
; 
; INPUTS:  
;  Nx : Number of pixels in the X direction
;  Ny : Number of pixels in the Y direction
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
; vector_of_structures: a vector of structures of type gms5data
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;     gms5data_cmn: containing gms5data_cmn, gms5data_defined, gms5data_size, gms5data
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
; EXAMPLE:  rec=gms5data_str(10) will create a vector of 10 structures of
;          type gms5data
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
FUNCTION gms5data_str

COMMON gms5data_cmn, gms5data_defined, gms5data_size, gms5data
COMMON gms5cal_cmn, gms5cal_defined, gms5cal_size, gms5cal
COMMON gms5image_cmn, gms5image_defined, gms5image_size, gms5image
COMMON gms5grid_cmn, gms5grid_defined, gms5grid_size, gms5grid

rcsid = "$Id$"
IF n_elements( gms5data_defined ) EQ 0 THEN BEGIN


  IF n_elements(gms5cal_defined ) EQ 0 THEN BEGIN 
    junk = gms5cal_str()
    junk = 0
  ENDIF 

  IF n_elements(gms5image_defined) EQ 0  THEN BEGIN 
    junk = gms5image_str()
    junk = 0
  ENDIF 

  IF  n_elements(gms5grid_defined) EQ 0  THEN BEGIN 
    junk = gms5grid_str()
    junk = 0
  ENDIF 

  
  gms5data =  {GMS5DATA, $
              caldata: {GMS5CALDATA},$
              imagedata: {GMS5IMAGE},$
              griddata: {GMS5GRID}}
  gms5data_defined = 1

  gms5data_size = TotSize(gms5data)

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( gms5data , nstruct )
end






