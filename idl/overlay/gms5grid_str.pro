;+
; NAME:  gms5grid_str 
; PURPOSE:  Returns (and defines, if not already defined) a structure
;           for use with gms5grid data.
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  GMS5structure = gms5grid_str()
;
;
; 
; INPUTS:  None
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
; GMS5Gridstructure:  a structure of type gms5grid
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;     gms5grid_cmn: containing gms5grid_cmn, gms5grid_defined, gms5grid_size, gms5grid
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
; EXAMPLE:  rec=gms5grid_str(10) will create a vector of 10 structures of
;          type gms5grid
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION gms5grid_str

COMMON gms5grid_cmn, gms5grid_defined, gms5grid_size, gms5grid

rcsid = "$Id$"
IF n_elements( gms5grid_defined ) eq 0 THEN BEGIN

  gms5grid =  { GMS5GRID, $
                filename: '',$
                date: '',$
                minlon: 0., $  ; for use in distinguishing grida from grid files
                xloc: fltarr(241,241),$
                yloc: fltarr(241,241) }
  gms5grid_defined = 1
  gms5grid_size = TotSize(gms5grid)

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( {gms5grid} , nstruct )
end






