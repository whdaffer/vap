;+
; NAME:  Satmovie_defs_str.pro
; $Id$
; PURPOSE:  Defines a structure to contain the definitions used in
;          running the satellite movie program.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  
;
; CALLING SEQUENCE:  structs = satmovie_defs_str(nstruct)
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  nstruct: the number you want: default=1
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  a vector of 'nstructs' satmovie_defs structures.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  satmovie_defs_cmn - for keeping track of whether this
;                                 structure has already been defined.
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
;
;Jet Propulsion Laboratory
;Copyright (c) 1997, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION satmovie_defs_str, nstruct


COMMON satmovie_defs_cmn, satmovie_defs_defined, satmovie_defs_size, satmovie_defs
;
satmovie_defs_size =  -1 ; record length in bytes.
IF n_elements( satmovie_defs_defined ) eq 0 THEN BEGIN
  satmovie_defs =  { desig: '',$ ; region of interest
                      vlonpar : fltarr(3),$ ; lon min/max/inc of vector field
                      vlatpar : fltarr(3),$ ; lat min/max/inc of vector field
                      loncent : 0.,$ ; central lon of satellite map projection
                      latcent : 0.,$ ; central lat of satellite map projection
                      limit   : fltarr(8),$ ; limits of map, left, uppper, right, lower: 
                                      ; lat, then lon
                                      ;so that limits([0,2,4,6]) are the
                                      ;lats 
                      min_vect : 0l,$ ; minimum number of vectors in field for successfull movie
                      interp_path : '',$   ; where to put interpolated wind files
                      wpath       : '',$ ; where to get wind data
                      anim_path   : ''} ; where to make the animations.
                       
  

  satmovie_defs_defined = 1

ENDIF

IF n_elements( nstruct ) eq 0 THEN nstruct = 1
IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( satmovie_defs, nstruct )
end





