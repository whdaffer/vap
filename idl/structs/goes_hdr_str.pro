;+
; NAME:  Goes_Hdr_Str.pro
; $Id$
; PURPOSE:  Defines the structure to be used in reading the header of
;          files created by the grid_goes.c program.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Gridded Goes file/Data manipulation
;
; CALLING SEQUENCE:  structures = goes_hdr_str(nstruct)
; 
; INPUTS:  
;
; OPTIONAL INPUTS:   nstruct : the number of structure you want (default=1)
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  structures - a 'nstruct' vector of the structures defined below.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  goes_hdr_cmn - for keeping track of whether this
;                                 structure has already been defined.
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
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION goes_hdr_str, nstruct

COMMON goes_hdr_cmn, goes_hdr_defined, goes_hdr_size, goes_hdr
;

IF n_elements( goes_hdr_defined ) eq 0 THEN BEGIN

  goes_hdr = { $
              version     : 0l,$
              type        : 0l,$
              rows        : 0l,$
              cols        : 0l,$
              year        : 0l,$
              doy         : 0l,$
              hhmm        : 0l,$
              resolution  : fltarr(2),$
              limits      : fltarr(4),$
              areafilename: bytarr(256) }
  goes_hdr_defined = 1

ENDIF

goes_hdr_size = n_tags( goes_hdr, /length )
IF n_elements( nstruct ) eq 0 THEN nstruct = 1
IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( goes_hdr, nstruct )
end





