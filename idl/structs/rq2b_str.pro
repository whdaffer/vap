;+
; NAME:  rq2b_str 
; $Id$
; PURPOSE:  Returns (and defines, if not already defined) a structure
;           for use with rq2b data.
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  vector_of_structures = rq2b_str(nstruct )
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
; vector_of_structures: a vector of structures of type rq2b
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;     rq2b_cmn: containing rq2b_cmn, rq2b_defined, rq2b_size, rq2b
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
; EXAMPLE:  rec=rq2b_str(10) will create a vector of 10 structures of
;          type rq2b
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION rq2b_str, nstruct, ncells= ncells, redefine=redefine

COMMON rq2b_cmn, rq2b_defined, rq2b_size, rq2b

rcsid = "$Id$"
redefine =  keyword_set( redefine )

;
IF n_elements( ncells ) EQ 0 THEN numcells = 76 ELSE numcells =  ncells 

IF n_elements( rq2b_defined ) eq 0 OR redefine THEN BEGIN

  rq2b =  {RQ2BDATA, $
          su     : fltarr(numcells)   ,$ ; Selected 'u' 
          sv     : fltarr(numcells)   ,$ ; Selected 'V' 
          lon    : fltarr(numcells)   ,$  
          lat    : fltarr(numcells)   ,$                
          sel    : lonarr(numcells)   ,$                
          row    : 0L                 ,$                
          rowtime: ''                 ,$
          qual   : lonarr(numcells)   ,$                
          nambig : lonarr(numcells)   ,$                
          mu     : fltarr(numcells)   ,$; Model U              
          mv     : fltarr(numcells)   } ; Model V

  rq2b_defined = 1

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1
rq2b_size =

RETURN, replicate( rq2b , nstruct )
end






