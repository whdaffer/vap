;+
; NAME:  wfdt_str 
; $Id$
; PURPOSE:  Returns (and defines, if not already defined) a structure
;           for use with wfdt data.
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  vector_of_structures = wfdt_str(nstruct )
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
; vector_of_structures: a vector of structures of type wfdt
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;     wfdt_cmn: containing wfdt_cmn, wfdt_defined, wfdt_size, wfdt
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
; EXAMPLE:  rec=wfdt_str(10) will create a vector of 10 structures of
;          type wfdt
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
FUNCTION wfdt_str, nstruct, redefine=redefine

COMMON wfdt_cmn, wfdt_defined, wfdt_size, wfdt

rcsid = "$Id$"
redefine =  keyword_set( redefine )

IF n_elements( wfdt_defined ) eq 0 OR redefine THEN BEGIN

  wfdt =  {WFDT, $
           name: '', $
           start_time:{IDLDT}, $
           end_time:{IDLDT} }
  wfdt_defined = 1

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1
wfdt_size = n_tags(wfdt,/length)

RETURN, replicate( wfdt , nstruct )
end






