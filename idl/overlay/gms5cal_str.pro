;+
; NAME:  gms5cal_str 
; PURPOSE:  Returns (and defines, if not already defined) a structure
;           for use with gms5cal data.
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  vector_of_structures = gms5cal_str(nstruct )
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
; vector_of_structures: a vector of structures of type gms5caldata
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;     gms5cal_cmn: containing gms5cal_cmn, gms5cal_defined, gms5cal_size, gms5cal
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
; EXAMPLE:  rec=gms5cal_str(10) will create a vector of 10 structures of
;          type gms5cal
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
FUNCTION gms5cal_str, nstruct, redefine=redefine

COMMON gms5cal_cmn, gms5cal_defined, gms5cal_size, gms5cal

rcsid = "$Id$"
redefine =  keyword_set( redefine )
IF n_elements( gms5cal_defined ) eq 0 OR redefine THEN BEGIN


    IrData = { GMS5IRCALDATA, $
               valid    : 0, $
               beta     : fltarr(7), $
               Gradient : 0., $
               Intercept: 0., $
               Bias     : 0., $
               Temps    : fltarr(256) }



    gms5cal = { GMS5CALDATA, $
                  id:0, $
                  date:0.0d, $
                  sensel: 0, $
                  Ir: replicate({GMS5IRCALDATA},3), $
                  VisAlbedo: fltarr(64) }
    
  gms5cal_defined = 1

  gms5cal_size = TotSize( gms5cal )

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( {GMS5CALDATA} , nstruct )
end






