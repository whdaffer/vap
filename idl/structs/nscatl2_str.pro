;+
; NAME:  nscatl2_str.pro
; $Id$
; PURPOSE:  Used to read Nscat L2 data.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Nscat L2 data I/O
;
; CALLING SEQUENCE:  structures = nscatl2_str(nstruct)
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  nstruct: the number you want (def=1)
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  none
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  nscatl2_cmn - for keeping track of whether this
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
;Copyright (c) 1996, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION nscatl2_str, nstruct

COMMON nscatl2_cmn, nscatl2_size, nscatl2_defined
;
NSCATL2_SIZE =  1456l; record length in bytes.

IF n_elements( nscatl2_defined ) eq 0 THEN BEGIN

  nscatl2 =  { nscatl2                   ,$  
               time       : bytarr(24)   ,$  
               row        : 0            ,$         
               spare1     : bytarr(6)    ,$  
               low_wind   : 0l           ,$        
               hi_wind    : 0l           ,$  
               lat        : intarr(24)   ,$  
               lon        : intarr(24)   ,$  
               col        : bytarr(24)   ,$  
               nsig       : bytarr(24)   ,$  
               nbeam12    : bytarr(24)   ,$  
               nbeam34    : bytarr(24)   ,$  
               nbeam56    : bytarr(24)   ,$  
               nbeam78    : bytarr(24)   ,$  
               qualflag   : bytarr(24)   ,$  
               meanwind   : intarr(24)   ,$  
               windspd    : intarr(24,4) ,$ ; NB, reverse of (r)mgdr
               winddir    : intarr(24,4) ,$
               errspd     : intarr(24,4) ,$
               errdir     : intarr(24,4) ,$
               mle_like   : intarr(24,4) ,$
               nambig     : bytarr(24)   ,$
               wvc_sel    : bytarr(24)   ,$
               spare2     : bytarr(24,4)  $
               }

  nscatl2_defined = 1

ENDIF

IF n_elements( nstruct ) eq 0 THEN nstruct = 1
IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( { nscatl2 } , nstruct )
end





