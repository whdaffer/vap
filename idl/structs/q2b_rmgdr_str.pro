;+
; NAME:  q2b_rmgdr_str 
; PURPOSE:  Returns (and defines, if not already defined) a structure
;           for use with q2b_rmgdr data.
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  vector_of_structures = q2b_rmgdr_str(nstruct )
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
; vector_of_structures: a vector of structures of type q2b_rmgdr
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;     q2b_rmgdr_cmn: containing q2b_rmgdr_cmn, q2b_rmgdr_defined, q2b_rmgdr_size, q2b_rmgdr
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
; EXAMPLE:  rec=q2b_rmgdr_str(10) will create a vector of 10 structures of
;          type q2b_rmgdr
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
FUNCTION q2b_rmgdr_str, nstruct

COMMON q2b_rmgdr_cmn, q2b_rmgdr_nheader_recs, $
                      q2b_rmgdr_size, $
                      q2b_rmgdr_defined, $
                      q2b_rmgdr
       

q2b_rmgdr_nheader_recs = 0l

rcsid = "$Id$"
redefine =  keyword_set( redefine )
IF n_elements( q2b_rmgdr_defined ) eq 0 THEN BEGIN

  q2b_rmgdr =  {Q2B_RMGDR, $
        Row_Time    : BYTARR(24),     $
        Rev         : 0,              $
        WVC_Row     : 0,              $
        WVC_Lat     : INTARR(76),     $
        WVC_Lon     : INTARR(76),     $
        wvcqual_flag: INTARR(76),     $
        model_speed : INTARR(76),     $
        model_dir   : INTARR(76),     $
        nambig      : BYTARR(76),     $
        windspd     : intarr(4,76),   $    
        winddir     : intarr(4,76),   $    
        errspd      : intarr(4,76),   $   
        errdir      : intarr(4,76),   $   
        mle_like    : intarr(4,76),   $     
        wvc_sel     : BYTARR(76),     $    
        num_sig0    : bytarr(76),     $     
        cen_lat     : intarr(4,76),   $     
        cen_lon     : intarr(4,76),   $     
        cell_azi    : intarr(4,76),   $     
        inc_angle   : intarr(4,76),   $             
        sigma0      : intarr(4,76),   $     
        coeff_a     : intarr(4,76),   $           
        coeff_b     : intarr(4,76),   $           
        coeff_c     : fltarr(4,76),   $           
        attenuation : intarr(4,76),   $
        sig0_qual   : intarr(4,76),   $
        sig0_mode   : intarr(4,76),   $
        surface     : intarr(4,76)    } 

  q2b_rmgdr_defined = 1

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1
q2b_rmgdr_size = 11960l

RETURN, replicate( q2b_rmgdr , nstruct )
end
