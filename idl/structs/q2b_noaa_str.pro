;+
; NAME:  q2b_noaa_str 
; PURPOSE:  Returns (and defines, if not already defined) a structure
;           for use with q2b_noaa data. This is the Full Noaa Near
;           Real time data made by Scott Dunbar's Noaa NRT processor.
;
;
; AUTHOR: WHD
;
;
; CATEGORY:  Data I/O
;
;
;
; CALLING SEQUENCE:  vector_of_structures = q2b_noaa_str(nstruct )
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
; vector_of_structures: a vector of structures of type q2b_noaa
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;     q2b_noaa_cmn: containing q2b_noaa_cmn, q2b_noaa_defined, q2b_noaa_size, q2b_noaa
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
; EXAMPLE:  rec=q2b_noaa_str(10) will create a vector of 10 structures of
;          type q2b_noaa
;
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.1  1998/11/05 18:40:23  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION q2b_noaa_str, nstruct, ncells=ncells

COMMON q2b_noaa_cmn, q2b_noaa_nheader_recs, $
                      q2b_noaa_size, $
                      q2b_noaa_defined, $
                      q2b_noaa
       

q2b_noaa_nheader_recs = 0l

rcsid = "$Id$"
redefine =  keyword_set( redefine )
IF n_elements(ncells) EQ 0 THEN ncells = 76
IF n_elements( q2b_noaa_defined ) eq 0 THEN BEGIN

  q2b_noaa =  {Q2B_NOAA, $
        Row_Time    : BYTARR(24),     $
        Rev         : 0,              $
        WVC_Row     : 0,              $
        WVC_Lat     : INTARR(ncells),     $
        WVC_Lon     : INTARR(ncells),     $
        wvcqual_flag: INTARR(ncells),     $
        model_speed : INTARR(ncells),     $
        model_dir   : INTARR(ncells),     $
        nambig      : BYTARR(ncells),     $
        windspd     : intarr(4,ncells),   $    
        winddir     : intarr(4,ncells),   $    
        errspd      : intarr(4,ncells),   $   
        errdir      : intarr(4,ncells),   $   
        mle_like    : intarr(4,ncells),   $     
        wvc_sel     : BYTARR(ncells),     $    
        num_sig0    : bytarr(ncells),     $     
        cen_lat     : intarr(4,ncells),   $     
        cen_lon     : intarr(4,ncells),   $     
        cell_azi    : intarr(4,ncells),   $     
        inc_angle   : intarr(4,ncells),   $             
        sigma0      : intarr(4,ncells),   $     
        coeff_a     : intarr(4,ncells),   $           
        coeff_b     : intarr(4,ncells),   $           
        coeff_c     : fltarr(4,ncells),   $           
        attenuation : intarr(4,ncells),   $
        sig0_qual   : intarr(4,ncells),   $
        sig0_mode   : intarr(4,ncells),   $
        surface     : intarr(4,ncells)    } 

  q2b_noaa_defined = 1

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1
q2b_noaa_size = 11960l

RETURN, replicate( q2b_noaa , nstruct )
end
