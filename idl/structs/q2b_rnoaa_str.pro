;+
; NAME:  q2b_rnoaa_str 
; PURPOSE:  Returns (and defines, if not already defined) a structure
;           for use with q2b_rnoaa data. This is the Reduced Noaa Near
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
; CALLING SEQUENCE:  vector_of_structures = q2b_rnoaa_str(nstruct, ncells=ncells )
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
; vector_of_structures: a vector of structures of type q2b_rnoaa
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;     q2b_rnoaa_cmn: containing q2b_rnoaa_cmn, q2b_rnoaa_defined, q2b_rnoaa_size, q2b_rnoaa
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
; EXAMPLE:  rec=q2b_rnoaa_str(10) will create a vector of 10 structures of
;          type q2b_rnoaa
;
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.1  1998/11/05 19:24:33  vapuser
; Initial revision
;
; Revision 1.1  1998/11/05 18:40:23  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
FUNCTION q2b_rnoaa_str, nstruct, ncells=ncells

COMMON q2b_rnoaa_cmn, q2b_rnoaa_nheader_recs, $
                      q2b_rnoaa_size, $
                      q2b_rnoaa_defined, $
                      q2b_rnoaa
       

q2b_rnoaa_nheader_recs = 1l

rcsid = "$Id$"
IF n_Elements(ncells) EQ 0 THEN ncells =  76
IF n_elements( q2b_rnoaa_defined ) eq 0 THEN BEGIN

  q2b_rnoaa =  {Q2B_RNOAA, $
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
        wvc_sel     : BYTARR(ncells) } 

  q2b_rnoaa_defined = 1

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1
q2b_rnoaa_size = 3980L

RETURN, replicate( q2b_rnoaa , nstruct )
end