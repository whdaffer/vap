;+
; NAME:  q2bsvh_str.pro
; $Id$
; PURPOSE:  defines structure used in reading SVH q2b data
;
;
; AUTHOR:
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  
;
;
; 
; INPUTS:  
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
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;
;
; SIDE EFFECTS:  
;
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
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-


FUNCTION q2bsvh_str, nstruct, redefine=redefine
COMMON q2bsvh_cmn, q2bsvh_defined, q2bsvh_size, q2bsvh
;

IF n_elements( q2bsvh_defined ) eq 0 OR keyword_set( redefine ) THEN BEGIN


  q2bsvh ={ $
          f77pad1   : 0l,$
          idx       : lonarr(2),$
          lat       : 0.0,$      
          lon       : 0.0,$      
          nambig    : 0L,$       
          qual      : 0l,$       
          sel       : 0l,$       
          sspeed    : 0.,$       
          sdir      : 0.,$       
          mspeed    : 0.,$       
          mdir      : 0.,$       
          speed     : fltarr(4),$
          dir       : fltarr(4),$
          like      : fltarr(4),$
          speed_err : fltarr(4),$
          dir_err   : fltarr(4),$
          f77pad2   : 0l }

  q2bsvh_defined = 1

ENDIF

IF n_elements( nstruct ) eq 0 THEN nstruct = 1
IF nstruct le 0 THEN nstruct = 1
q2bsvh_size =  n_tags( q2bsvh,/length )
RETURN, replicate( q2bsvh , nstruct )
END

