;+
; NAME:  
; $Id$
; PURPOSE:  Defines the q2bdata structure
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:   Data I/O
;
;
;
; CALLING SEQUENCE:  str=q2b_str(1 [,ncells=ncells ])
;
;
; 
; INPUTS:  
;
;    nstruct : the number of structure you want defined.
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;    ncells: the number of cells in the output WVC parts of the
;            structure. default=76
;
;    redefine: A flag to allow for redefinition of the structure,
;              probably won't work now that it's a named structure.
;
;
; OUTPUTS:  
;
;     An array of `nsctruct' structures of type q2bdata
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  q2b_cmn
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
; Revision 1.4  2000/02/23 21:23:45  vapuser
; Added rain_flag fields
;
; Revision 1.3  1999/11/12 19:58:01  vapuser
; Added fields (su2/sv2) to support new DIRTH selected vectors in the
; new L2B data product. (per Bryan's request) Took out 'sel2' as it
; wasn't being used.
;
; Revision 1.2  1998/10/05 23:14:45  vapuser
; Added Rowtime, a string.
;
; Revision 1.1  1998/10/05 22:48:02  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION q2b_str, nstruct, ncells= ncells, redefine=redefine

COMMON q2b_cmn, q2b_defined, q2b_size, q2b, numcells

rcsid = "$Id$"
redefine =  keyword_set( redefine )

IF n_elements( ncells ) EQ 0 THEN numcells = 76 ELSE numcells =  ncells 
;

IF n_elements( q2b_defined ) eq 0 OR redefine THEN BEGIN

  q2b =  {q2bdata, $
          u      : fltarr(4,numcells) ,$  
          v      : fltarr(4,numcells) ,$  
;          mle    : fltarr(4,numcells) ,$
          lon    : fltarr(  numcells)   ,$  
          lat    : fltarr(  numcells)   ,$                
          sel    : lonarr(  numcells)   ,$                
;          sel2   : lonarr(  numcells)   ,$                
          idx    : lonarr(  numcells)   ,$ 
          rain_flag: intarr(numcells), $
          row    : 0L                   ,$                
          rowtime: ''                   ,$
          qual   : lonarr(  numcells)   ,$                
          nambig : lonarr(  numcells)   ,$                
          mu     : fltarr(  numcells)   ,$                
          mv     : fltarr(  numcells)   ,$                
          su     : fltarr(  numcells)   ,$ ; Selected 'u' 
          sv     : fltarr(  numcells)   ,$ ; Selected 'v' 
          smle   : fltarr( numcells)    ,$ ; mle for the 'selected'
;          tb_h   : intarr(numcells,2)   ,$ ; mean/stdev
;          tb_v   : intarr(numcells,2)   ,$ ; mean/stdev
;          num_tb : bytarr(numcells,2)   ,$ ; H/V
;          tb_rainrate: intarr(numcells), $ ; 
;          tb_atten:    intarr(numcells), $
          su2    : fltarr(  numcells)   ,$ ; DIRTH selected 'u' 
          sv2    : fltarr(  numcells)   $ ; DIRTH selected 'v' 
   }                    

  ; The Level 2B files now have the DIRTH processed vectors. This is
  ; what will be in the su2,sv2 fields if those vectors are
  ; present. Otherwise, those fields will be copies of the su/sv
  ; fields or will be identically 0. Wasteful, I know. But I can't
  ; think of what else to do.

  q2b_defined = 1

ENDIF
IF n_elements( nstruct ) eq 0 THEN nstruct = 1

IF nstruct le 0 THEN nstruct = 1
q2b_size = N_tags(q2b(0),/length ); record length in bytes.

RETURN, replicate( q2b , nstruct )
end





