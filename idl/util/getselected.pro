;+
; NAME:  GetSelected
; $Id$
; PURPOSE:  Retrieve the 'selected' vectors from amongst the ambiguities
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Quickscat utility
;
; CALLING SEQUENCE:  getselected, wvc_sel, n_ambig, u, v, su, sv
; 
; INPUTS:  
;
;  wvc_sel: The ncells by nrows array that tells which ambiguity is
;           selected for each wind vector cell (0 means 'no
;           selection')
;  n_ambig: The ncells by nrows array that stores the number of
;           ambiguities that are 'good' for each wind vector cell.
;  U      : the 4 by ncells by nrows float array containing the
;           ambiguities in the U direction
;  V      : Mutatis Mutandis
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
;
;  SU     : The 'selected' U vectors. This array will have
;           NANs for each WVC that doesn't have a selected
;           ambiguity. If you use 'PLOTVECT' to plot them, it will be
;           transparent, as that routine will screen these out.
;  SV     : The 'selected' V vectors. 
;
;
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
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
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

pro getselected, sel, nambig, u,v,su,sv
    ;sel = rnoaa.wvc_sel-1
    x = where( nambig EQ 1 AND sel LT 0, nx )
    IF nx NE 0 THEN sel[x] =  0
    good = where( nambig GE 1 AND sel GT -1, ngood)
    bad = where(  nambig LT 1 OR  sel le -1, nbad)
    
    dims = size(u,/dim)
    nx = dims[1] &  ny=dims[2]

    tsel = sel
    tsel[bad] = 0
    tsel = tsel+lindgen(nx,ny)*4

    su = (sv=fltarr(nx,ny))
    su[*] = u[tsel]
    sv[*] = v[tsel]
    su[bad] = !values.f_nan
    sv[bad] = !values.f_nan
end
