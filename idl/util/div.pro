;+
; NAME:  div
; $Id$
; PURPOSE:  computer the divergence of a vector field.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: div=div( u,v,x,y)
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
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
; Revision 1.1  1999/10/06 21:16:41  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION div, u,v,x,y, tx,ty

  IF n_params() LT 4 THEN BEGIN 
    Usage,'divergence=div(u,v,x,y)'
    return,0
  ENDIF 

  dims = size(u,/dim)
  ncols = dims[0]
  nrows = dims[1]
  div    =  fltarr( ncols-1, nrows-1 )
  tu = u
  tv = v
  tx = x
  ty = y

  good = where( finite(u) AND finite(v), ngood )
  bad = where( finite(u) EQ 0 OR finite(v) EQ 0, nbad )
  IF nbad NE 0 THEN BEGIN 
    tu[bad] = (tv[bad]=0)
    FOR i=0,nrows-1 DO BEGIN 
      z = where( abs(x[*,i]) LE 1.e-10,nx)
      nz = where( abs(x[*,i]) GE 1.e-10,nnz)
      IF nnz GE ncols/10. THEN BEGIN 
        tmp = interpol(x[nz,i],nz,z)
        tx[z,i] = tmp
      ENDIF ELSE tx[*,i] = 0.
      
    ENDFOR 
    ;FOR i=0l,ncols-1 DO BEGIN 
    ;ENDFOR 
    
    tmp = interpol(y[good],good,bad)
    ty[bad] = tmp
  ENDIF 


  mag =  sqrt( tu^2 + tv^2 )
  g =  where( mag NE 0., nx )
  tu[g] =  tu[g]/mag[g]
  tv[g] = tv[g]/mag[g] &  g=0
  udiffs =  div
  vdiffs =  div

  t = tu[1:ncols-1,*] - tu[0:ncols-2,*]
;  dx =tx[1:ncols-1,*] - tx[0:ncols-2,*]
  FOR i=0,nrows-2 DO udiffs[*,i] =  t[*,i] + t[*,i+1]
;  tx = tx[0:ncols-2,0:nrows-2]+dx[*,0:nrows-2]

  t = tv[*,1:nrows-1] - tv[*,0:nrows-2]
;  dy =ty[*,1:nrows-1] - ty[*,0:nrows-2]
  FOR i=0,ncols-2 DO vdiffs[i,*]= t[i,*]+t[i+1,*]
;  ty = ty[0:ncols-2,0:nrows-2]+dy[0:ncols-1,*]

  div =  udiffs + vdiffs

  return,div
END
