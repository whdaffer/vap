;+
; NAME:  Reallocv
; $Id$
; PURPOSE:  Make a vector *bigger*, preserving the data that's already
;          in the vector. 
;
;          This is for the case where you're filling an vector with
;          data and you miscalculated (or couldn't know a priori) how
;          big the vector had to be, so you need to make it larger.
;
;          Making it smaller or changing the shape of the vector in
;          other ways is for other routines.
;
; AUTHOR:  William H. Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: newvector=reallocv(vector,dim [,value]) 
; 
; INPUTS:  
;
;  vector: A 1d vector. 
;
;          No checking is done, so if you send in a
;          multi-dimensioned array, you probably won't get out what
;          you want.
;
;  dim: the new size
;       If dim <= n_elements(vector) the call to this routine is a
;       no-op and the input vector is returned. If you want to make
;       the vector smaller, do it yourself or use
;       rebin/congrid... something like that.
;
; OPTIONAL INPUTS:  
;
;  value: A scalar to be used in initializing the array. If not
;         present, use vector[0]. It's assumed that whatever process
;         is calling this routine will deal with the implications of
;         this operation.
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  the new vector (or the same one, if dim <= n_elements(vector))
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  The newvector is filled with vector[0] (or `value')
;               in all locations before the old data is copied into
;               the new vector. The calling process has to deal with this.
;
; RESTRICTIONS:  You should only call this with vectors. No checks are
;               made. caveat user
;
; PROCEDURE:  
;
;  Look at the code
;
; EXAMPLE:  
;
;   Say you're filling a vector with data and you find that you're
;   initial estimate of the size of this vector is too small. So
;   you've got something like:
;
;   nv=1000
;   vector=fltarr(nv)
;   p=0
;   for i=0,n_elements(files)-1 do begin 
;     data=read(file)
;     nn=n_elements(data)
;     if p+nn ge nv then begin 
;        nv=nv+1000
;        vector=reallocv(vector,nv,0.)
;     endif 
;     vector[p:nn-1] = temporary(data)
;   endfor 
;
; MODIFICATION HISTORY:
;
; $Log$
;
;
;Jet Propulsion Laboratory
;Copyright (c) 2001, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION reallocv, vector, dim, value
  IF n_params() LT 2 THEN return,vector
  nn = n_elements(vector)
  IF dim LE  nn THEN return, vector
  IF n_elements(value) EQ 0 THEN value = vector[0]
  newvector =  replicate(value,dim)
  newvector[0:nn-1] =  temporary(vector)
  vector = temporary(newvector)
  return, vector
END 
