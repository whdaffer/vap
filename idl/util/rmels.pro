;+
; NAME:  rmels.pro
; $Id$
; PURPOSE: remove the indicated elements, reduce array accordingly,
;          (unless the /nondestructive keyword is set) and return the
;          removed elements.
;
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: 
;
;   removed_elements=rmels( array, index_vector [,/nondestructive] ) 
; 
; INPUTS:  
;
;  array: The array that is to be shortened.
;  index_vector: the indices of the elements to be removed.
;
; OPTIONAL INPUTS:  
;       
; KEYWORD PARAMETERS:  
;
;   nondestructive : flag. If set, the input array is not shortened.
;
; OUTPUTS:  
;
;  Success: A vector having the same number of elements as
;           'index_vector' containing the elements of 'array' that
;           were in those indices.
;`
;  Error: A scalar !values.f_nan
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:   The elements of 'ARRAY' are removed, array is
;               correspondingly shortened, and turned into a 1d array.
;
; If the input indices comprise the entire array, and the
; /nondestructve flag is clear, a message is emitted, but the function
; continues, removing (and thereby undefining) the input array and
; returning it in the return argument 'retvals.' As this will probably
; be confusing, the user is advised to check against this possibility
; before calling this function.
;
;
; RESTRICTIONS:  The indices argument should be either integer or
;                float 'type' (i.e. byte, integer, longword, float, or
;                double)
;
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
; William Daffer
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION rmels, array, indices, nondestructive=nondestructive

  destructive =  keyword_set(nondestructive) EQ 0

  retvals = !values.f_nan
  IF n_params() LT 2 THEN BEGIN 
    Usage,"removed_elements = rmels( array, indices [,/nondestructive] )"
    return, retvals
  ENDIF 

  IF NOT (isa( indices, /type_integer) OR $
          isa( indices,/type_float) ) THEN BEGIN 
    Message,'INDICES must be either integer or float type',/cont
    return, retvals
  ENDIF 

  IF min(indices,max=mx) LT 0 OR $
     mx GT n_elements(array)-1 THEN BEGIN 
    Message,'Out of range INDICES, must be in [0,n_elements(array)-1]',/cont
    return, retvals
  ENDIF 

  tindices = indices[ uniq( indices, sort(indices) ) ]

  IF n_elements(tindices) EQ n_elements(array) THEN BEGIN 
    IF  destructive THEN BEGIN 
      Message,"All elements are to be removed!"
      return, temporary( array )
    ENDIF ELSE return, array
  ENDIF 

  tidx =  replicate(1b,n_elements(array))
  retvals = array[tindices]

  IF destructive THEN BEGIN 
    tidx[tindices] = 0
    array =  temporary( array[ where( tidx ) ] )
  ENDIF 

  return, retvals
END

