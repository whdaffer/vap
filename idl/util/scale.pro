;+
; NAME:  scale
; $Id$
; PURPOSE:  Scales arrays to the range 0 to 1
;
; AUTHOR:  William Daffer
;
; CATEGORY:  General array manipulation
;
; CALLING SEQUENCE:  scaled_array=scale(array,minv=minv,maxv=maxv,/double])
; 
; INPUTS:  
;
;   The array to scale. A float  (or double) copy is made of this array, which is
;   then used in all manipulations.
;
; OPTIONAL INPUTS:  
;       
; KEYWORD PARAMETERS:  
;
;
;   MinV: The value (possible existing in the input array) that gets mapped to 0.
;   MaxV: The value (possible existing in the input array) that gets mapped to 1.
;   Double: If set, manipulations are to a double array.
;
; OUTPUTS:  
;
;   Failure: the null string
;   Success: the scaled array (float or double, as depends on presence
;            of /double keyword)
;
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  No strings, Objects, pointers, structures or undef'd
;               or complex values
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.2  1998/11/25 19:11:31  vapuser
; Took 'scale' out of call to Usage
;
; Revision 1.1  1998/11/25 19:06:34  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION scale, array, minv=minv, maxv=maxv, double=double

  double =  keyword_set(double)

  IF n_params() LT 1 THEN BEGIN 
    Usage, 'scaled_array=scale( array [, minv=minv, maxv=maxv, /double] )'
    return,''
  ENDIF 

  vt = vartype(array)
  scaled_array = ''

  CASE VT OF 
    'STRING'    : Message,"Can't scale strings",/cont
    'STRUCTURE' : Message,"Can't scale structures",/cont
    'OBJECT'    : Message,"Can't scale objects",/cont
    'POINTER'   : Message,"Can't scale pointers",/cont
    'UNDEFINED' : Message,"Can't scale undefined things",/cont
    'COMPLEX'   : Message,"Can't scale Complex things",/cont
    'COMPLEX_DOUBLE' : Message,"Can't scale Complex things",/cont
    ELSE: BEGIN 
      sc = 1.0
      IF double THEN BEGIN 
        IF VT EQ 'DOUBLE' THEN $
          scaled_array = array ELSE $
          scaled_array = 1.0d*array
      ENDIF ELSE BEGIN 
        IF VT EQ 'FLOAT' THEN $
          scaled_array = array ELSE $
          scaled_array = 1.0*array
      ENDELSE 
      IF n_Elements(minv) EQ 0 THEN minv = min(array,max=mx)
      IF n_elements(maxv) EQ 0 THEN maxv = mx
      scaled_array =  ( (minv> scaled_array < maxv) -minv)/(maxv-minv)

    END
  ENDCASE 
  return,scaled_array
END
    

