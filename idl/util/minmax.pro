function minmax, array
;+
; NAME:  minmax.pro
; $Id$
; PURPOSE:  Return Minimum and Maximum of input array
;
; AUTHOR:  William Daffer
;
; CATEGORY:  General array manipulation
;
; CALLING SEQUENCE:  minmax=minmax(array)
; 
; INPUTS:  
;
;  Array: An array
;
; OPTIONAL INPUTS:   none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;  [min,max] if success
;  [-1,0] if failure
;
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  none
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; PROCEDURE:  if array doesn't exist, return [-1,1], else return [min(array,max=mx),mx]
; EXAMPLE:  
; MODIFICATION HISTORY:
;
; $Log$
;
; Copyright William Daffer, 1999
;-
mn =  1 &  mx= -1
IF n_elements( array ) NE 0 THEN $
  mn = min( array, max=mx ) ELSE $
  message,' input array is undefined ',/cont
return,[ mn, mx ]
end

