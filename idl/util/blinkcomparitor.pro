;+
; NAME:  blinkcomparator
; $Id$
; PURPOSE:  blink between several images
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Image Utility
;
; CALLING SEQUENCE:  blinkcomparitor, array_of_images [,slice_dimension]
; 
; INPUTS:  
;
;  Array_of_images: Like it says. 
;
;
;
; OPTIONAL INPUTS:  
;
;  slice_dimension: tells whether the array of images is 
;
;      [3,x,y] (slice=0)
;      [x,3,y] (slice=1)
;      [x,y,3] (slice=2) (the default if slice_dimension is unspecified)
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  to screen. No assumption are made about the current screen
;          state. Nothing is tested. Output goes to default window, if
;          one is open.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  Windows open, if not already open. Too bad if it's
;               too small.
;
; RESTRICTIONS:  plot_device must = 'x'
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;
;Copyright (c) 2000, William Daffer
;-
; No Warranties!

PRO blinkcomparitor, imarray, slice=slice
   IF n_params() LT 1 THEN BEGIN 
     Usage,"Need IMARRAY!"
     RETURN
   ENDIF 

   nd = size(imarray,/n_dim)
   dims = size(imarray, /dim)

   IF nd NE 3 THEN BEGIN 
     Message,"Need more than one image to blink compare!",/cont
     return
   ENDIF 

   IF n_elements(slice) EQ 0 THEN slice =  2 
   nim = dims[slice]
   ii = 0
   modulus = nim
   ans = ''
   REPEAT BEGIN 
     CASE slice OF
       0: tv,imarray[ii,*,*]
       1: tv,imarray[*,ii,*]
       2: tv,imarray[*,*,ii]
     ENDCASE 
     ii = (ii+1) MOD modulus
     read,"Continue? [Y/n/q] ",ans
     ans = strmid( strtrim( strupcase(ans),2),0,1)
   ENDREP UNTIL ans EQ 'N' OR ans EQ 'Q'

   
END
