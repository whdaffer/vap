;+
; NAME:  hist_3d.pro
; $Id$
; PURPOSE:  Create a 3 dimensional histogram
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Statistics
;
; CALLING SEQUENCE:  hist3d=hist_3d(a1,a2,a3 $
;                                  [,min1=min1,max1=max1,bin1=bin1, $
;                                   ,min2=min2,max2=max2,bin2=bin2, $
;                                   ,min3=min3,max3=max3,bin3=bin3 ] )
;
; 
; INPUTS:  
;
;  A1, A2, A3. Arrays with which to create the 3d histogram
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  min{1,2,3}: The minimum or A{1,2,3} to be used in the
;                      histograming. Default = minimum(A{1,2,3})
;                      max{1,2,3}: simile Default = maximum(A{1,2,3})
;                      Bin{1,2,3}: simile, Default=1.
;
; OUTPUTS: 
;
;  Success: The 3d histogram
;  Error: a scalar -1
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  A huge amount of memory is used, temporarily
;
; RESTRICTIONS:  none
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1998,William Daffer
;-
; No Warranties!
;

FUNCTION hist_3d, x1,x2,x3, $
                  min1=min1,max1=max1,$
                  min2=min2,max2=max2,$
                  min3=min3,max3=max3,$
                  bin1=bin1,bin2=bin2,bin3=bin3

IF n_Params() LT 3 THEN BEGIN 
  Usage, 'hist3d=hist_3d(a1,a2,a3 [,min1=min1,max=max1,bin1=bin1,min2=min2,max=max2,bin2=bin2,min3=min3,max=max3,bin3=bin3  )'
  return,-1
ENDIF 

catch, error
IF error NE 0 THEN BEGIN 
  message,!error_state.msg,/cont
  return,-1
ENDIF 

m1 = min(x1,max=mx1)
m2 = min(x2,max=mx2)
m3 = min(x3,max=mx3)

IF n_elements(min1) EQ 0 THEN min1 = m1
IF n_elements(min2) EQ 0 THEN min2 = m2
IF n_elements(min3) EQ 0 THEN min3 = m3
IF n_elements(max1) EQ 0 THEN max1 = mx1
IF n_elements(max2) EQ 0 THEN max2 = mx2
IF n_elements(max3) EQ 0 THEN max3 = mx2
IF n_elements(bin1) EQ 0 THEN bin1 = 1L
IF n_elements(bin2) EQ 0 THEN bin2 = 1L
IF n_elements(bin3) EQ 0 THEN bin3 = 1L

n1 =  floor((max1-min1)/bin1)+1l
n2 =  floor((max2-min2)/bin2)+1l
n3 =  floor((max3-min3)/bin3)+1l

IF bin1 LE 0 OR bin2 LE 0 OR bin3 LE 0 THEN BEGIN 
  Message,'Illegal bin size, must be > 0',/cont
  return,-1
END

IF min1 EQ 0 AND min2 EQ 0 AND min3 EQ 0 AND $
   bin1 EQ 1l AND bin2 EQ 1l AND bin3 EQ 1l AND $
   max1 LE n1 AND max2 LE n2 AND max3 LE n3 AND $
   m1 GE 0 AND m2 GE 0 AND m3 GE 0 THEN $
  h = n1*n2*long(x3) + n1*long(x2) + long(x1) $
ELSE IF bin1 EQ 1l AND bin2 EQ 1l AND bin3 EQ 1l THEN $
 h = n1*n2*long((x3 < max3)-min3> 0l) + $
        n1*long((x2 < max2)-min2> 0l) + $
           long((x1 < max1)-min1> 0l)   $
ELSE $
 h = n1*n2*long(((x3 < max3)-min3> 0l)/bin3 ) + $
        n1*long(((x2 < max2)-min2> 0l)/bin2 ) + $
           long(((x1 < max1)-min1> 0l)/bin1 )   

h =  histogram(h,min=0,max=n1*n2*n3-1)
RETURN,reform(h,n1,n2,n3,/overwrite)
END





