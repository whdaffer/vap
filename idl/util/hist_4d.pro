;+
; NAME:  hist_4d.pro
; $Id$
; PURPOSE:  Create a 3 dimensional histogram
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Statistics
;
; CALLING SEQUENCE:  hist4d=hist_4d(a1,a2,a3,a4 $
;                                  [,min1=min1,max1=max1,bin1=bin1, $
;                                   ,min2=min2,max2=max2,bin2=bin2, $
;                                   ,min3=min3,max3=max3,bin3=bin3,$
;                                   ,min4=min4,max4=max4,bin4=bin4 ] ) ] )
;
; 
; INPUTS:  
;
;  A1, A2, A3, A4. Arrays with which to create the 4d histogram
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  min{1,2,3,4}: 
;                        The minimum or A{1,2,3} to be used in the
;                        histograming. Default = minimum(A{1,2,3})
;                      max{1,2,3,4}: simile Default = maximum(A{1,2,3})
;                      Bin{1,2,3,4}: simile, Default=1.
;
; OUTPUTS: 
;
;  Success: The 4d histogram
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
;
;Copyright (c) 1998,William Daffer
;-
; No Warranties!
;

FUNCTION hist_4d, x1,x2,x3,x4, $
                  min1=min1,max1=max1,bin1=bin1,$
                  min2=min2,max2=max2,bin2=bin2,$
                  min3=min3,max3=max3,bin3=bin3,$
                  min4=min4,max4=max4,bin4=bin4


IF n_Params() LT 4 THEN BEGIN 
  Usage, 'hist4d=hist_4d(a1,a2,a3,a4 [,min1=min1,max=max1,bin1=bin1,min2=min2,max=max2,bin2=bin2,min3=min3,max=max3,bin3=bin3,min4=min4,max=max4,bin4=bin4  )'
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
m4 = min(x4,max=mx4)

IF n_elements(min1) EQ 0 THEN min1 = m1
IF n_elements(min2) EQ 0 THEN min2 = m2
IF n_elements(min3) EQ 0 THEN min3 = m3
IF n_elements(min4) EQ 0 THEN min4 = m4
IF n_elements(max1) EQ 0 THEN max1 = mx1
IF n_elements(max2) EQ 0 THEN max2 = mx2
IF n_elements(max3) EQ 0 THEN max3 = mx3
IF n_elements(max4) EQ 0 THEN max4 = mx4
IF n_elements(bin1) EQ 0 THEN bin1 = 1L
IF n_elements(bin2) EQ 0 THEN bin2 = 1L
IF n_elements(bin3) EQ 0 THEN bin3 = 1L
IF n_elements(bin4) EQ 0 THEN bin4 = 1L

n1 =  floor((max1-min1)/bin1)+1l
n2 =  floor((max2-min2)/bin2)+1l
n3 =  floor((max3-min3)/bin3)+1l
n4 =  floor((max4-min4)/bin4)+1l

IF bin1 LE 0 OR bin2 LE 0 OR bin3 LE 0 OR bin4 LE 0 THEN BEGIN 
  Message,'Illegal bin size, must be > 0',/cont
  return,-1
END

IF min1 EQ 0 AND min2 EQ 0 AND min3 EQ 0 AND min4 EQ 0 AND $
   bin1 EQ 1l AND bin2 EQ 1l AND bin3 EQ 1l AND bin4 EQ 1l AND $
   max1 LE n1 AND max2 LE n2 AND max3 LE n3 AND max4 LE n4 AND $
   m1 GE 0 AND m2 GE 0 AND m3 GE 0 AND m4 GE 0 THEN $
   h = long(x1) + n1*( long(x2) + n2*(long(x3) + n3*long(x4))) $
  ;  h = n1*n2*n3*long(x4) + n1*n2*long(x3) + n1*long(x2) + long(x1) $
ELSE IF bin1 EQ 1l AND bin2 EQ 1l AND bin3 EQ 1l AND bin4 EQ 1l THEN $
h =          long((x1 < max1)-min1> 0l )  + $
         n1*(long((x2 < max2)-min2> 0l )  + $
         n2*(long((x3 < max3)-min3> 0l )  + $
         n3*(long((x4 < max4)-min4> 0l ) ) ) ) $


; h = n3*n2*n1*long((x4 < max4)-min4> 0l) + $
;        n2*n1*long((x3 < max3)-min3> 0l) + $
;           n1*long((x2 < max2)-min2> 0l) + $
;              long((x1 < max1)-min1> 0l)   $
ELSE $
h =          long(((x1 < max1)-min1> 0l)/bin1 )  + $
         n1*(long(((x2 < max2)-min2> 0l)/bin2)  + $
         n2*(long(((x3 < max3)-min3> 0l)/bin3)  + $
         n3*(long(((x4 < max4)-min4> 0l)/bin4) ) ) ) 

; h = n3*n2*n1*long(((x4 < max4)-min4> 0l)/bin4 ) + $
;        n2*n1*long(((x3 < max3)-min3> 0l)/bin3 ) + $
;           n1*long(((x2 < max2)-min2> 0l)/bin2 ) + $
;              long(((x1 < max1)-min1> 0l)/bin1 )   


h =  histogram(h,min=0,max=n1*n2*n3*n4-1)
RETURN,reform(h,n1,n2,n3,n4,/overwrite)
END





