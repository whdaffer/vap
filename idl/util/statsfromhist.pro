;+
; NAME:  StatsFromHist.pro
; $Id$
; PURPOSE:  Calculate Mean and Variance from 1d histogram
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Statistics
;
; CALLING SEQUENCE:  stats=statsFromHist(histogram [,xarray])
; 
; INPUTS:  histogram - a 1d histogram
;
; OPTIONAL INPUTS:  xarray - Array of ordinates for histogram.
;                            If not present, histogram is assumed to
;                            go from 0,n_elements(histogram)-1
;	
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:   [ mean, variance] if successful
;            [0,-1] if not.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
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
; Revision 1.2  1999/03/29 17:10:40  vapuser
; Changed some comments
;
; Revision 1.1  1999/03/29 17:07:03  vapuser
; Initial revision
;
;
;
;Copyright (c) 1999, William Daffer
;-

FUNCTION statsfromhist, hist, x

  ; Caluclate the mean and variance from the 1d histogram
  ; 'hist'. If the ordinates aren't provided in the parameter 'x', the
  ; array is assumed to run from 0 to n_elements(hist)-1.

  ; Returns [mean,variance] or [0,-1] if error.

  retarray = [0,-1]
  IF n_params() LT 1 THEN BEGIN 
    Usage,"stats=statsfromhist(histogram [,xarray])"
    return,[0,-1]
  ENDIF 

  IF NOT exist(hist) THEN BEGIN 
    Message,'Input parameter HIST is required',/cont
    return,[0,-1]
  ENDIF 

  IF NOT exist(x) THEN x = lindgen(n_elements(hist))
  
  nn = total(hist)
  IF nn NE 0 THEN BEGIN 
    
    mean = total(x*hist)/(nn > 1)
    variance =  (total(x^2*hist) - nn*mean^2 )/( (nn-1)> 1 )
    retarray = [mean,variance]

  ENDIF ELSE $
    Message,'Zero Histogram!',/cont

  return, retarray

END
