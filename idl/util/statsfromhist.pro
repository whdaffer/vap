;+
; NAME:  StatsFromHist.pro
; $Id$
; PURPOSE:  Calculate Mean and Variance from 1d  histogram. 
;           Alternately, calcuate the mode or median.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Statistics
;
; CALLING SEQUENCE:  stats=statsFromHist(histogram [,xarray $
;                                                    /mode |, /median])
; 
; INPUTS:  histogram - a 1d histogram
;
; OPTIONAL INPUTS:  xarray - Array of ordinates for histogram.
;                            If not present, histogram is assumed to
;                            go from 0,n_elements(histogram)-1
;	
; KEYWORD PARAMETERS:  
;
;     Mode: flag, calculate the mode instead
;     Median: flag, calculate the median instead
;
; OUTPUTS:   
;   Success: [ mean, variance] 
;            mode if /mode 
;            median if /median
;
;   Error:   !values.f_nan
;            
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
; Revision 1.3  1999/03/29 17:18:02  vapuser
; Added some argument testing
;
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

FUNCTION statsfromhist, hist, x, mode=mode, median=median, mean=mean

  ; Caluclate the mean and variance from the 1d histogram 'hist'. Or,
  ; calculate the mode or median, if the correct keywords are set. If
  ; the ordinates aren't provided in the parameter 'x', the array is
  ; assumed to run from 0 to n_elements(hist)-1.

  ; If successful, returns [mean,variance] if no other specification
  ; is made. Returns mode, median or just the mean, if those keywords
  ; is set. Returns !values.f_nan if there's some error.


  retvals = !values.f_nan
  IF n_params() LT 1 THEN BEGIN 
    Usage,"stats=statsfromhist(histogram [,xarray])"
    return,retvals
  ENDIF 

  IF NOT exist(hist) THEN BEGIN 
    Message,'Input parameter HIST is required',/cont
    return,retvals
  ENDIF 

  IF NOT exist(x) THEN x = lindgen(n_elements(hist))
  
  nn = total(hist)
  IF nn NE 0 THEN BEGIN 
    
    CASE 1 OF 
      keyword_set(mode): BEGIN 
        t = max(hist, ii)
        retvals = x[ii]
      END 
      keyword_set(median): BEGIN 
        tt = where( hist NE 0, ntt )
        IF  ntt EQ 1 THEN BEGIN 
          median = x[tt]
        ENDIF ELSE BEGIN 
          xx = x[tt]
          hh = hist[tt]
          s = sort(xx)
          xx = xx[s]
          h = hh[s]
          IF nn MOD 2 EQ 0 THEN $
            mididx = nn/2 ELSE $
            mididx = nn/2+1
          hsum = h*0
          hsum[0] = h[0]
          i = 0
          WHILE hsum[i] LT mididx DO BEGIN 
            i = i+1
            hsum[i] = hsum[i-1] + h[i]
          ENDWHILE 
          retvals = xx[i-1]
        ENDELSE 
      END 
      ELSE: BEGIN 
        m = total(x*hist)/(nn > 1)
        IF NOT keyword_set(mean) THEN BEGIN 
          variance =  (total(x^2*hist) - nn*m^2 )/( (nn-1)> 1 )
          retvals = [m,variance]
        ENDIF ELSE retvals = m
      END
    ENDCASE 

  ENDIF ELSE BEGIN 
    IF NOT (keyword_set(mode) OR $
            keyword_set(median) OR $
            keyword_set(mean)) THEN retvals = [0,-1] ELSE retvals = 0
  ENDELSE 
  return, retvals

END
