;+
; NAME:  leveler.pro
; $Id$
; PURPOSE:  Given an array, creates a vector with values suitable for
;          use as contour levels in contouring data. Allows for
;          linear or, what I've taken to calling a 'geometric'
;          progressions of the contours. The 'geometric' is symetrical
;          about the 'center' which is either input or calculated from the
;          range of the inarr. And samples the center of the range of
;          'inarr' more than the extremities. I suggest you try it to see
;           what it does. I put it together to meet some long
;          forgotten need.
;
;          Note, this routine is *not* very well tested, use at your
;          own risk.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Contouring Utility
;
; CALLING SEQUENCE:  levels=leveler( inarr, n [,center=center, $
;                                     minmax=minmax, base=base, $
;                                    /linear|,/geometric ] )
; 
; INPUTS:  
;
;  inarr: the array you want to get the levels for 
;
; OPTIONAL INPUTS: 
;  n:     the number of levels
;
; KEYWORD PARAMETERS:  
;
;    base: The base of the geometric progression (default=0.5)
;    center: the center of symetry for the geometric case.
;    minmax: the min/max to be used in the caluclations. Default =
;            min/max of inarr.
;    linear: divide the interval up into 'n' levels spaced linearly.
;    geometric: divide the interval [min, center] geometrically, then
;               divide the intercal [center,max] geometrically, but
;               with the progression the reverse of the [min,center]
;               interval. The levels are most closely spaced around
;               the 'center'
; OUTPUTS:  
;
;  success: The desired levels.
;  error: !values.f_nan
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  none
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
; PROCEDURE:  none
; EXAMPLE:  levels=leveler(findgen(100),10,/geom)
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION leveler, inarr, n, center=center, $
                  minmax=minmax, $
                  linear=linear, $
                  geometric=geometric, base=base


errval = !values.f_nan

ON_ERROR,0

IF n_params() LT 1 THEN BEGIN 
  Usage, "levels=leveler(inarr [,n, center=center, minmax=minmax, base=base, /linear|/geometric)"
  return, errval
END

catch, error
IF error NE 0 THEN BEGIN 
  Message,!error_state.msg,/cont
  return,errval
ENDIF 


linear =  keyword_set( linear )
geometric = keyword_set( geometric )
IF NOT linear AND NOT geometric THEN linear = 1


IF linear AND keyword_set(base) THEN BEGIN 
 str =  " Keyword 'BASE' only useful with keyword " + $
 "'GOEMETRIC', keyword ignored"
  message,str,/info
ENDIF 

IF NOT exist(base) THEN base = 1./2.

sz = size( inarr )

IF sz(1) EQ 1 AND sz(2) EQ 1 THEN BEGIN 
   rr = inarr 
ENDIF ELSE BEGIN 
  IF keyword_set(minmax) THEN BEGIN 
    rr =  minmax(inarr )    
  ENDIF ELSE BEGIN 
    m = median(inarr)
    absdev = total( abs(inarr-m) )/n_elements(inarr)
    rr = min(inarr) >  (([-1,1]*2*1.6*absdev)+m) < max(inarr)
  ENDELSE 

ENDELSE 

range = rr(1)-rr(0)

IF NOT exist(center) THEN center =  rr(0) + range/2.
IF NOT exist( n ) THEN n =  5

IF n Le 1 THEN $
  message,'Parameter N must be > 2'

CASE 1 OF 
  linear: BEGIN 
    x = findgen( n )/(n-1)
    levels = x*range+rr(0)
  END 
  geometric: BEGIN 

    IF n LT 4 THEN $
      message,' When using geometric levels, N must be >= 4'

    IF (n MOD 2) EQ 0 THEN BEGIN 
      str =  " parameter N must be odd, " + $
       " it will be set to n+1 "
      message,str,/cont
      n = n+1
    ENDIF 

    
    n1 = n/2
    t1 = geomprog( [rr(0),center], n1, base=base)
    t2 =  rr(1) + center -reverse( geomprog( [center, rr(1) ], n/2, base=base) )
    levels = [t1,center,t2]
  end
ENDCASE 
return, levels
END


