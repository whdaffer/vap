;+
; NAME:  findbreaks.pro
; $Id$
; PURPOSE:  Find the place where the difference between succeeding
;           elements of an where vector violates some max difference
;           criterion.
;
;           The assumption here is that you've done something like the
; following. Say you have an vector of some quantity with sentinel
; values representing missing data. Sea Surface Height along Topex
; track is an example, where missing data occurs when the satellite
; crosses over land. You want only those segments that have breaks in
; them of less than 10 points, for instance. You might do something
; like this
;
;    good=where( ssh ne missing_data_sentinel_value, ngood )
;    breaks = findbreaks(good,10)
;    
;   Now, the segments are:
;
;   ssh[ good[breaks[0,i]:breaks[1,i]] ] for i=0,nbreaks-1
;
;
;
; AUTHOR:   William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: breaks=findbreaks( where_vector, test )
; 
; INPUTS:  
;
;  Array: The array to be tested
;  test: the Maximum difference allowed before a 'break' is declared 
;
; OPTIONAL INPUTS:  none
;       
; KEYWORD PARAMETERS:  none
;
; OUTPUTS: 
;
;  Success: 
;
;   breaks: A 2d array. The vector breaks[*,0] the beginning of each
;          segment; the vector breaks[1,*] the end of each segment. 
;          The 'i-th' (i=0,1,...) segment of the data is
;          given by array(breaks[0,i]:breaks[1,i]) 
;
;  If the array has no breaks the return is the two element array 
;
;       return,[0,n_elements(array)-1]
;
;
;  Error: Scalar -1.
;
; OPTIONAL OUTPUTS: none 
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
; Revision 1.1  1999/04/07 22:16:41  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-
; No Warranties!
;
FUNCTION findbreaks, array, test


  breaks = -1
  IF n_params() LT 1 THEN BEGIN 
    Message,'Usage: breaks=findbreaks(array [,test])',/cont
    return,breaks
  ENDIF

  catch, Error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,-1
  ENDIF 

  IF n_elements(test) EQ 0 THEN test = 1
  nn = n_elements(array)
  diff = diffarr(array)
  IF finite(diff[0]) THEN BEGIN 
    x = where( diff GT test, nx )
    IF nx NE 0 THEN BEGIN 
      breaks = [0,x, x+1,nn-1]
      breaks = reform(breaks(sort(breaks)),2,nx+1)
    ENDIF ELSE breaks=[0,nn-1]
  ENDIF 

  return, breaks
END
