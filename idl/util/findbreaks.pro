;+
; NAME:  findbreaks.pro
; $Id$
; PURPOSE:  Find the place where an the difference between succeeding
;           elements of an array violates some min max difference
;           criterion.
;
; AUTHOR:   William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: breaks=findbreaks( array, test )
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
;  Success: breaks: the vector containing the beginning of each
;          segment. The 'i-th' (i=0,1,...) segment of the data is
;          given by array(breaks[i]:breaks[i+1]-1) except for the last
;          segment, which consists of 
;          array(breaks[j]:n_elements(array)-1) 
;          where j=n_elements(breaks)-1
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
;
;Copyright (c) 1998, William Daffer
;-
; No Warranties!
;
FUNCTION findbreaks, array, test

    ; Returns the starting points of each of the segments of an array,
    ; segments being defined as those where the difference between
    ; successive elements of the array is less than 'test', defaulting
    ; to '1'. This is most useful, at least for me, when the array I'm
    ; testing is an array of indices, such as those obtained using
    ; 'where.' In this case, I'm trying to segment the array used in
    ; the 'where' into regions which satify a particular condition. As
    ; an example, consider an array of latitudes. One might want to
    ; segment it into those elements within one degree of the equator,
    ; but still want to know where the first such segment begins (the
    ; ascending node of a satellite's orbit, for instance) and where
    ; the second segment begins (the descending node).

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
      breaks = [0, x+1, nn]
    ENDIF ELSE breaks=[0,nn]
  ENDIF 

  return, breaks
END
