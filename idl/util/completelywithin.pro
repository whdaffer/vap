;+
; NAME:  CompletelyWithin
; $Id$
; PURPOSE:  Determines wether one set of lat/lon limits is inside another.
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  Map manipulation/Plotting.
;
;
;
; CALLING SEQUENCE:  
;
;    isIt = CompletelyWithin( limits1, limits2 )
;    
;
; 
; INPUTS:  
;
;   Limits1 : a 4-vector.
;             [lon0, lat0, lon1, lat1] where
;             lon0,lat0 is the lower left corner and 
;             lon1,lat1 is the upper right corner.
;
;   Limits2 : a 4-vector. 
;             The same arrangement as Limits1
;
;
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:  
;
;   isIt = 0 if the rectangle described by limits1 is not completely
;            within the rectangle described by limits2
;   isIt = 1 if it is.
;   isIt = -2 if there is some problem.
;
;
; OPTIONAL OUTPUTS:  None
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  None
;
;
;
; RESTRICTIONS:  Will only work in those mapping situations where 4
;                element limits make sense. Don't try it when you using
;                a stereographic projection of the south pole, for
;                instance.
;
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION CompletelyWithin, limits1, limits2
  IF n_params() NE 2 THEN BEGIN 
    Message,'Usage: isIt = CompletelyWithin( limits1, limits2 )',/cont
    return,-2
  ENDIF 
  IF N_Elements(limits1) NE 4 OR N_Elements(limits2) NE 4 THEN BEGIN 
    Message,'Usage: isIt = CompletelyWithin( limits1, limits2 )',/cont
    print,'Where... Limits1 and Limits2 are 4-vectors arranges as [lon0,lat0,lon1,lat1]'
    return,-2
  ENDIF 

  lonrange1 = fixlonrange( [ limits1[0], limits1[2] ] )
  lonrange2 = fixlonrange( [ limits2[0], limits2[2] ] )

  x1 = where( lonrange1 LT 0, nx1 )
  x2 = where( lonrange2 LT 0, nx2 )
  IF nx1 NE nx2 THEN BEGIN 
    IF nx1 NE 0 THEN $
      lonrange2 = fixlonrange(lonrange2,/west) ELSE $ 
      lonrange1 = fixlonrange(lonrange1,/west) 
  ENDIF 
  Return, (lonrange1[0] GE lonrange2[0] AND $
           lonrange1[1] LE lonrange2[1] AND $
           limits1[1] GE limits2[1] AND $
           limits1[3] LE limits2[3] )
END
