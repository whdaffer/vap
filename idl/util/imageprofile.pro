;+
; NAME:  ImageProfile
; $Id$
; PURPOSE:  Calculate a profile along line defined by start/end poiints.
;
; AUTHOR:  William Daffer (with nods of theivery to Dave Fanning
;
; CATEGORY:  Image analysis
;
; CALLING SEQUENCE:  profileArr=imageProfile(image [,$
;                               startPt[,EndPt]])
; 
; 
; INPUTS:  
;  Image: 2-d image
;
; OPTIONAL INPUTS: 
;
;  StartPt: 2-vector. Col/Row at which to start profile (defaults to [0,0])
;  EndPt:  2-vector. Col/Row at which to end profile (defaults to
;         [nx-1, ny-1] where nx,ny are the size of the array in the
;         x/y direction, respectively.)
;
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  profileArr: a n-by-3 array, where xi=profileArr[*,0],
;          yi=profileArr[*,1] and zi=profileArr[*,2]. zi is tha actual
;          profile. The other two variables are the points at whic
;          the interpolation occurs. Not, if x0=x1 (where
;          x0=startPt[0] and x1=EndPt[0] then the returned profile is
;          just that column of the image array. Similarly for the case
;          that y0=y1)
;
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  None
;
; PROCEDURE:  if StartPt[0] eq EndPt[0] then return the appropriate
;            column. If StartPt[1] eq EndPt[1] then return the
;          appropriate row. Else, construct xi and yi and interpolate.
;
; EXAMPLE:  
;
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1998/11/17 18:01:44  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION imageProfile, image, startPt, endPt
  IF n_params() LT 1 THEN BEGIN 
    Message,'Usage: [ [x],[y],[z]] = imageProfile(image [,startPt [,EndPt ] ] )',/cont
    return,''
  ENDIF 
  ndims = size(image,/n_dim)
  sz = size(image,/dimensions)
  IF ndims NE 2 THEN BEGIN 
    Message,'Image must be 2-d!',/cont
    return,''
  ENDIF 

  IF n_Elements(startPt) NE 2 THEN startPt = [0,0]
  IF N_elements(endPt) NE 2 THEN endPt =  [ sz[0], sz[1] ]-1

  x0 = startPt[0]
  y0 =startPt[1]
  x1 = endPt[0]
  y1 = endPt[1]

  CASE 1 OF 
    x0 EQ x1 : BEGIN 
        ; Column Profile 
      zi = reform(image[x0,*])
      xi = replicate(x0,n_elements(zi))
      yi = indgen(n_elements(zi))+y0
    END
    y0 EQ y1: BEGIN 
        ; Row Profile 
      zi = image[*,y0]
      xi = indgen(n_elements(zi))+x0
      yi = replicate(y0,n_elements(zi))
    END
    ELSE: BEGIN 
      ; Arbitrary profile
      npoints =  Abs(x1-x0+1)> Abs(y1-y0+1)
      xi = x0 + (x1 - x0) * Findgen(nPoints) / (nPoints - 1)
      yi = y0 + (y1 - y0) * Findgen(nPoints) / (nPoints - 1)
      zi = Interpolate(image,xi,yi)
    END
  ENDCASE 
  return, [ [xi], [yi], [zi] ]

END
