;+
; NAME:  
; $Id$
; PURPOSE:  
;
;
; AUTHOR:
;
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  
;
;
; 
; INPUTS:  
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
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;
;
; SIDE EFFECTS:  
;
;
;
; RESTRICTIONS:  
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
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION qswathextent,node
  ;
  ;  compute beg, 'middle' and end columns of the Qscat 25km wvc grid
  ;  and return their lat/lon
  ;  This routine is based on gridplsws.pro from Scott Dunbar.
  ;
  rcsid = "$Id$"

  lonlat = !values.F_Nan
  IF n_params() NE 1 THEN BEGIN 
    Message,'Lonlat=qswathextent(longitude_of_ascending_node)',/cont
    return,lonlat
  ENDIF 
    
  ae = 6378.137	      ; Earth radius
  inc = 98.616*!dtor  ; Inclination
  cosi = cos(inc)     
  sini = sin(inc)

  P2 = 101.92	; orbit period (min)
  P1 = 1440.	; 1 mean solar day (min)
  pr = P2/P1

  ni = 1624     ; Number of Rows
  nj = 76       ; Number of columns
  res = 25.
  ;RGI = 200.

  res_i = (360./ni)*!dtor ; Angular sepration between rows
  res_j = res/ae          ; Angular seperation between cols

  ;del = RGI/ae + 0.25*res_j

  ; Set up WVC grid

  wvc_i = indgen(ni) + 1
  lonp1 =  (findgen(ni) + 0.5)*res_i - 90.*!dtor
  latp1 = -(findgen(nj)-(nj/2-0.5))*res_j

  ; for j = 0,nj-1 do latp1(j) = -(j-(nj/2 - 0.5))*res_j


  latw = fltarr(3,ni)
  lonw = fltarr(3,ni)


  ascnow = node*!dtor - pr*lonp1
  x = [0,76/2,75]
  FOR j = 0,2 DO BEGIN
    jj = x[j]
    latw[j,*] = asin($
                     cosi*sin(latp1[jj]) + $
                     sini*cos(latp1[jj])*sin(lonp1) $
                    )
    lonw[j,*] = atan( $
                     (cosi*sin(lonp1) - sini*tan(latp1[jj])), $
                     cos(lonp1) $
                    ) + ascnow
  ENDFOR
  
  lonw = lonw/!dtor
  latw = latw/!dtor

  bad = where( lonw LT 0, nbad )
  IF nbad NE 0 THEN lonw[bad] =  lonw[bad] + 360.
  

  lonlat = [ [[lonw]],[[latw]] ] ; a 3 by nrows by 2 array.
  return, lonlat

END



