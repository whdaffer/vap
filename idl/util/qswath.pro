;+
; NAME:  
; $Id$
; PURPOSE:  
;
; AUTHOR:
;
; CATEGORY:  
;
; CALLING SEQUENCE:  
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.1.1.1  2001/11/30 23:57:08  vapuser
; Initial Checkin
;
;
;Jet Propulsion Laboratory
;Copyright (c) 2001, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1407 is acknowledged.
;-

FUNCTION qswath,nodes, times, cols, rowtime=rowtime
  ;
  ;  compute beg, 'middle' and end columns of the Qscat 25km wvc grid
  ;  and return their lat/lon
  ;  This routine is based on gridplsws.pro from Scott Dunbar.
  ;
  rcsid = "$Id$"

  lonlat = !values.F_Nan
  IF n_params() LT 1 THEN BEGIN 
    Message,'Lonlat=qswath(longitude_of_ascending_node [,times, cols, rowtime=rowtime])',/cont
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

  nnodes =  n_elements(nodes)

  IF n_elements(cols) EQ 0 THEN BEGIN 
    x =  indgen(76)
  ENDIF ELSE BEGIN 
    x = cols
  ENDELSE 
  nx = n_elements(x)
  lonlat =  fltarr(nx,  ni, 2, nnodes )
  IF n_elements(times) NE 0 THEN rowtime =  dblarr(ni, nnodes)

  FOR i=0,n_elements(nodes)-1 DO BEGIN 

    ascnow = nodes[i]*!dtor - pr*lonp1


    latw = fltarr(nx,ni)
    lonw = fltarr(nx,ni)

    FOR j = 0l,nx-1 DO BEGIN
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

    period = p2/(24*60)
    IF arg_present(rowtime) THEN $
       rowtime[*,i] =  dindgen(ni)/(ni-1)*period + (times[i] - (period/4))
    bad = where( lonw LT 0, nbad )
    IF nbad NE 0 THEN lonw[bad] =  lonw[bad] + 360.


    lonlat[*,*,0,i] =  lonw
    lonlat[*,*,1,i] =  latw

    
  ENDFOR 
  IF nnodes EQ 1 THEN BEGIN 
    lonlat =  reform(lonlat,nx,ni,2)
    IF arg_present(rowtime) THEN rowtime =  reform(rowtime)
  ENDIF 

  return, lonlat

END



