;+
; NAME:  
; $Id$
; PURPOSE:  
;
; AUTHOR: Lay out one or more SeaWinds swaths
;
; CATEGORY:  QuikSCAT/SeaWinds utility
;
; CALLING SEQUENCE:  swath=qswath(nodes, times, cols, rowtime=rowtime)
; 
; INPUTS:  
;
;   nodes: float vector: the longitude of the ascending nodes for 
;          the ground tracks desired. 
;
;
; OPTIONAL INPUTS:  
;
;  times: (I) A vector of floats or doubles of the same shape as
;         `nodes,' the time of the equator crossings given in `nodes'
;         as 'days' from some convenient epoch. The user is completely
;         responsible for making the proper conversions of time, but
;         nothing about the swath really depends on time, this is just
;         a convenience calculation in case the user wants to have
;         some idea what time a particular row occured in the
;         orbit.
;
;         Used in calculating `rowtimes.' If absent this
;         variable is ignored.
;
;  cols: (I). A vector listing the desired columns of the swath (zero
;        indexed!). If this variable is absent the full swath of 76
;        columns is returned.
;  
;	
; KEYWORD PARAMETERS:  
;
;   rowtime: (O) The `time' of each row as calculated from the
;            variable 'times.'
;
;
; OUTPUTS:  
;
;       swath: An n_elements(cols: default=76) by 1624 by 2 by
;              n_elements(nodes) array of floats. 
;
;       swath[*,*,0,*] are the longitudes
;       swath[*,*,1,*] are the latitudes
;
; OPTIONAL OUTPUTS: none  
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  Takes a while if you request the full swath.
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
; Revision 1.1  2001/12/10 23:59:10  vapdev
; Renamed from  to
;
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



