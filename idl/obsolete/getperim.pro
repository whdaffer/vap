;+
; NAME:  GetPerim
; $Id$
; PURPOSE:  Return the perimeter of a section of swath data. useful
;          for determining overlaps of data.
;
; AUTHOR:  whd
;
; CATEGORY:  swath data utility
;
; CALLING SEQUENCE:  
;
;    [ [perim_x],[perim_y]] = getPerim(lon,lat  [,xi,yi] )
; 
; INPUTS:  
;
;   lon: The n by m array of swath data longitudes. 
;   lat; the n by m array of swath data latitudes.
;
;   This data is assumed to be arranged with n = number of columns,  
;   m = number of rows and the rows monotonically increasing in time.
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;  retval: the perimeter as a [r,2] array where r is the number of
;          rows in the input arrays that have good lon/lat data. 
;
;  retval[*,0] is the'x' value of each perimeter point and
;  retval[*,1] the 'y'. The perimeter isn't closed, so if you want it
;  to be closed just form
;
;   px = [retval[*,0], retval[0,0]] 
;   py = [retval[*,1], retval[1,0]]
;
;
;
; OPTIONAL OUTPUTS:  
;
;  xi/yi. The indices into the lon/lat arrays.
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
;  It's assumed that the lon/lat arrays are sorted so that the rows
;  increase monotically in time. This imposes an orientation on the
;  'swath' and the perimeter is return so that it starts on the 'lower
;  left' and returns to the lower right.
;
;  It is also assumed that all points where both lat and lon are small
;  (1.e-7 or smaller) are invalid data, these are ignored.
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION getperim, lon, lat, xi, yi

  ddims = size(lon,/dim)
  good = where( abs(lon) GT 1.e-7 AND abs(lat) GE 1.e-7,ngood)
  unpack_where,lon,good,goodc,goodr

  retval = 0
  first = 1

  FOR rr=0,ddims[1]-1 DO BEGIN 
    rx = where(goodr EQ rr,nrx)
    IF nrx GE  2 THEN BEGIN
      tt =  minmax(goodc[rx])
      IF tt[1] - tt[0] GE 2 THEN BEGIN 
        IF first THEN BEGIN 
          xi = tt
          yi = [rr,rr]
          first = 0
        ENDIF ELSE BEGIN 
          xi = [xi,tt]
          yi = [yi,[rr,rr]]
        ENDELSE 
      ENDIF 
    ENDIF 
  ENDFOR 

  
  nxi = n_elements(xi)
  xi = reform(xi,2,nxi/2)
  yi = reform(yi,2,nxi/2)
  ddims = size(xi,/dim)
  
  IF ddims[1] GE 3 THEN BEGIN 


    xi=[ reform(xi[0,*]),reverse(reform(xi[1,*]))] 

    yi=[ reform(yi[0,*]),reverse(reform(yi[1,*]))]  
    px =  lon[[xi],[yi]]
    py =  lat[[xi],[yi]]
    retval = [ [px], [py] ]
  ENDIF 

  return, retval
end
