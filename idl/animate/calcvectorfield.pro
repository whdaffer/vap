;+
; NAME: CalcVectorField
; $Id$
; PURPOSE:  Calculate the vector field using quantities held in
;          common. Trust me, you don't want to call this routine. It's
;          use almost exclusively by animate_wind_field.pro
;
;
; AUTHOR:  Mike Spencer (updated by William Daffer)
;
; CATEGORY:  Vap Animation
;
; CALLING SEQUENCE: All quantities are held in common. The routine is
;                   called with no arguments. (I know, I know, it's
;                   horrible, but it's been this way for 2 year and it
;                   isn't going to change right now)
; 
; INPUTS:  None
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  Quantities are change in the common
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:   PRS. I ain't going to explain it now.
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
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
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

; Given the current postions of vector locations this routine
; calculates the value of the U/V components at that point. That is,
; it calculates the wind field at the input vector locations.
;
; Variables are passed via the common block 'prs'. These variables
; are:
;
; long_sel   - Longitude of each vector position field
; lats_sel   - Latitude of of each vector position field
; uu_sel     - U value at vector postion given in long/lat_sel (output)
; vv_sel     - V value at vector postion given in long/lat_sel (output)
;
;    NB, uu_sel(i) and vv_sel(i) has location = [Long_sel(i),lat_sel(i)]
;  
; lons       - Longitude of interpolated wind field
; lats       - Latitude of interpolated wind field
; uu         - Interpolated wind field U componenet
; vv         - Interpolated wind field V componenet
;
;    NB, uu(i) and vv(i) has location [lons(i),lats(i)]
;
; ileft      - index of interpolated fields grid point to the 
;              left of long/lat_sel(i)
; iright     - index of interpolated field's grid point to the 
;              right of long/lat_sel(i)
; itop       - ditto, except top
; ibot       - ditto, except bottom
; dist_left  - distance of long/lat_sel(i) from grid point to left
; dist_right - ditto, except right
; dist_top   - ditto, except top
; dist_bot   - ditto, except bottom
; dist       - array containing all the distances
; weights    - weights
; invdist    -
; xfinc      - info about the interpolated field file structure
; xf0        - "      "    "      "        "    "     "
; yfinc      - "      "    "      "        "    "     "
; yf0        - "      "    "      "        "    "     "
; eps        - a small number.
; 
PRO calcvectorfield


COMMON prs, long_sel, lats_sel, lons, lats, uu, vv, uu_sel, vv_sel, $
           ileft, iright, itop, ibot, dist_left, dist_right, dist_top, $
           dist_bot, dist, weights, invdist, xfinc, xf0, yfinc, yf0, eps



ileft  = fix( (long_sel - xf0)/xfinc )
iright = ileft+1
ibot   = fix( ( lats_sel - yf0)/yfinc )
itop   = ibot+1
;
dist_left  = abs(  long_sel     - (ileft*xfinc + xf0)    )+eps
dist_right = abs(  iright*xfinc + xf0         - long_sel )+eps
dist_bot   = abs(  lats_sel     - (ibot*yfinc  + yf0)    )+eps
dist_top   = abs(  itop*yfinc   + yf0         - lats_sel )+eps
;
dist( *,0 ) = sqrt( dist_left^2  + dist_bot^2 )
dist( *,1 ) = sqrt( dist_left^2  + dist_top^2 )
dist( *,2 ) = sqrt( dist_right^2 + dist_top^2 )
dist( *,3 ) = sqrt( dist_right^2 + dist_bot^2 )
;

invdist =  1./dist^2
wtot =  invdist(*,0) + invdist(*,1) + invdist(*,2) + invdist(*,3)

FOR i=0,3 DO weights(*,i)  =  (invdist(*,i))/wtot

uu_sel = ( weights(*,0) * uu(  ileft,  ibot ) + $
           weights(*,1) * uu(  ileft,  itop ) + $
           weights(*,2) * uu(  iright, itop ) + $
           weights(*,3) * uu(  iright, ibot )  )

vv_sel = ( weights(*,0) * vv(  ileft,  ibot ) + $
           weights(*,1) * vv(  ileft,  itop ) + $
           weights(*,2) * vv(  iright, itop ) + $
           weights(*,3) * vv(  iright, ibot ) )


return
END
