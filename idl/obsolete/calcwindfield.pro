PRO calcwindfield

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




