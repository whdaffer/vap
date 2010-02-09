
;+
;
; Implements a version of objective analysis as described in 
;
;  "Low-Level Flows of the GATE area during Summer 1972"
;   Gregory j. Tripoli and T.N.Krishnamurti
;   Dept of Meteorology, Florida State University, Tallahassee Fla
;   
;  Monthly Weather Review, Volume 103
;  pp 197-216 
;  March 1975.
; 
; @param
; @keyword help {in}{type=boolean}{default=false}
;    Print a usage message and exit
; @keyword verbose {in}{type=boolean}{default=false}
;    Increase the number of messages emitted (not implemented as yet)
; @returns
; @examples
;
;-

FUNCTION objanl,u,v,lon,lat,lonpar,latpar,rainf,ermax,ug,vg,$
                time=time, $
                time0=time0, $ 
                trainf=trainf, $
                help=help,$
                verbose=verbose



   COMPILE_OPT IDL2, LOGICAL_PREDICATE
   catch, error
   IF error ne 0 THEN BEGIN
     catch,/cancel
     Message,!error_state.msg,/cont,/noname
     return,0
   ENDIF  
   dollar0=dollar0()
   
   IF n_elements(trainf) EQ 0 THEN trainf = 1.0
   help = keyword_set(help)
   IF help THEN BEGIN 
     Message,usage,/info
     return,0
   ENDIF 
   verbose=keyword_set(verbose)

   ;; Mark those points that are
   ;; outside of the largest radious 

   lon0=lonpar[0]
   lon1=lonpar[1]
   loninc=lonpar[2]
   lat0=latpar[0]
   lat1=latpar[1]
   latinc=latpar[2]

   nlon = (lon1-lon0)/loninc+1
   nlat = (lat1-lat0)/latinc+1

   ;; output array
   IF n_elements(ug) EQ 0 THEN BEGIN 
     num = (ug = fltarr(nlon,nlat))
     one = replicate(1,nlon,nlat)
     loni = (long(lon-lon0)/loninc) MOD 360
     lati = long(lat-lat0)/latinc
     
     ug[loni,lati] +=  u[loni,lati]
     num[loni,lati] +=  1
     ug /= (num> 1)
     xx = where(num EQ 0,nxx)
     IF nxx NE 0 THEN ug[xx] =  mean(u)
   ENDIF ELSE BEGIN 
     dim = size(ug,/dim)
     IF dim[0] NE nlon || dim[1] NE nlat THEN BEGIN 
       print,'Input UG array dimensions = ',dim
       print,'Required dimensions = ',nlon,nlat
       Message,'Dimension mismatch for input UG array'
     ENDIF 
   ENDELSE 

   IF n_elements(vg) EQ 0 THEN BEGIN 
     num = (vg = fltarr(nlon,nlat))
     one = replicate(1,nlon,nlat)
     loni = (long(lon-lon0)/loninc) MOD 360
     lati = long(lat-lat0)/latinc
     vg[*] =  mean(v)
     vg[loni,lati] +=  v[loni,lati]
     num[loni,lati] +=  1
     vg /= (num> 1)
     xx = where(num EQ 0,nxx)
     IF nxx NE 0 THEN vg[xx] =  mean(v)

   ENDIF ELSE BEGIN 
     dim = size(vg,/dim)
     IF dim[0] NE nlon || dim[1] NE nlat THEN BEGIN 
       print,'Input VG array dimensions = ',dim
       print,'Required dimensions = ',nlon,nlat
       Message,'Dimension mismatch for input VG array'
     ENDIF 
   ENDELSE 

   glon = findgen(nlon)*loninc+lon0
   glat = findgen(nlat)*latinc+lat0

   dout = rainf[0]*loninc

   ;; Do a preliminary cut, eliminate those pooints that are outside
   ;; rainf[0] of the grid.

   bad=where(lon LT lon0-dout OR $
             lon GT lon1+dout OR $
             lat LT lat0-dout OR $
             lat GT lat1+dout,nbad, comp=good,ncomp=ngood)

   IF ngood EQ 0 THEN $
    Message,'No points inside expanded grid! Aborting'

   uu = u[good]
   vv = v[good]
   llon = lon[good]
   llat = lat[good]

   IF n_elements(time) NE 0 THEN ttime = time[good]
   
   udev = uu*0+!values.f_nan
   vdev = vv*0+!values.f_nan

   ;; divide measurements into those inside the grid and those
   ;; outside. Apply Cressman weighting to the latter.

   cressmanPts = where(llon LT  lon0 OR  $
                       llon GT lon1 OR $
                       llat LT lat0 OR $
                       llat GE lat1, $
                       comp=inGrid,ncomp=ninGrid, $
                       nCressmanPts)

   tw1 = -alog(0.5)/trainf^2 ;; time weighting, if it gets uses
   
   npasses=N_ELEMENTS(rainf) 
   FOR  p=0,npasses-1 DO BEGIN
     t0 = systime(1)
     r=rainf[p]
     r2=r^2
     cgmax =  0.6*r

     ;; accumulation arrays
     a = fltarr(nlon*nlat,3)
     sumx = (sumy = (store = fltarr(nlon*nlat)))

     ;; for each point _inside_ the grid of 'stations', interpolate the
     ;; guess field to the measurement location and take the
     ;; difference. Store that difference for later use

     uu2 = uu[inGrid]
     vv2 = vv[inGrid]
     llon2 = llon[inGrid]
     llat2 = llat[inGrid]

     llon2 = (llon2-lon0)/loninc
     llat2 = (llat2-lat0)/latinc
     ugi = interpolate(ug,llon2,llat2)
     vgi = interpolate(vg,llon2,llat2)

     udev[inGrid] = uu2-ugi
     vdev[inGrid] = vv2-vgi

     uu2 = (vv2 = (llon2 = (llat2 = (ugi = (vgi = 0)))))
     ;; for each point _outside_ the grid of 'stations', find that
     ;; stations that are within the radius-of-influence of this point
     ;; and form the Cressman weighted average of those points

     uu2 = uu[CressmanPts]
     vv2 = vv[CressmanPts]
     lloni = fix((llon[CressmanPts]-lon0)/loninc)
     llati = fix((llat[CressmanPts]-lat0)/latinc)
     latcor = cos(llat[CressmanPts]*!dtor)
     a1 = (a2 = (a3 = 0.0))
     FOR vec=0,nCressmanPts-1 DO BEGIN 
       i1 = 0> (lloni[vec]-fix(r)-1)
       i2 = (lloni[vec]+fix(r)+1) < (nlon-1)
       j1 = 0> (llati[vec]-fix(r)-1)
       j2 = (llati[vec]+fix(r)+1) < (nlat-1)
       ni = i2-i1+1
       nj = j2-j1+1
       ii = findgen(ni)+i1
       jj = findgen(nj)+j1
       xdist = rebin(abs(lloni[vec]-ii)*latcor[vec],ni,nj)
       ydist = transpose(rebin(abs(llati[vec]-jj),nj,ni))
       dist2 = xdist^2 + ydist^2 
       p1 = r2-dist2
       p2 = r2+dist2
       w = reform(p1/p2,ni*nj)
       kk = jj*nlon+ii
       a1 +=  total(w*ug[kk])
       a2 +=  total(w*vg[kk])
       a3 +=  total(w)

       udev[CressmanPts[vec]] =  uu2[vec]-a1/a3
       vdev[CressmanPts[vec]] =  vv2[vec]-a2/a3
       
     ENDFOR 
     
     uu2 = (vv2 = (llon2 = (llat2 = (ugi = (vgi = 0)))))


     lloni = fix((llon-lon0)/loninc)
     llati = fix((llat-lat0)/latinc)
     latcor = cos(llat*!dtor)

     i1 = 0> (fix(lloni)-fix(r)-1)
     i2 =    (fix(lloni)+fix(r)+1) < (nlon-1)

     j1 = 0> (fix(llati)-fix(r)-1)
     j2 =    (fix(llati)+fix(r)+1) < (nlat-1)


     ;; A note in objanl.f says to increate X direction using
     ;; i1= 0> (fix(lon)-fix(r/latcor)-1)
     ;; i2=    (fix(lon)+fix(r/latcor)+1)<(nlon-1)
     ;; might have to do this too.

     FOR vec=0,n_elements(uu)-1 DO BEGIN 
       ni = i2[vec]-i1[vec]+1
       nj = j2[vec]-j1[vec]+1
       ii = rebin(findgen(ni)+i1[vec],ni,nj)
       jj = transpose(rebin(findgen(nj)+j1[vec],nj,ni))
       xdist = abs(llon[vec]-ii)*latcor
       ydist = abs(llat[vec]-jj)
       dist2 = xdist^2 + ydist^2
       weight = make_array(/float,value=1.0,dim=size(dist2,/dim))
       p1byp2 = (r2-dist2)/(r2+dist2)
       good = where(abs(r2+dist2) GT 1.e5,ngood)
       IF n_elements(time) NE 0 THEN BEGIN 
         weight =  ttime*gamma
       ENDIF 
       IF ngood NE 0 THEN weight[good] *=  p1byp2

       
       kk = nlon*jj+ii
       ;; weight is a ni by nj array
       a[kk,0] +=  udev[vec]*weight
       a[kk,1] +=  vdev[vec]*weight
       a[kk,2] +=  weight
       store[kk] +=  replicate(1l,ni,nj)
       sumx[kk] +=  sqrt(xdist)
       sumy[kk] +=  sqrt(ydist)
     ENDFOR

     xc = sumx/(store> 1)
     yc = sumy/(store> 1)
     
     
     dist = sqrt(xc^2+yc^2)
     p1byp2 = (r-dist)/(r+dist)

     ug +=  a[*,0]*p1byp2/a[*,2]
     vg +=  a[*,1]*p1byp2/a[*,2]

     print,'Time to do pass ',p, systime(1)-t0, ' seconds'
   ENDFOR ;; Loop over Radii of Influence (rainf[p])
   return,1
 END


;+
;@file_comments
; <pre>
; NAME:  
; $Id$
; PURPOSE:  
;
; AUTHOR:  William H. Daffer
;          818-354-4007
;          William.H.Daffer@jpl.nasa.gov
;
; </pre>
;-

;
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.2  2010/01/21 18:06:42  whdaffer
; continuing work
;
; Revision 1.1  2010/01/19 23:00:23  whdaffer
; stuff
;
; Revision 1.6  2009/09/16 21:23:43  whdaffer
; Hoping CVS won't expand the Log message
;
   ;; Copyright 2010, by the California Institute of
; Technology. ALL RIGHTS RESERVED. United States Government
; Sponsorship acknowledged. Any commercial use must be
; negotiated with the Office of Technology Transfer at the
; California Institute of Technology. 
; 
; This software may be subject to U.S. export control
; laws. By accepting this software, the user agrees to
; comply with all applicable U.S. export laws and
; regulations. User has the responsibility to obtain export
; licenses, or other export authority as may be required
; before exporting such information to foreign countries or
; providing access to foreign persons. 
;
; Last Modified :
;
