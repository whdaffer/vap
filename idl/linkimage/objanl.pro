
;+
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
  lan1=lonpar[1]
  loninc=lonpar[2]
  lat0=latpar[0]
  lan1=latpar[1]
  latinc=latpar[2]

  nlon = (lon1-lon0)/loninc
  nlat = (lat1-lat0)/latinc+1
  dout = rainf[0]*loninc

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

  cressmanPts = where(llon LT  lon0 OR  $
                      llon GT lon1 OR $
                      llat LT lat0 OR $
                      llat GE lat1, $
                      comp=inGrid,ncomp=ninGrid, $
                      ncressmanPts)

  tw1 = -log(0.5)/trainf^2;; time weighting
            
  npasses=N_ELEMENTS(rainf) 
  glon = findgen(nlon)*loninc+lon0
  glat = findgen(nlat)*latinc+lat0
  FOR  p=0,npasses-1DO BEGIN
    r=rainf[r])
    r2=r^2

    ;; for each point _inside_ the grid of 'stations' Interpolate the
    ;; guess field to this location and take the difference. Store
    ;; that difference for later use

    uu2 = uu[inGrid]
    vv2 = vv[inGrid]
    llon2 = llon[inGrid]
    llat2 = llat[inGrid]

    llon2 = (llon2-lon0)/loninc
    llat2 = (llat2-lat0)/latinc
    ugi = interpolate(ug,llon2,llat2)
    vgi = interpolate(vg,llon2,llat2)

    udev = uu2-ugi
    vdev = vv2-vgi

    ;; for each point _outside_ the grid of 'stations' Interpolate the
    ;; guess field to this location and take the difference using
    ;; Cressman weighting. Store that difference for later use

    uu2 = uu[CressmanPts]
    vv2 = vv[CressmanPts]
    llon2 = llon[CressmanPts]
    llat2 = llat[CressmanPts]

    llon2 = (llon2-lon0)/loninc
    llat2 = (llat2-lat0)/latinc
    ugi = interpolate(ug,llon2,llat2)
    vgi = interpolate(vg,llon2,llat2)

    udev = uu2-ugi
    vdev = vv2-vgi


    FOR v=0,nCressmanPts DO BEGIN
    ENDFOR
  ENDFOR

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
