;+
; NAME:  hasgaps
; $Id$
; PURPOSE:  run through a list of RNOAA files, checking for gaps
;          larger than 'tolerance' while over water. 
;
; AUTHOR:  whd
;
; CATEGORY:  SeaWinds Utility
;
; CALLING SEQUENCE:  1|0 = hasgaps(wfiles $
;                   [, tolerance,$
;                   lonpar, latpar, 
;                   decimate=decimate, crdecimate=crdecimate,
;                   excludecols=excludecols, rflag=rflag, $
;                   doall=doall,loni=loni,lati=lati,mask=mask])
; 
; INPUTS:  
;
;   wfiles: vector of fully qualified file names
;   
;
; OPTIONAL INPUTS:  
;  tolerance: the maximum angular distance (in degrees) allowed
;             between any grid point and data measured in 'grid'
;             cells. (default=3)
;
;  lonpar: a float 3-vector [min,max,increment] (default=[0.,359,1])
;  latpar: a float 3-vector [min,max,increment] (default=[-60.,60,1])
;
; KEYWORD PARAMETERS:  
;
;   doall: don't stop when you find the first gap but do the whole
;          grid.
;   loni: the resultant
;
; OUTPUTS:  
;
;    1: The input set of files has gaps relative to the input grid.
;    0: The input set of files does NOT have  gaps relative to the input grid.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  Construct the grid from the input or defaulted
;            lonpar/latpar parameters. Read and concatenate all the
;            wind data into one big array. Run through the grid and
;            see if there are any locations that are more than
;            'tolerance' away from any wind data. Stop at the first
;            such occurance and return 1.
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 2000, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION hasgaps, wfiles, tolerance,lonpar, latpar, $
                  decimate=decimate, crdecimate=crdecimate,$
                  excludecols=excludecols, rflag=rflag, $
                    doall=doall, loni=loni, lati=lati, mask=mask

  orbinc = 25.258
  done = 0

  doall =  keyword_set(doall)
  test = arg_present(loni) AND arg_present(lati) AND arg_present(mask)

  IF doall AND test NE 1 THEN BEGIN 
    message,'if /doall is set, you need all 3 (loni,lati,mask) set up to return data',/info
    return,0
  ENDIF 

  nn = n_elements(wfiles)
  IF n_elements(lonpar) NE 3 THEN lonpar =  [0., 359, 1]
  IF n_elements(latpar) NE 3 THEN latpar =  [-60.,60,1]
  IF n_elements(tolerance) EQ 0 THEN tolerance = 8.5
  IF n_elements(decimate) EQ 0 THEN decimate = 0
  IF n_elements(crdecimate) NE 2 THEN crdecimate = [1,1]
  IF n_elements(excludecols) EQ 0 THEN excludecols = ""
  rflag =  keyword_set(rflag)



    ;; Generate the grid

  nlon = (lonpar[1]-lonpar[0])/lonpar[2]+1
  nlat = (latpar[1]-latpar[0])/latpar[2]+1
  loni = (findgen(nlon)*lonpar[2]+lonpar[0]) # replicate(1.,nlat)
  lati = replicate(1.,nlon)#(findgen(nlat)*lonpar[2]+latpar[0])
  
  mask = loni*0


  landmask = runlandmask(loni, lati)
  water =  landmask EQ 0
  wateri = where(water,nwateri)
  IF nwateri EQ 0 THEN BEGIN 
    Message,'This grid has no water points! -- Exiting',/cont
    return,0
  ENDIF 

  
  FOR f=0,nn-1 DO BEGIN 
    file = wfiles[f]
    q = obj_new('q2b',file=file, decimate=decimate, $
                crdecimate=crdecimate, excludecols=excludecols,$
                rainflag=rflag, rf_action=0)
    s = q-> getplotdata(u,v,lon,lat)
    g=where(finite(u) AND finite(v),ng)
    unpack_where,u,g,c,r
    r=r[uniq(r,sort(r))]
    lon = lon[*,r]
    lat = lat[*,r]
    u = (v=0)

    obj_destroy,q

    
    gg = where(abs(lon) GE 1.e-7 AND abs(lat) GE 1.e-7, ngg)
    IF ngg NE 0 THEN BEGIN 
      lon = lon[gg]
      lat = lat[gg]

      gg = where( lon GE lonpar[0] AND lon LE lonpar[1] AND $
                  lat GE latpar[0] AND lat LE latpar[1],ngg)
      IF ngg NE 0 THEN BEGIN 
        lon =  lon[gg]
        lat =  lat[gg]

        ilon = round(lon*lonpar[2]-lonpar[0])
        ilat = round(lat*latpar[2]-latpar[0])

        mask[ilon,ilat] = 1
      ENDIF 
    ENDIF 

  ENDFOR 

  mask2 = mask
  bad = where(mask EQ 0 AND water,nbad)
  good = where( mask EQ 1 AND water,ngood)
  reallydone = 0
  IF nbad NE 0 THEN BEGIN 
    w = 0l
    WHILE w LT nbad AND NOT done DO BEGIN 

      dist = sphdist( lati[wateri],loni[wateri], lati[bad[w]],loni[bad[w]])
      gg = where( (dist LE tolerance) AND (dist GT 1.e-7) AND mask[wateri], ngg)
      done =  ngg EQ 0
      IF doall THEN BEGIN 
        IF done THEN BEGIN 
          reallydone = 1
          done = 0
        ENDIF ELSE mask2[bad[w]] =  1
      ENDIF 
      w = w+1
    ENDWHILE 
  ENDIF 

  IF doall THEN BEGIN 
    done = reallydone
    mask = mask2
  ENDIF 
  return,done
END
