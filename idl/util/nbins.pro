;
; $Log$
;
; $Id$
;
;
;
FUNCTION nbins, lon, lat, xx, $
            minlon=minlon, maxlon=maxlon,loninc=loninc, $
            minlat=minlat,maxlat=maxlat, latinc=latinc,$
            init=init, help=help

  COMMON nbins_cmn, mnlon,mxlon,lninc,mnlat,mxlat,ltinc, helpmsg
  
  nn = -1
  IF keyword_set( init ) THEN BEGIN 
    IF NOT keyword_set( minlon ) THEN mnlon = 0 ELSE $ 
      mnlon = minlon
    IF NOT keyword_set( maxlon ) THEN mxlon =  359 ELSE $ 
      mxlon = maxlon
    IF NOT keyword_set( loninc ) THEN lninc =  1 ELSE $ 
      lninc = loninc
    IF NOT keyword_set( minlat ) THEN mnlat = -90 ELSE $ 
      mnlat = minlat
    IF NOT keyword_set( maxlat ) THEN mxlat =  90 ELSE $ 
      mxlat = maxlat
    IF NOT keyword_set( latinc ) THEN ltinc =  1  ELSE $ 
      ltinc = latinc
    return,1
  ENDIF 
  
  IF n_elements( helpmsg ) EQ 0 THEN BEGIN 
    lf =  string(byte(10))

    helpmsg =  $
     'Usage: nn=nbins(lon,lat[,wherevec,minlon=minlon,maxlon=maxlon,...)' + lf
    helpmsg =  helpmsg + $
     ' You may initialize the lat/lon grid quanties min/max/inc' + lf
    helpmsg =  helpmsg + $
     ' By calling once with the all min/max/inc keywords set as well as ' + lf
    helpmsg =  helpmsg + $
     "  the 'init' keyword. Thereafter, these same values " + lf
    helpmsg =  helpmsg + '  will be used in the calculations'
  ENDIF 
  IF keyword_set( help ) THEN BEGIN 
    print,helpmsg
    return,-1
  ENDIF 

  IF keyword_set( minlon ) THEN  mnlon2 = minlon ELSE $
   IF n_elements(mnlon) NE 0 THEN mnlon2  = mnlon
  IF keyword_set( maxlon ) THEN  mxlon2 = maxlon ELSE $
   IF n_elements(mxlon) NE 0 THEN mxlon2  = mxlon
  IF keyword_set( loninc ) THEN  lninc2 = loninc ELSE $
   IF n_elements(lninc) NE 0 THEN lninc2 = lninc
  IF keyword_set( minlat ) THEN  mnlat2 = minlat ELSE $
   IF n_elements(mnlat) NE 0 THEN mnlat2  = mnlat
  IF keyword_set( maxlat ) THEN  mxlat2 = maxlat ELSE $
   IF n_elements(mxlat) NE 0 THEN mxlat2  = mxlat
  IF keyword_set( latinc ) THEN  ltinc2 = latinc ELSE $
   IF n_elements(ltinc) NE 0 THEN ltinc2 = ltinc
 
  test =  n_elements( mnlon2 ) * n_elements(mxlon2) * n_elements(lninc2)
  test =  test*n_elements( mnlat2 ) * n_elements(mxlat2) * n_elements(ltinc2)
  IF test NE 0 THEN BEGIN 

    nlon = (mxlon2 - mnlon2)/lninc2 + 1
    nlat = (mxlat2 - mnlat2)/ltinc2 + 1

    IF n_elements(xx) EQ 0 THEN xx =  lindgen(n_elements(lon))
    loni = (fix((lon(xx)+lninc2/2.)/lninc2) -mnlon2 ) MOD nlon 
    s = sign(lat(xx))
    lati =  fix(s*(abs(lat(xx))+ltinc2/2)) -mnlat2
    h     = hist_2d( loni, lati,min1=0, max1=nlon-1,$
                     min2=0,max2=nlat-1)
    x1    = where( h, nn )

  ENDIF ELSE BEGIN 
    MESSAGE,'Must run nn=nbins(...,/init) to initialize ' + $
   ' or specify min/max/inc quantities via keywords',/cont
    print,nbins(/help)          ;
  ENDELSE 

return,nn
END

