PRO  read_l2_data, files , u,v,lon,lat, ambig = ambig,$
                   mintime = mintime,$
                   maxtime = maxtime
; read the specified l2 files and return the specified
; ambiguities of u,v and associated positions
; if ambig isn't set, take the wvc_sel ambigs.
;
; 1 <= ambig <= 4, take specified ambiguity
; ambig = -1 => take ambiguity specified in the data.wvc_sel variable,
; i.e. the 'selected' ambiguity.
; not( keyword_set( ambig ) ) => take wvc_sel ambiguity.
; William Daffer;
; 4-0161, daffer@rainy
;

COMMON nscatl2_cmn, nscatl2_size, nscatl2_defined 
NSCATL2_SIZE = 1456L 

IF keyword_set( ambig ) THEN amb =  ambig ELSE $
                             amb = 0

mintime =  '9999-999T99:99:99.999'
maxtime =  '0000-000T00:00:00.000'


nf =  n_elements( files )
first_read =  1;
FOR f=0,nf-1 DO BEGIN 
  openr,lun,files(f),/get, error=err
  r = 0

  IF err EQ 0 THEN BEGIN

    fs =  fstat(lun)
    nrecs =  fs.size/NSCATL2_SIZE
    r =  NSCATL2_STR( nrecs )
    readu,lun,r
    free_lun,lun

    mintime =  min( [ mintime, string(r.time)  ] )
    maxtime =  max( [ maxtime, string(r.time) ] )


    ; first 3 records are headers
    r =  r(3:n_elements(r)-1)
    nnew =  n_elements( r )
    newlat =  r.lat*0.01
    newspeed =  r.windspd*0.01


    ; dir and lon are unsinged integers, so take 
    ; care of the case where they are negative.
    newdir =  r.winddir*0.01
    b = where( newdir LT 0.,nb )
    IF nb NE 0 THEN newdir(b) =  655.36 + newdir(b)

    newlon =  r.lon*0.01
    b = where( newlon LT 0.,nb )
    IF nb NE 0 THEN newlon(b) =  655.36 + newlon(b)

    ; level 2 files are backwards from rmgdr and mgdr files
    nn =  n_elements(newdir(0,0,*))
    tmp =  fltarr( 4,24,nn)
    FOR i=0,nn-1 DO tmp( *,*,i ) =  transpose( newdir(*,*,i ) )
    newdir =  tmp
    tmp =  fltarr( 4,24,nn)
    FOR i=0,nn-1 DO tmp( *,*,i ) =  transpose( newspeed(*,*,i ) )
    newspeed =  tmp
    tmp =  0


    NSCAT_GETUV, newdir, newspeed, newu, newv 
    IF first_read THEN BEGIN
      IF amb GT 1 AND amb LE 4 THEN BEGIN 
        u       =  reform( newu( amb-1,*,*), 24l*nnew )
        v       =  reform( newv( amb-1,*,*), 24l*nnew )
        lon     =  reform( newlon, 24l*nnew )
        lat     =  reform( newlat, 24l*nnew )
      ENDIF ELSE BEGIN 
        u =  fltarr( nnew*24l )
        v =  u
        lon =  u
        lat =  u
        ii =  0
        FOR i=0,nnew-1 DO BEGIN 
          g =  where( r(i).nambig GT 0 AND r(i).wvc_sel GE  1, ng )
          IF ng NE 0 THEN BEGIN 
            sel =  r(i).wvc_sel(g)-1
            ix =  indgen(ng) + ng*indgen(ng)
            u(ii:ii+ng -1)   =  (newu(sel ,g,i ))(ix)
            v(ii:ii+ng -1)   =  (newv(sel,g,i ))(ix)
            lon(ii:ii+ng -1) =  newlon(g,i)
            lat(ii:ii+ng -1) =  newlat(g,i)
            ii =  ii+ng
          ENDIF 
        ENDFOR 
        u   =  u  (0:ii-1)
        v   =  v  (0:ii-1)
        lon =  lon(0:ii-1)
        lat =  lat(0:ii-1)
      ENDELSE 
      first_read   =  0l
    ENDIF ELSE BEGIN 
      IF amb GT 1 AND amb LE 4 THEN BEGIN 
        u       =  [u,  reform( newu( amb-1,*,*), 24l*nnew )]
        v       =  [v,  reform( newv( amb-1,*,*), 24l*nnew )]
        lon     =  [lon,reform( newlon, 24l*nnew )]
        lat     =  [lat,reform( newlat, 24l*nnew )]
      ENDIF ELSE BEGIN 
        tu =  fltarr( 24*nnew )
        tv =  tu
        tlon =  tu
        tlat =  tu
        ii =  0
        FOR i=0,nnew-1 DO BEGIN 
          g =  where( r(i).nambig GT 0 AND r(i).wvc_sel GE 1, ng )
          IF ng NE 0 THEN BEGIN 
            sel =  r(i).wvc_sel(g)-1
            ix =  indgen(ng) + ng*indgen(ng)
            tu(ii:ii+ng -1)   = ( newu( sel,g,i ))(ix)
            tv(ii:ii+ng -1)   = ( newv( sel,g,i ))(ix)
            tlon(ii:ii+ng -1) =  newlon(g,i)
            tlat(ii:ii+ng -1) =  newlat(g,i)
            ii =  ii+ng
          ENDIF 
        ENDFOR 
        tu   =  tu  (0:ii-1)
        tv   =  tv  (0:ii-1)
        tlon =  tlon(0:ii-1)
        tlat =  tlat(0:ii-1)
        u    =  [u,   tu   ]
        v    =  [v,   tv   ]
        lon  =  [lon, tlon ]
        lat  =  [lat, tlat ]

      ENDELSE 
    ENDELSE 
  ENDIF ELSE BEGIN
    message,!err_string,/cont
  ENDELSE 
ENDFOR ; end loop over files
RETURN
END
