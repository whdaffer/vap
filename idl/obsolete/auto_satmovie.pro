;+
; NAME:  Auto_satmovie
; $Id$
;
;  PURPOSE: Make Nscat/Qscat/Seawinds animations using the satellite
;           projection
;
; AUTHOR:  William
;
; CATEGORY:  Nscat/Qscat/Seawinds animations
;
; CALLING SEQUENCE: auto_satmovie[, time, time_inc,
;                   roi=roi,wpath=wpath,vlonpar=vlonpar,vlatpar=vlatpar,
;                   limit=limit,interp_path=interp_path,anim_path=anim_path,
;                   min_nvect=min_nvect,dateit=dateit
; 
; INPUTS:  None are required
;
; OPTIONAL INPUTS:  
;          time - the end time of the animation (default=current time)
;          time_inc - time-time_inc = timerange of animation (default=26
;          hours
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
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.2  2001/12/08 00:02:36  vapdev
; Getting rid of obsolete RSI routines and fixing ENV vars
;
; Revision 1.1.1.1  2001/12/04 19:14:14  vapuser
; Imported sources
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO AUTO_SATMOVIE, date_time, time_inc, roi=roi, $
              wpath = wpath, $  ; path to wind files (def=$VAP_WINDS)
              vlonpar= vlonpar ,$
              vlatpar= vlatpar,$
              limit= limit,$
              interp_path = interp_path , $ ; path to output interp file 
                                            ; (def=$VAP_ANIM)
              anim_path= anim_path,$        ; path for output animation files 
                                            ; (def=$VAP_ANIM/daily)
              min_nvect =  min_nvect ,$     ; minimum number of vectors needed in ROI 
                                            ; to make movie.
              dateit =  dateit 




COMMON prs, long_sel, lats_sel, lons, lats, uu, vv, uu_sel, vv_sel, $
           ileft, iright, itop, ibot, dist_left, dist_right, dist_top, $
           dist_bot, dist, weights, invdist, xfinc, xf0, yfinc, yf0, eps



user = getenv('USER')
cur_dir =  getenv('PWD')

dateit =  keyword_set( dateit )

cronjob = 0 ; flag for cronjob runs.
IF (user NE "" ) THEN BEGIN 
  lockfile = (findfile('/tmp/' + user + '.auto_movie.lock', count=n))(0)
  IF n EQ 1 THEN cronjob = 1
ENDIF ELSE cronjob = 0

IF cronjob THEN BEGIN 
  openw, llun, lockfile, /get, error= err
  IF err NE 0 THEN BEGIN 
    message,!err_string,/cont
    return
 ENDIF 
ENDIF 
IF N_elements( time_inc ) EQ 0 THEN time_inc =  26


IF n_elements( roi ) EQ 0 THEN roi =  'NPAC'
IF n_elements( animpar ) EQ 0 THEN animpar =  [480,360,60]


  ; Get the defaults for this ROI
IF n_elements( roi ) EQ 0 THEN   roi =  'NPAC'
roi =  strupcase(roi)

roistr =  READ_SATMOVIE_DEFS( roi )
IF strpos( roistr.desig, 'ERROR' ) NE -1 THEN BEGIN 
  MESSAGE,"Error reading defaults for roi " + roi + $
    " error = " + roistr.desig,/cont
  return
ENDIF 

MESSAGE,' Looking for roi ' + roi,/cont

IF n_elements( wpath ) ne 0 THEN roistr.wpath =  wpath
IF n_elements( vlonpar ) NE 0 THEN roistr.vlonpar =  vlonpar
IF n_elements( vlatpar ) NE 0 THEN roistr.vlatpar =  vlatpar 
IF n_elements( limit ) NE 0 THEN roistr.limit =  limit
IF n_elements( interp_path ) NE 0 THEN roistr.interp_path =  interp_path
IF n_elements( anim_path ) NE 0 THEN roistr.anim_path =  anim_path
IF n_elements( min_nvect ) NE 0 THEN roistr.min_nvect =  min_nvect

vlonpar     = roistr.vlonpar       
vlatpar     = roistr.vlatpar       
limit       =  roistr.limit
loncent     =  roistr.loncent
latcent     = roistr.latcent
interp_path = roistr.interp_path   
anim_path   = roistr.anim_path     
wpath       = roistr.wpath
min_vect   = roistr.min_vect

IF n_elements( date_time ) EQ 0 THEN BEGIN
 ; Get the current GMT doy and hour
  spawn,'date -u +%j/%Y/%m/%d/%H',ret
  date_time =  ret(0)
  tmp =   strsplit(  ret(0), '/',/extract) 
  actual_doy = tmp(0)
  test_year  = tmp(1)
  test_month = tmp(2)
  test_dom   = tmp(3)
  test_hour  = tmp(4)
  test_doy =  actual_doy
  date_string =  strmid( test_year, 2,2 ) + '/' + test_month + '/' + test_dom + '/' + test_hour
  IF test_year GT 1996 THEN test_doy =  actual_doy + (test_year-1996)*365 + 1
  test_day =  test_doy + fix(test_hour)/24.
ENDIF ELSE BEGIN
  ; parse the users input date/time.
  tmp =   strsplit(  date_time, '/' ,/extract) 
  date_string =  date_time
  test_year =  fix(tmp(0))
  test_month = tmp(1)
  test_dom =  tmp(2)
  test_hour =  tmp(3)
  IF test_year LT 95 THEN $
   test_year =  2000 + test_year ELSE $
   test_year =  1900 + test_year
  test_date_str =  test_dom+'-'+test_month+'-'+strtrim(test_year,2)
  actual_doy = datestr2doystr( test_date_str )
  actual_doy =  strmid( actual_doy, 4, 3)
  
  IF test_year GT 1996 THEN test_doy =  actual_doy + (test_year-1996)*365 + 1
  test_day =  test_doy + fix(test_hour)/24.
ENDELSE 

anim_date_str =  test_year + test_month + test_dom + test_hour 


wf =  findfile( wpath + '/N*', count=nf)
IF nf EQ 0 THEN BEGIN
  wp =  getenv('VAP_WINDS')
  str =  'ERROR: No wind files in dir ' + wp
  message,str,/cont
  IF cronjob THEN BEGIN 
    printf,llun,str
    free_lun,llun
  ENDIF 
  RETURN
ENDIF ELSE BEGIN
  
  ; the file names  have this form.
  ;      0123456789012345678
  ;/path/N961118.S1725.E1859
  
  ; find the end of the path
  l =  strlen( wf(0) )
  t =  reverse( byte(wf(0)) )
  x = where( t EQ (byte('/'))(0), nx )
  IF nx NE 0 THEN name_off =  l-x(0) ELSE name_off = 0
  
  year       = strmid( wf, name_off+1, 2 )
  month      = strmid( wf, name_off+3, 2 )
  dom        = strmid( wf, name_off+5, 2 )
  start_hour = strmid( wf, name_off+9, 2 )
  start_min  = strmid( wf, name_off+11,2 )
  end_hour   = strmid( wf, name_off+15,2 )
  end_min    = strmid( wf, name_off+17,2 )
  
  ; convert day-of-month/month/year to doy
  x = where( fix(year) LT 96, nx )
  y = where( fix(year) GE 96, ny )
  IF nx EQ 0 THEN year =  '19' + year ELSE BEGIN
    year(y) =  '19' + year(y)
    year(x) =  '20' + year(x)
  ENDELSE 
  x = 0 &  y=0
  date_str =  dom + '-' + month + '-' + year
  doy = intarr(nf)
  FOR i=0,nf-1 DO BEGIN
    tmp =  DateStr2DoyStr(date_str(i))
    doy(i) =  fix( strmid( tmp, 4,3))
 ENDFOR 

  ; Convert to fractional days since 1-jan-1996
  ; (NB. 1996 is leap year, but 2000 isn't)
  filedays =  [ [(fix(year)-1996)*365 + doy + fix(start_hour)/24. + fix(start_min)/(24.*60.)], $
                [(fix(year)-1996)*365 + doy + fix(end_hour)/24.   + fix(end_min)/(24.*60.)] ]
  x = where( year GT 1996,nx )
  IF nx NE 0 THEN filedays(x,*) =  filedays(x,*)+1

    ; The following make the assumption that the only case where the
    ; end time will be < the start time is when a pass runs over the
    ; day boundary (eg, a pass starts at 23 and goes to 00:40) Back in
    ; Feb. we were seeing passes that started at 15:00 (say) and went
    ; until 0800. In these cases, it was the start time that was bogus,
    ; there were a few records at the start of the file that were
    ; from the previous day of data. We're hoping that those don't
    ; come back, if they do, we'll have to make the following code
    ; smarter. 


  x = where( float(end_hour) LT float(start_hour),nx )
  IF nx NE 0 THEN filedays(x,1) =  filedays(x,1)+1

  
    ; Find the file whose start time is before the end of our time
    ; range and and then find the file whose end time is
    ; after the . Failing that, take the file whose end time is closest to
    ; the end of our time range. Similarly, find the file whose start
    ; time preceeds our time range but whose stop time is inside of
    ; it. IF there is none such, find the file whose start time is
    ; closest to the start of the range but still greater than it.
    ; files whose stop time is > the start of our time range
  x1 =  min( where( filedays(*,1) GT test_day-time_inc/24., nx1 ) )
    ; files whose start time is < the end of our time range  
  x2 =  max( where( filedays(*,0) LT test_day, nx2 ) )
  IF nx1 EQ 0 OR nx2 EQ 0 THEN nx =  0 ELSE nx =  x2-x1+1
  IF nx EQ 0 THEN BEGIN 
    str =  'ERROR: No windfiles in input time range '
    message, str,/cont
    IF cronjob THEN BEGIN 
      printf,llun,str
    ENDIF 
    return
  ENDIF 

  x =  indgen(nx)+x1
  wf =  wf(x)
  nf =  n_elements(wf)
  filedays =  filedays(x,*)
  start_hour =  start_hour(x)
  end_hour =  end_hour(x)
  start_min =  start_min(x)
  end_min =  end_min(x)
     ; Now find out how much time is actually covered by this data.

  diff =  filedays(*,1)-filedays(*,0)
  total_time =  total( diff  )*24.
  diff =  transpose(diff)


  
  message,'Using time_inc = ' + string( time_inc, form='(i2)'),/cont
  message,'Wind Path = ' + wpath,/cont
  message,'Interp_path = ' + interp_path,/cont
  message,'anim_path = '+anim_path,/cont
  message,'date_time = ' + date_string,/cont
  tt =  transpose(wf)
  message,'Using the following files in the interpolation',/cont
  print,tt


  lonpar =  fltarr(3) &  latpar=lonpar
  lonpar(0:1) =  [ min( limit([1,3,5,7]),max=mx), mx ]
  latpar(0:1) =  [ min( limit([0,2,4,6]),max=mx), mx ]
  lonpar(2) =  1.
  latpar(2) =  1.

   ; Here we read the files and make the interpolated field.

  
  message,' Found ' + string(nf,form='(i3)') + ' files ',/cont

   READ_RMGDR_DATA,wf(0),uu,vv,llon,llat,mint=mmint,maxt=mmaxt
   FOR i=1,nf-1 DO BEGIN
     read_rmgdr_data,wf(i),u,v,lon,lat,mint=mint,maxt=maxt
     uu =  [uu,u]
     vv =  [vv,v]
     llon = [llon,lon]
     llat = [llat,lat]
     mmint = [mmint, mint]
     mmaxt =  [mmaxt,maxt]
  ENDFOR 

    ; convert lonpar to east longitude
  t =  where( lonpar LT 0, nt )
  IF nt NE 0 THEN lonpar(t) =  lonpar(t) + 360.
  x =  where( llon GT lonpar(0)-10 AND llon LE lonpar(1)+10 AND $
              llat GT latpar(0)-10 AND llat LE latpar(1)+10, nx )


  IF nx GT min_vect THEN BEGIN  

    
    genv,/save ; save graphics environment.
    CD,anim_path
    set_plot,'z'

    ; need 480x368 because if we're going to use mpeg, each dim must be
    ; divisible by 16.
    ;device,set_resolution=[480,368]

    device,set_resolution=animpar(0:1) ; for xinteranimate
    READ_VAP_ANIM_CT,r,g,b
    tvlct,r,g,b
    n_colors = n_elements(r)


    eps =  0.000102
    path_inc =  0.04 

    rainf = [12., 10., 6,   4 ] 
    ermax = [50., 20., 10., 5.]
    ui =  fltarr(360,121) &  vi=ui
    SUCCOR, llon,llat,uu,vv,ui,vi,lonpar,latpar,ermax,rainf
    rainf = [10.,6.,3.,2]
    ermax = [10.,6.,3.,2]
    SUCCOR, llon,llat,uu,vv,ui,vi,lonpar,latpar,ermax,rainf


    uu =  ui &  vv= vi ; need uu/vv so defined for CALCWINDFIELD (though common)

    interp_filename =  anim_date_str  + '_interp.bin'
    ofile =  interp_path + '/' + interp_filename
    openw,wlun, ofile, /get_lun, error=err
    IF err NE 0 THEN message, !err_string
    writeu,wlun,ui,vi
    free_lun,wlun
    loni =  findgen(360)#replicate(1,121)
    lati =  replicate(1,360)#(findgen(121)-60)
    ss = 1>sqrt( ui^2+vi^2)<30  
    CONTOUR,ss,loni,lati,levels=findgen(30),c_colors=bindgen(30),/cell_fill,$
     xstyle=4,ystyle=4 
    im=tvrd() & x=where(im) & unpack_where,im,x,cc1,rr1    
    cc = minmax(cc1)
    rr = minmax(rr1)
    im = im(cc(0):cc(1),rr(0):rr(1))
    sz = size( im) 
    nx =  sz(1) &  ny= sz(2)
    oo = convert_coord( cc1,rr1,/dev,/to_data) 
    lons1 =  reform(oo(0,*),nx,ny)
    lats1 =  reform(oo(1,*),nx,ny)
    oo = 0
    lons1 =  lons1(*,0) &  lats1=reform(lats1(0,*) )

    ;MAP_SET,20,-45,sat_p=[20,0,0],limit=[0,-125,60,-45,0,35,-60,-45],/noborder,/satellit

    ; gets west coast pretty well
    MAP_SET,latcent,loncent,sat_p=[20,0,0],/noborder,/satellite ,$
     limit=limit

    mapim2=map_patch(im,lons1,lats1,xsiz=xsz,ysiz=ysz,xstart=xs,ystart=ys)          
    tv,mapim2,xs,ys
    wim =  tvrd()
    x =  where( wim ) &  unpack_where, wim, x, cc, rr 

    oo = convert_coord( cc,rr,/dev,/to_data) 
    yy = where( oo(0,*) lt 360. and oo(1,*) lt 360.,nyy) 
    cc=cc(yy) & rr=rr(yy) & oo=oo(0:1,yy)  

    tlon=reform(oo(0,*)) & tlat=reform(oo(1,*))&tmask=long(tlon*0) 
    land_mask,tlon,tlat,tmask 
    land=where(tmask eq 1) 
    t = where( tlon lt 0., nt ) 
    IF nt GT 0 THEN tlon(t) = tlon(t)+360. 
    wim2=wim 
    ix = tlon(land)*12.
    iy = (tlat(land)+90)*12. 


    openr,1,'$VAP_LIBRARY/land_elevations.bin'
    landel =  intarr( 12*360, 12*180 + 1 ) 
    readu,1, landel 
    close,1 

    wim3 =  wim
    wim(cc(land),rr(land)) = 0b
    wim2(cc(land),rr(land)) = (landel(ix,iy)+31) < 50   
    water = where( tmask EQ 0, nw )
    IF nw NE 0 THEN BEGIN 
      wim2(cc(water),rr(water)) = 0
      wim(cc(water),rr(water)) = 255
    ENDIF 

    landel = 0

    erase

    ;  wim, continents in black, ocean white
    ;  wim2, continents in colors, oceans in black
    ;  wim3, filled contour plot of wind field
    ;  wim4, vectors on wim3


    ;
    ;
    ;
    ;This piece does the animation and interpolation stuff
    ;
    t =  where( vlonpar LT 0, nt )
    IF nt NE 0 THEN vlonpar(t) =  vlonpar(t) + 360.
    xv0 =  vlonpar(0) &  xv1= vlonpar(1) &  xvinc= vlonpar(2)
    yv0 =  vlatpar(0) &  yv1= vlatpar(1) &  yvinc= vlatpar(2)

    nxv =  fix( (xv1-xv0)/xvinc)
    nyv =  fix( (yv1-yv0)/yvinc)
    nframes = 60
    xfinc = 1. &  yfinc=1.
    xf0 = min(loni,max=xf1) & yf0=min(lati,max=yf1)  
    ; offset in the underlying wind field array of xv0 and yv0 
    xfoff =  (xv0 - xf0)/xfinc
    yfoff =  (yv0 - yf0)/yfinc

    nn =  nxv*nyv
    dist        = fltarr( nn,4 )   
    weights     = dist               

    long_sel1 = reform( (fltarr(nyv)+1)#((findgen(nxv)*xvinc) +xv0), nn ) 
    lats_sel1 = reform( (findgen(nyv)*yvinc+yv0) # (fltarr(nxv)+1)  ,nn ) 
    long_sel  = long_sel1
    lats_sel  = lats_sel1
    nframes =  60 ; number of frames in the animation.
    time_mov  = fix( (nframes-1)*randomu( seed,nn ) )
    ;

    ;
    real_start_time =  systime(1)
    tottime =  0.
    iter =  0l
    FOR i=0,nframes+59 DO BEGIN 
       print,'working on frame ',i
      CALCWINDFIELD
      IF i GE 60 THEN BEGIN 
        TV,wim3
        PLOTVECT, uu_sel, vv_sel, long_sel, lats_sel, $
           length=4, color=n_colors-1, thick=2
        wim4 = tvrd()
        tv,(wim4 AND wim) + wim2
        gifim =  tvrd()
        frm_str =  '000'
        num =  strtrim( i-59, 2 )
        strput, frm_str, num, 3-strlen(num)
        file = 'gwind.' + frm_str

        write_gif,file,gifim, r,g,b

      ENDIF 
      long_sel = long_sel + path_inc*uu_sel
      lats_sel = lats_sel + path_inc*vv_sel
      time_mov = time_mov+1


      x =  where( time_mov GE nframes, nx )
      IF nx NE 0 THEN BEGIN
        long_sel(x) =  long_sel1(x)
        lats_sel(x) =  lats_sel1(x)
        time_mov(x) =  0
      ENDIF 


    ENDFOR 

      ; make the movie
     omov_file =  'daily_' + strlowcase( roi ) 
     IF dateit THEN omov_file =  omov_file + '_' + anim_date_str
     omov_file =  omov_file + '.mov'
     ; construct string to send to spawn and execute it.
     exe_str =  'dmconvert -f qt -p video,comp=qt_cvid,squal=0.9,tqual=0.9,rate=15 ' + $
      ' -n gwind.0##,start=1,end=60,step=1 gwind.0## ' + omov_file
     message,' executing string: ' + exe_str
     spawn,exe_str,ret
     IF ret(0) NE '' THEN BEGIN
       str =  'ERROR: in dmconvert '
       message,str, /cont
       IF cronjob THEN begin
         printf, llun, str
         free_lun,llun
       ENDIF 
     ENDIF 

      ; reset to previous graphic environment
    GENV,/restore
    CD,cur_dir
  ENDIF ELSE BEGIN
    message,' Insufficient data! Only ' + strtrim( nx ,2 ) + ' vectors in ROI',/cont
  ENDELSE 
ENDELSE 
END
