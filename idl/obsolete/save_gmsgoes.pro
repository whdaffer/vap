  PRO SAVE_GMSGOES,GMSTIME,maxLat,minLat, minlon, maxlon, resdeg
;
  common combdat,  bytimage
  common limits, blatmin, blatmax, blonmin, blonmax
  common colortab, red,green,blue
  COMMON colors, r_curr, g_curr, b_curr, r_orig, g_orig, b_orig

;
  print, maxLat,  minLat
;
; Use a Z buffer to produce the .srf file. Z buffers are not constrained by 
;   color map limitations of the interactive X server, i.e. can use 256 colors
;
;  set_plot,'Z'
; set_plot,'x'
  
  set_plot,'ps'
  ; for Typh violet
  ;device,/color,bits=8,/land,file='/disk2/vap/gms-5/violet-hit.ps'
  ;device,/color,bits=8,/land,file='/disk3/gms/dale-hit.ps'
  device,/color,bits=8,/land,$
   xsiz=5.84615, /inches, file='/disk3/linwood/nscatbig.ps'
;   xsiz=5.84615, /inches, file='/disk3/linwood/nscatsmall.ps'
;    xsiz=5.84615, /inches, file='/disk3/linwood/sara.ps'
  ps =  (!d.name EQ 'PS')

  red =  bindgen(256)
  green = red
  blue = red

  tvlct,red,green,blue
;  print, 'reds:   ', red
;  print, 'greens: ', green
;  print, 'blues:  ', blue
  help,/device
;
;
; Limits of bytimage
;
  slatmin = minLat &   slatmax = maxLat
  slonmin = minlon   &   slonmax = maxlon
;
  print, 'Lat/lon limits of bytimage: ', slatmin, slatmax, slonmin, slonmax
;
; images start at the top
;
  ;
  ml = fix(1./resdeg)

  latstart  = (blatmax-slatmax)*ml    &  lonstart  = (slonmin-blonmin)*ml
  latlen    = (slatmax-slatmin)*ml+1  &  lonlen    = (slonmax-slonmin)*ml+1
  latend    =  latstart + latlen -1      &  lonend    =  lonstart + lonlen -1
;
; extract a small image area to plot
;  bytimage    =  bytimage(lonstart:lonend,latstart:latend)
  print, 'Pixel limits of bytimage: ', lonstart,lonend, latstart,latend
  latcent   = 0.
  loncent   = (slonmax+slonmin)*0.5
; 
; set to 0 for left justification
;
  latalign  = 0.0
  !order=0
;
; window size for 40S to 40N and 130E to 110W 
;

  ; read the land elevation file if it isn't in the common
  openr,1,'$VAP_ROOT/animate/land_elevations.bin'
  landel =  intarr( 12l*360, 12l*180 + 1 )
  readu,1, landel
  close,1

  BACKGROUND  = 0b
  WATER_START = 1b
  LAND_START  = 13b
  CLOUD_START = 33b
  WIND_START  = 73b
  WHITE       = 98b
  N_WIND_COLORS =  WHITE-WIND_START


  spawn,' wc -l $VAP_ROOT/goes-vis-overlay-ct.txt', nlines
  nlines =  nlines(0)

  openr,rlun,'$VAP_ROOT/goes-vis-overlay-ct.txt',/get,error=err
  IF err NE 0 THEN BEGIN 
    message,!err_string,/cont
    return
  ENDIF 
  tmp =  bytarr(3,nlines)
  top_color_index =  nlines-1
  readf,rlun, tmp
  free_lun,rlun
  tmp =  transpose( tmp )
  red   =  tmp( *,0)
  green =  tmp(*,1)
  blue  =  tmp(*,2) &  tmp=0

  red(13) =  25
  green(13) = 110
  blue(13) = 0

  r_curr = red 
  g_curr = green
  b_curr = blue
  r_orig = red
  g_orig = green
  b_orig = blue

  tvlct,red,green,blue

  cbar =  bytarr( 10, 10, n_wind_colors ) 
  FOR i=0,n_wind_colors-1 DO cbar(*,*,i) =  byte(i) + wind_start
  cbar =  transpose( reform( cbar, 10, 10*n_wind_colors ) )

  minpix =  60;
  ;IF NOT ps THEN device,set_resolution=[640,480]
  IF NOT ps THEN device,set_resolution=[1280,1024]
  bytimage =  256-bytimage 
  nlon =   n_elements( bytimage(*,0) )
  nlat =   n_elements( bytimage(0,*) )
  tmp =  bytimage 
  bytimage( *,reverse( indgen(nlat) ) ) =  bytimage( *,indgen(nlat) )
  print,'minmax(byteimage)= ',minmax(bytimage)
  print,' '
  ;  IF NOT ps THEN window,xsize=640,ysiz=480,colors=n_elements(red)
 smallpix = where( bytimage LE minpix, nsmallpix )
;  smallpix = lindgen(n_elements(bytimage) ) &  nsmallpix=n_elements(bytimage)
  largepix =  where( bytimage GT minpix, nlargepix )
  unpack_where, bytimage, smallpix, col, row
  bytimage(largepix) =  $
   (bytscl( bytimage(largepix), min=minpix, top=39 ) + cloud_start ) <  $
   (wind_start-1)
  largepix =  0l ; free memory

  dlon =  (findgen( nlon )*resdeg + minlon) # (fltarr(nlat)+1)
  dlat =  (fltarr(nlon)+1) # (findgen(nlat)*resdeg + minlat )
  lons =  dlon( smallpix )
  lats =  dlat( smallpix )
  mask =  lonarr( nsmallpix )
  t7 =  systime(1)
  xx =  where( lons LT 0, nxx )
  IF nxx NE 0 THEN lons(xx) =  lons(xx)+360.


  print,' Going into land_mask '
  t =  systime(1)
  LAND_MASK, lons, lats, mask
  print,' Time to do land_mask = ', $
   systime(1)-t, ' seconds for ', nsmallpix, ' pixels '
  ;lon =  0
  ;lat =  0

  sea =  where( mask EQ 0, nsea )
  IF nsea NE 0 THEN  BEGIN 
    bytimage( smallpix( sea ) ) =  $
        bytscl( bytimage( smallpix( sea ) ), $
             top= 10 )   + water_start
;    bytimage( smallpix( sea )) =     water_start
    bytimage( smallpix( sea )) =   $
     water_start >  bytimage( smallpix( sea )) <  (land_start-1)
  ENDIF 

  print,' Going into land section '
  t1 =  systime(1)
  land =  where( mask EQ 1, nland )
  
  IF nland NE 0 THEN BEGIN 
;    ix =  lons( land )*12.
;    iy =  (lats( land )+90.)*12.
;    bytimage( col(land), row(land) ) =  landel( ix, iy ) + LAND_START
;    bytimage( smallpix( land ) ) =    land_start >  bytimage( smallpix( land ) ) <  (cloud_start-1)
    bytimage( smallpix( land ) ) = 13
  ENDIF 
  smallpix = 0l
  t2 =  systime(1)
  print,' land section took ', (t2-t1)/60., ' minutes '

  sea =  0l &  land= 0l &  smallpix= 0l
;  xx =  where( bytimage EQ 0, nxx )
;  IF nxx NE 0 THEN BEGIN
;    IF keyword_set( wcolor ) THEN $
;     bytimage( xx ) =  wcolor ELSE $
;     bytimage( xx ) =  water_start+2
;  ENDIF 

  t3 =  systime(1)
  print,' sea section took ', (t3-t2)/60., ' minutes '

;
  map_set,latcent,loncent,         $
     limit=[slatmin,slonmin,slatmax,slonmax],xmargin=[0,0],ymargin=[0,2],    $
     /noborder
     ; /isotropic
  newa = map_image(bytimage,xs,ys,xsiz, ysiz, $
                   latmin=slatmin,latmax=slatmax,      $
                   lonmin=slonmin,lonmax=slonmax, /compress)
  bytimage = 0l ; free memory

  u = 0
  v = 0
  lon = 0
  lat = 0
  ; Used for Typhoon Violet
  ; restore,'/disk2/vap/gms-5/violet-hit.save'
  ;restore,'/disk3/gms/dale-hit.save'
  ; used for linwood jones' stuff
   restore,'/disk3/linwood/nscatbig.save'
  ; restore,'/disk3/linwood/nscatsmall.save'
  ;restore,'/disk3/linwood/sara.save'
  s =  size( newa  ) &  ny= s(2)
  IF ps THEN BEGIN
    scalef = 0.02
   newa(*,ny-40:ny-1) =  top_color_index ; ps looks best in white
   TV, newa, xs,ys, XSIZ=xsiz, YSIZ=ysiz 
  ENDIF ELSE BEGIN 
    scalef = 1.
   newa(*,ny-40:ny-1) =  0 ; gif/x looks best in black
   TV, newa, xs,ys
  ENDELSE 

   xy =  convert_coord( [ xs + xsiz/2, (ny-40)/scalef ] , /dev, /to_data )
   y =  xy(1)

  xx =  where( lat LT y, nxx )
  IF nxx NE 0 THEN $
   PLOTVECT, u(xx),v(xx),lon(xx),lat(xx),$
;   length      = 0.5,$
   length      = 1,$
   start_index = wind_start, $
   ncolors     = n_wind_colors, $
   minspeed    = 2 ,$
   maxspeed    = 20 ,$
   thick       = 2

  ; This title used for Typhoon Violet
  ; title_str =  'Typhoon Violet 09/20/96: GMS-5 IR1: 2332, NSCAT Data: 09/21/210235 '

  ; This one used for Typhoon Dale.
  ; title_str =  ' Typhoon Dale 11/11/96: GMS-5 IR1: 1232 , NSCAT Data: 1400 '

  ; This title used for Lynwood Jones' stuff
  title_str =  'Typhoon Violet: GMS-5 IR1: 09/19/2332, NSCAT:  09/20/0120 '
  ; title_str =  'Typhoon Violet: nscatsmall.txt'
  ; title_str =  'Typhoon Violet: sara.txt'

  ; !p.color=7
  IF NOT ps THEN BEGIN
    xsiz =  !d.x_size
    ysiz =  !d.y_size
  ENDIF 
  cbxfac =  0.5
  cbyfac =  0.75
  IF ps THEN $
  cbloc = convert_coord( [(xsiz-cbxfac*(10*n_wind_colors)/scalef)/2,$
                          ysiz-32/scalef], /device,/to_normal ) ELSE $
  cbloc = convert_coord( [(xsiz-cbxfac*(10*n_wind_colors)/scalef)/2,$
                          ysiz-45/scalef], /device,/to_normal )
  ; color bar x location 
  cbx =  cbloc(0)
  ; color bar y location
  cby =  cbloc(1)

  ; color bar title
  IF ps THEN $
   cbtitloc = convert_coord( [xsiz/2,ysiz-22/scalef], $
                             /device,/to_normal ) ELSE $
   cbtitloc = convert_coord( [xsiz/2,ysiz-32/scalef], $
                             /device,/to_normal ) 

  cbtitx =  cbtitloc(0) &  cbtity= cbtitloc(1)
  ; end of color bar
  cbend =  convert_coord( [(xsiz-cbxfac*(10*n_wind_colors)/scalef)/2 + $
                           cbxfac*(10*n_wind_colors + 2)/scalef, $
                             ysiz-45/scalef], /dev, /to_norm )
  cbendx =  cbend(0)
  ; title location
  titloc =  convert_coord( [xsiz/2,ysiz - !d.y_ch_size-2],  $
                           /device, /to_normal )   
  titx =  titloc(0) &  tity =  titloc(1)
  IF ps THEN text_color = 0 ELSE text_color = 255

  ; Annotate the image

  ; Put down title string
  xyouts, titx, tity, title_str, align=0.5, /normal, charsize=1.05, $
   color=text_color

  ; Put down color bar
  IF ps THEN y =  ysiz-35/scalef ELSE y =  ysiz-45/scalef
  tv, cbar, cbx*xsiz, y,xsiz=cbxfac*10*n_wind_colors/scalef,ysiz=cbyfac*10/scalef
  ; Annotate the color bar
  cbtitle = ' Winds (M/S)'
  cblables = [' 2','>20']
  xyouts, cbtitx, cbtity, cbtitle,    align = 0.5, /normal, color=text_color
  xyouts, cbx-0.01, cby, cblables(0), align = 1.0, /normal, color=text_color
  xyouts, cbendx,   cby, cblables(1), align = 0.0, /normal, color=text_color
  xyouts, 0.5, (cbtity+tity)/2, $
   '(GMS-5 data courtesy of the University of Hawaii)' , align=0.5, $
   /normal, color=text_color, charsiz=0.75 
   
 ; IF ps THEN $
 ;  xyouts, !x.window(0)+.05, (cbtity + tity)/2., $
 ;  'of University of Hawaii ',align=0, $
 ;  /normal, color=text_color, charsize=0.75 ELSE $
 ;  xyouts, !x.window(0), cby, $
 ;  'of University of Hawaii ',align=0, $
 ;  /normal, color=text_color, charsize=0.75


;  map_set,latcent,loncent,         $
;    limit=[slatmin,slonmin,slatmax,slonmax],xmargin=[0,0],ymargin=[0,2],    $
;    /noerase,title=plot_title, /noborder
;
  help,T
  help,newa
  help,bytimage

  IF NOT ps THEN begin
    T      = tvrd()
   ; giffile = 'dale-hit.gif'
   giffile = 'violet-hit.1280x1024.gif'
    print,' writing file ', giffile
    write_gif, giffile, t, red,green,blue
  ENDIF 
;  srfdir = '/s/cronman/gms-goes/gms-ir1/data/'

;

;  srfile = GMSTIME +  '.srf'
;  srfile = srfdir + srfile
;  print,'Writing srf file to ', srfile
;  write_srf, srfile, T,red,green,blue
   
;  navfile = GMSTIME +  '.nav'
;  navfile = srfdir + navfile
;  openw,nlun,navfile,/get_lun
;  printf,nlun,slonmin,67,(slonmax-slonmin)/690.
;  printf,nlun,slatmin,5,(slatmax-slatmin)/588.
;  device,/close
   IF ps THEN device,/close

  return
  END



